{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}

module Language.Haskell.Brittany.Internal.Layouters.DataDecl
  ( layoutDataDecl
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Config.Types

import           RdrName ( RdrName(..) )
import           GHC ( Located, runGhc, GenLocated(L), moduleNameString )
import qualified GHC
import           HsSyn
import           Name
import           Language.Haskell.GHC.ExactPrint.Types ( mkAnnKey )

import           Language.Haskell.Brittany.Internal.Layouters.Type
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Stmt
import           Language.Haskell.Brittany.Internal.Layouters.Pattern
import           Language.Haskell.Brittany.Internal.Utils

import           Bag ( mapBagM )



layoutDataDecl
  :: Located (HsDecl RdrName)
  -> Located RdrName
  -> LHsQTyVars RdrName
  -> HsDataDefn RdrName
  -> ToBriDocM BriDocNumbered
layoutDataDecl ld name (HsQTvs _ bndrs _) defn = case defn of

  HsDataDefn NewType (L _ []) _ctype Nothing [cons] mDerivs -> case cons of
    (L _ (ConDeclH98 consName Nothing (Just (L _ [])) details _)) ->
      docWrapNode ld $ do
        nameStr     <- lrdrNameToTextAnn name
        consNameStr <- lrdrNameToTextAnn consName
        tyVarLine   <- fmap return $ createBndrDoc bndrs
        -- headDoc     <- fmap return $ docSeq
        --   [ appSep $ docLit (Text.pack "newtype")
        --   , appSep $ docLit nameStr
        --   , appSep tyVarLine
        --   ]
        rhsDoc      <- fmap return $ createDetailsDoc consNameStr details
        createDerivingPar mDerivs $ docSeq
          [ appSep $ docLit (Text.pack "newtype")
          , appSep $ docLit nameStr
          , appSep tyVarLine
          , docSeparator
          , docLit (Text.pack "=")
          , docSeparator
          , rhsDoc
          ]
    _ -> briDocByExact ld

  HsDataDefn DataType (L _ lhsContext) _ctype Nothing [] mDerivs ->
    docWrapNode ld $ do
      lhsContextDoc <- docSharedWrapper createContextDoc lhsContext
      nameStr       <- lrdrNameToTextAnn name
      tyVarLine     <- fmap return $ createBndrDoc bndrs
      createDerivingPar mDerivs $ docSeq
        [ appSep $ docLit (Text.pack "data")
        , lhsContextDoc
        , appSep $ docLit nameStr
        , appSep tyVarLine
        ]

  HsDataDefn DataType (L _ lhsContext) _ctype Nothing [cons] mDerivs ->
    case cons of
      (L _ (ConDeclH98 consName mForall mRhsContext details _)) ->
        docWrapNode ld $ do
          lhsContextDoc <- docSharedWrapper createContextDoc lhsContext
          nameStr       <- lrdrNameToTextAnn name
          consNameStr   <- lrdrNameToTextAnn consName
          tyVarLine     <- fmap return $ createBndrDoc bndrs
          forallDoc     <- docSharedWrapper createForallDoc mForall
          rhsContextDoc <- case mRhsContext of
            Nothing         -> return docEmpty
            Just (L _ ctxt) -> docSharedWrapper createContextDoc ctxt
          rhsDoc        <- fmap return $ createDetailsDoc consNameStr details
          createDerivingPar mDerivs $ docSeq
            [ appSep $ docLit (Text.pack "data")
            , lhsContextDoc
            , appSep $ docLit nameStr
            , appSep tyVarLine
            , docSeparator
            , docLit (Text.pack "=")
            , docSeparator
            , forallDoc
            , rhsContextDoc
            , rhsDoc
            ]
      _ -> briDocByExact ld

  _ -> briDocByExactNoComment ld

createContextDoc :: HsContext RdrName -> ToBriDocM BriDocNumbered
createContextDoc [] = docEmpty
createContextDoc [t] =
  docSeq [layoutType t, docSeparator, docLit (Text.pack "=>"), docSeparator]
createContextDoc ts = docSeq
  [ docLit (Text.pack "(")
  , docSeq $ List.intersperse docCommaSep (layoutType <$> ts)
  , docLit (Text.pack ") =>")
  , docSeparator
  ]

createBndrDoc :: [LHsTyVarBndr RdrName] -> ToBriDocM BriDocNumbered
createBndrDoc bs = do
  tyVarDocs <- bs `forM` \case
    (L _ (UserTyVar vname)) -> return $ (lrdrNameToText vname, Nothing)
    (L _ (KindedTyVar lrdrName kind)) -> do
      d <- docSharedWrapper layoutType kind
      return $ (lrdrNameToText lrdrName, Just $ d)
  docSeq
    $   List.intersperse docSeparator
    $   tyVarDocs
    <&> \(vname, mKind) -> case mKind of
          Nothing   -> docLit vname
          Just kind -> docSeq
            [ docLit (Text.pack "(")
            , docLit vname
            , docSeparator
            , docLit (Text.pack "::")
            , docSeparator
            , kind
            , docLit (Text.pack ")")
            ]

createDerivingPar
  :: HsDeriving RdrName
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
createDerivingPar mDerivs mainDoc = do
  case mDerivs of
    Nothing                 -> docLines [mainDoc]
    Just (L _ [(HsIB _ t)]) -> do
      docAlt
        [ docPar mainDoc $ docEnsureIndent BrIndentRegular $ docSeq
          [docDeriving, docSeparator, layoutType t]
        ]
    Just (L _ ts          ) -> do
      docAlt
        [ docPar mainDoc $ docEnsureIndent BrIndentRegular $ docSeq
          [ docDeriving
          , docSeparator
          , docLit $ Text.pack "("
          , docSeq $ List.intersperse docCommaSep $ ts <&> \(HsIB _ t) ->
            layoutType t
          , docLit $ Text.pack ")"
          ]
        ]

docDeriving :: ToBriDocM BriDocNumbered
docDeriving = docLit $ Text.pack "deriving"

createDetailsDoc
  :: Text -> HsConDeclDetails RdrName -> (ToBriDocM BriDocNumbered)
createDetailsDoc consNameStr details = case details of
  PrefixCon args -> docSeq
    [ docLit consNameStr
    , docSeparator
    , docSeq $ List.intersperse docSeparator $ args <&> layoutType
    ]
  RecCon (L _ []) -> docEmpty
  RecCon (L _ [L _ (ConDeclField names t _)]) -> docSeq
    [ docLit consNameStr
    , docSeparator
    , appSep $ docLit $ Text.pack "{"
    , createNamesAndTypeDoc names t
    , docSeparator
    , docLit $ Text.pack "}"
    ]
  RecCon (L _ (fstField:fields)) ->
    docAddBaseY BrIndentRegular $ docPar
      (docLit consNameStr)
      (docLines
        [ docSeq
          [ docLit $ Text.pack "{ "
          , let L _ (ConDeclField names t _) = fstField
              in createNamesAndTypeDoc names t
          ]
        , docLines
          $ (\(L _ (ConDeclField names t _)) -> 
              docSeq [docCommaSep, createNamesAndTypeDoc names t])
          <$> fields
        , docLit $ Text.pack "}"
        ]
      )
  InfixCon arg1 arg2 -> docSeq
    [ layoutType arg1
    , docSeparator
    , docLit consNameStr
    , docSeparator
    , layoutType arg2
    ]

createForallDoc :: Maybe (LHsQTyVars RdrName) -> ToBriDocM BriDocNumbered
createForallDoc Nothing                   = docEmpty
createForallDoc (Just (HsQTvs _ bs _)) = do
  tDoc <- fmap return $ createBndrDoc bs
  docSeq
    [ docLit (Text.pack "forall ")
    , tDoc
    , docLit (Text.pack " .")
    , docSeparator
    ]

createNamesAndTypeDoc
  :: [GenLocated t (FieldOcc u)] -> Located (HsType RdrName) -> ToBriDocM BriDocNumbered
createNamesAndTypeDoc names t = docSeq
  [ docSeq
    $   List.intersperse docCommaSep
    $   names
    <&> \(L _ (FieldOcc fieldName _)) ->
          docLit =<< lrdrNameToTextAnn fieldName
  , docSeparator
  , docLit $ Text.pack "::"
  , docSeparator
  , layoutType t
  ]
