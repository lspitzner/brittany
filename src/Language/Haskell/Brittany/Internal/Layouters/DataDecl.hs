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
import           GHC ( Located, runGhc, GenLocated(L), moduleNameString, DerivStrategy(..) )
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
  :: Located (TyClDecl GhcPs)
  -> Located RdrName
  -> LHsQTyVars GhcPs
  -> HsDataDefn GhcPs
  -> ToBriDocM BriDocNumbered
layoutDataDecl _ _ (XLHsQTyVars ext) _ = absurdExt ext
layoutDataDecl ltycl name (HsQTvs _ bndrs) defn = case defn of
  HsDataDefn _ext NewType (L _ []) _ctype Nothing [cons] mDerivs -> case cons of
    (L _ (ConDeclH98 _ext consName (L _ False) _qvars (Just (L _ [])) details _conDoc)) ->
      docWrapNode ltycl $ do
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
    _ -> briDocByExact ltycl

  HsDataDefn _ext DataType (L _ lhsContext) _ctype Nothing [] mDerivs ->
    docWrapNode ltycl $ do
      lhsContextDoc <- docSharedWrapper createContextDoc lhsContext
      nameStr       <- lrdrNameToTextAnn name
      tyVarLine     <- fmap return $ createBndrDoc bndrs
      createDerivingPar mDerivs $ docSeq
        [ appSep $ docLit (Text.pack "data")
        , lhsContextDoc
        , appSep $ docLit nameStr
        , appSep tyVarLine
        ]

  HsDataDefn _ext DataType (L _ lhsContext) _ctype Nothing [cons] mDerivs ->
    case cons of
      (L _ (ConDeclH98 _ext consName (L _ _hasExt) qvars mRhsContext details _conDoc)) ->
        docWrapNode ltycl $ do
          lhsContextDoc <- docSharedWrapper createContextDoc lhsContext
          nameStr       <- lrdrNameToTextAnn name
          consNameStr   <- lrdrNameToTextAnn consName
          tyVarLine     <- fmap return $ createBndrDoc bndrs
          forallDoc     <- docSharedWrapper createForallDoc qvars
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
      _ -> briDocByExact ltycl

  _ -> briDocByExactNoComment ltycl

createContextDoc :: HsContext GhcPs -> ToBriDocM BriDocNumbered
createContextDoc [] = docEmpty
createContextDoc [t] =
  docSeq [layoutType t, docSeparator, docLit (Text.pack "=>"), docSeparator]
createContextDoc ts = docSeq
  [ docLit (Text.pack "(")
  , docSeq $ List.intersperse docCommaSep (layoutType <$> ts)
  , docLit (Text.pack ") =>")
  , docSeparator
  ]

createBndrDoc :: [LHsTyVarBndr GhcPs] -> ToBriDocM BriDocNumbered
createBndrDoc bs = do
  tyVarDocs <- bs `forM` \case
    (L _ (UserTyVar _ext vname)) -> return $ (lrdrNameToText vname, Nothing)
    (L _ (KindedTyVar _ext lrdrName kind)) -> do
      d <- docSharedWrapper layoutType kind
      return $ (lrdrNameToText lrdrName, Just $ d)
    (L _ (XTyVarBndr ext)) -> absurdExt ext
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
  :: HsDeriving GhcPs -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
createDerivingPar derivs mainDoc = do
  case derivs of
    (L _ []) -> docLines [mainDoc]
    (L _ types) ->
      docPar mainDoc
        $   docEnsureIndent BrIndentRegular
        $   docLines
        $   derivingClauseDoc
        <$> types

derivingClauseDoc :: LHsDerivingClause GhcPs -> ToBriDocM BriDocNumbered
derivingClauseDoc (L _ (XHsDerivingClause ext)) = absurdExt ext
derivingClauseDoc (L _ (HsDerivingClause _ext mStrategy types)) = case types of
  (L _ []) -> docSeq []
  (L _ ts) ->
    let
      tsLength = length ts
      whenMoreThan1Type val =
        if tsLength > 1 then docLit (Text.pack val) else docLit (Text.pack "")
      (lhsStrategy, rhsStrategy) = maybe (docEmpty, docEmpty) strategyLeftRight mStrategy
    in
      docSeq
        [ docDeriving
        , lhsStrategy
        , docSeparator
        , whenMoreThan1Type "("
        , docSeq $ List.intersperse docCommaSep $ ts <&> \(HsIB _ t) ->
          layoutType t
        , whenMoreThan1Type ")"
        , rhsStrategy
        ]
 where
  strategyLeftRight = \case
    (L _ StockStrategy          ) -> (docLit $ Text.pack " stock", docEmpty)
    (L _ AnyclassStrategy       ) -> (docLit $ Text.pack " anyclass", docEmpty)
    (L _ NewtypeStrategy        ) -> (docLit $ Text.pack " newtype", docEmpty)
    (L _ (ViaStrategy viaTypes) ) ->
      ( docEmpty
      , case viaTypes of
          HsIB _ext t -> docSeq
            [ docLit $ Text.pack " via "
            , layoutType t
            ]
          XHsImplicitBndrs ext -> absurdExt ext
      )

docDeriving :: ToBriDocM BriDocNumbered
docDeriving = docLit $ Text.pack "deriving"

createDetailsDoc
  :: Text -> HsConDeclDetails GhcPs -> (ToBriDocM BriDocNumbered)
createDetailsDoc consNameStr details = case details of
  PrefixCon args -> docSeq
    [ docLit consNameStr
    , docSeparator
    , docSeq $ List.intersperse docSeparator $ args <&> layoutType
    ]
  RecCon (L _ []) -> docEmpty
  RecCon (L _ [L _ (ConDeclField _ext names t _)]) -> docSeq
    [ docLit consNameStr
    , docSeparator
    , appSep $ docLit $ Text.pack "{"
    , docSeq $ createNamesAndTypeDoc names t
    , docSeparator
    , docLit $ Text.pack "}"
    ]
  RecCon (L _ (fstField:fields)) ->
    docAddBaseY BrIndentRegular $ docPar
      (docLit consNameStr)
      (docLines
        [ docCols ColRecDecl
          $ docLit (Text.pack "{ ")
          : let L _ (ConDeclField _ext names t _) = fstField
              in createNamesAndTypeDoc names t
        , docLines
          $ (\(L _ (ConDeclField _ext names t _)) ->
              docCols ColRecDecl $ docCommaSep : createNamesAndTypeDoc names t)
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

createForallDoc :: [LHsTyVarBndr GhcPs] -> ToBriDocM BriDocNumbered
createForallDoc []            = docEmpty
createForallDoc lhsTyVarBndrs =  docSeq
  [ docLit (Text.pack "forall ")
  , createBndrDoc lhsTyVarBndrs
  , docLit (Text.pack " .")
  , docSeparator
  ]

createNamesAndTypeDoc
  :: [GenLocated t (FieldOcc u)] -> Located (HsType GhcPs) -> [ToBriDocM BriDocNumbered]
createNamesAndTypeDoc names t =
  [ docSeq
    [ docSeq
      $   List.intersperse docCommaSep
      $   names
      <&> \(L _ (FieldOcc _ fieldName)) ->
            docLit =<< lrdrNameToTextAnn fieldName
    , docSeparator
    ]
  , docSeq
    [ docLit $ Text.pack "::"
    , docSeparator
    , layoutType t
    ]
  ]
