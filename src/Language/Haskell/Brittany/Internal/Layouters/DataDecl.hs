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
import           BasicTypes
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
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
layoutDataDecl _ _ (XLHsQTyVars ext) _ = absurdExt ext
layoutDataDecl ltycl name (HsQTvs _ bndrs) defn = case defn of
#else
layoutDataDecl ltycl name (HsQTvs _ bndrs _) defn = case defn of
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  HsDataDefn _ext NewType (L _ []) _ctype Nothing [cons] mDerivs -> case cons of
    (L _ (ConDeclH98 _ext consName (L _ False) _qvars (Just (L _ [])) details _conDoc)) ->
#else
  HsDataDefn NewType (L _ []) _ctype Nothing [cons] mDerivs -> case cons of
    (L _ (ConDeclH98 consName Nothing (Just (L _ [])) details _conDoc)) ->
#endif
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
    _ -> briDocByExactNoComment ltycl

#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  HsDataDefn _ext DataType (L _ lhsContext) _ctype Nothing [] mDerivs ->
#else
  HsDataDefn DataType (L _ lhsContext) _ctype Nothing [] mDerivs ->
#endif
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

#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  HsDataDefn _ext DataType (L _ lhsContext) _ctype Nothing [cons] mDerivs ->
#else
  HsDataDefn DataType (L _ lhsContext) _ctype Nothing [cons] mDerivs ->
#endif
    case cons of
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
      (L _ (ConDeclH98 _ext consName (L _ _hasExt) qvars mRhsContext details _conDoc)) ->
#else
      (L _ (ConDeclH98 consName (Just (HsQTvs _ qvars _)) mRhsContext details _conDoc)) ->
#endif
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
      _ -> briDocByExactNoComment ltycl

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
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    (L _ (UserTyVar _ext vname)) -> return $ (lrdrNameToText vname, Nothing)
    (L _ (KindedTyVar _ext lrdrName kind)) -> do
#else
    (L _ (UserTyVar vname)) -> return $ (lrdrNameToText vname, Nothing)
    (L _ (KindedTyVar lrdrName kind)) -> do
#endif
      d <- docSharedWrapper layoutType kind
      return $ (lrdrNameToText lrdrName, Just $ d)
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    (L _ (XTyVarBndr ext)) -> absurdExt ext
#endif
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
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
derivingClauseDoc (L _ (XHsDerivingClause ext)) = absurdExt ext
derivingClauseDoc (L _ (HsDerivingClause _ext mStrategy types)) = case types of
#else
derivingClauseDoc (L _ (HsDerivingClause mStrategy types)) = case types of
#endif
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
        , docSeq $ List.intersperse docCommaSep $ ts <&> \case
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
            HsIB _ t -> layoutType t
            XHsImplicitBndrs x -> absurdExt x
#else
            HsIB _ t _ -> layoutType t
#endif
        , whenMoreThan1Type ")"
        , rhsStrategy
        ]
 where
  strategyLeftRight = \case
    (L _ StockStrategy          ) -> (docLit $ Text.pack " stock", docEmpty)
    (L _ AnyclassStrategy       ) -> (docLit $ Text.pack " anyclass", docEmpty)
    (L _ NewtypeStrategy        ) -> (docLit $ Text.pack " newtype", docEmpty)
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    (L _ (ViaStrategy viaTypes) ) ->
      ( docEmpty
      , case viaTypes of
          HsIB _ext t -> docSeq
            [ docLit $ Text.pack " via "
            , layoutType t
            ]
          XHsImplicitBndrs ext -> absurdExt ext
      )
#endif

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
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  RecCon (L _ [L _ (ConDeclField _ext names t _)]) -> docSeq
#else
  RecCon (L _ [L _ (ConDeclField names t _)]) -> docSeq
#endif
    [ docLit consNameStr
    , docSeparator
    , appSep $ docLit $ Text.pack "{"
    , docSeq $ createNamesAndTypeDoc names t
    , docSeparator
    , docLit $ Text.pack "}"
    ]
  RecCon (L _ fields@(_:_)) -> do
    let (fDoc1 : fDocR) = mkFieldDocs fields
    docAddBaseY BrIndentRegular $ docPar
      (docLit consNameStr)
      (docLines
        [ docCols ColRecDecl
          $ docLit (Text.pack "{ ")
          : fDoc1
        , docLines $ fDocR <&> \f -> docCols ColRecDecl $ docCommaSep : f
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
 where
  mkFieldDocs = fmap $ \case
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    L _ (ConDeclField _ext names t _) -> createNamesAndTypeDoc names t
    L _ (XConDeclField x) -> absurdExt x
#else
    L _ (ConDeclField names t _) -> createNamesAndTypeDoc names t
#endif

createForallDoc :: [LHsTyVarBndr GhcPs] -> ToBriDocM BriDocNumbered
createForallDoc []            = docEmpty
createForallDoc lhsTyVarBndrs =  docSeq
  [ docLit (Text.pack "forall ")
  , createBndrDoc lhsTyVarBndrs
  , docLit (Text.pack " .")
  , docSeparator
  ]

createNamesAndTypeDoc
  :: [GenLocated t (FieldOcc GhcPs)]
  -> Located (HsType GhcPs)
  -> [ToBriDocM BriDocNumbered]
createNamesAndTypeDoc names t =
  [ docSeq
    [ docSeq
      $   List.intersperse docCommaSep
      $   names
      <&> \case
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
        L _ (XFieldOcc x) -> absurdExt x
        L _ (FieldOcc _ fieldName) ->
#else
        L _ (FieldOcc fieldName _) ->
#endif
            docLit =<< lrdrNameToTextAnn fieldName
    , docSeparator
    ]
  , docSeq
    [ docLit $ Text.pack "::"
    , docSeparator
    , layoutType t
    ]
  ]
