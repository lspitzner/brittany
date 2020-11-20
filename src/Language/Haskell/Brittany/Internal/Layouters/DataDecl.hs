{-# LANGUAGE CPP #-}
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
import           Data.Traversable (for)

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
  -- newtype MyType a b = MyType ..
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
        --   [ appSep $ docLitS "newtype")
        --   , appSep $ docLit nameStr
        --   , appSep tyVarLine
        --   ]
        rhsDoc      <- fmap return $ createDetailsDoc consNameStr details
        createDerivingPar mDerivs $ docSeq
          [ appSep $ docLitS "newtype"
          , appSep $ docLit nameStr
          , appSep tyVarLine
          , docSeparator
          , docLitS "="
          , docSeparator
          , rhsDoc
          ]
    _ -> briDocByExactNoComment ltycl


  -- data MyData a b
  -- (zero constructors)
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
        [ appSep $ docLitS "data"
        , lhsContextDoc
        , appSep $ docLit nameStr
        , appSep tyVarLine
        ]

  -- data MyData = MyData ..
  -- data MyData = MyData { .. }
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  HsDataDefn _ext DataType (L _ lhsContext) _ctype Nothing conss mDerivs ->
#else
  HsDataDefn DataType (L _ lhsContext) _ctype Nothing conss mDerivs ->
#endif
    docWrapNode ltycl $ do
      lhsContextDoc <- docSharedWrapper createContextDoc lhsContext
      nameStr       <- lrdrNameToTextAnn name
      tyVarLine     <- fmap return $ createBndrDoc bndrs
      consDocs <- for (filter (not . isGadt) conss) $ \case
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
        (L _ (ConDeclH98 _ext consName (L _ _hasExt) qvars mRhsContext details _conDoc)) -> do
#else
        (L _ (ConDeclH98 consName (Just (HsQTvs _ qvars _)) mRhsContext details _conDoc)) -> do
#endif
          consNameStr   <- lrdrNameToTextAnn consName
          forallDocMay  <- case createForallDoc qvars of
            Nothing -> pure Nothing
            Just x -> Just . pure <$> x
          rhsContextDocMay <- case mRhsContext of
            Nothing         -> pure Nothing
            Just (L _ ctxt) -> Just . pure <$> createContextDoc ctxt
          rhsDoc        <- fmap return $ createDetailsDoc consNameStr details
          fmap pure
            $ docNonBottomSpacing
            $ case (forallDocMay, rhsContextDocMay) of
                (Just forallDoc, Just rhsContextDoc) -> docAlt
                  [ docLines
                    [ docSeq [docForceSingleline forallDoc]
                    , docSeq
                      [ docLitS "."
                      , docSeparator
                      , docSetBaseY $ docLines [rhsContextDoc, docSetBaseY rhsDoc]
                      ]
                    ]
                  , docSeq
                    [ forallDoc
                    , docSeparator
                    , docLitS "."
                    , docSeparator
                    , rhsContextDoc
                    , rhsDoc
                    ]
                  ]
                (Just forallDoc, Nothing) -> docLines
                  [ docSeq [docForceSingleline forallDoc]
                  , docSeq [docLitS ".", docSeparator, rhsDoc]
                  ]
                (Nothing, Just rhsContextDoc) -> docSeq
                  [ docSetBaseY $ docLines [rhsContextDoc, docSetBaseY rhsDoc]
                  ]
                (Nothing, Nothing) -> docSeq [rhsDoc]
      createDerivingPar mDerivs $ docAlt
        [ -- data D = forall a . Show a => D a
          docAddBaseY BrIndentRegular
            $ docSeq
              [ docNodeAnnKW ltycl (Just GHC.AnnData) $ docSeq
                [ appSep $ docLitS "data"
                , docForceSingleline lhsContextDoc
                , appSep $ docLit nameStr
                , tyVarLine
                ]
              , parConstructors consDocs
              ]
        , -- data D
          --   = forall a
          --   . Show a =>
          --     D a
          docAddBaseY BrIndentRegular
            $ docPar ( docNodeAnnKW ltycl (Just GHC.AnnData)
            $ docSeq
              [ appSep $ docLitS "data"
              , docForceSingleline lhsContextDoc
              , appSep $ docLit nameStr
              , tyVarLine
              ]
            )
            (parConstructors consDocs)
        , -- data
          --   Show a =>
          --   D
          --   = forall a
          --   . Show a =>
          --     D a
          -- This alternative is only for -XDatatypeContexts.
          -- But I think it is rather unlikely this will trigger without
          -- -XDataTypeContexts, especially with the `docNonBottomSpacing`
          -- above, so while not strictly necessary, this should not
          -- hurt.
          docAddBaseY BrIndentRegular $ docPar
          (docLitS "data")
          ( docLines
            [ lhsContextDoc
            , docNodeAnnKW ltycl (Just GHC.AnnData)
            $ docSeq
              [ appSep $ docLit nameStr
              , tyVarLine
              ]
            , parConstructors consDocs
            ]
          )
        ]

  _ -> briDocByExactNoComment ltycl

isGadt :: Located (ConDecl pass) -> Bool
isGadt (L _ ConDeclGADT{}) = True
isGadt (L _ ConDeclH98{}) = False
isGadt (L _ XConDecl{}) = False

parConstructors :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
parConstructors [] = docEmpty
parConstructors [cons] = docAlt
  [ docSeq
    [ docSeparator
    , docLit (Text.pack "=")
    , docSeparator
    , cons
    ]
  , docPar docEmpty
      $ docSeq
        [ docLit (Text.pack "=")
        , docSeparator
        , cons
        ]
  ]
parConstructors (cons:additional) =
  docPar docEmpty
    $ docLines
    $ docSeq [docLit (Text.pack "=") , docSeparator , cons]
    : toSum additional
 where
  toSum = map (\x -> docSeq [docLit (Text.pack "|"), docSeparator, x])

createContextDoc :: HsContext GhcPs -> ToBriDocM BriDocNumbered
createContextDoc [] = docEmpty
createContextDoc [t] =
  docSeq [layoutType t, docSeparator, docLitS "=>", docSeparator]
createContextDoc (t1 : tR) = do
  t1Doc  <- docSharedWrapper layoutType t1
  tRDocs <- tR `forM` docSharedWrapper layoutType
  docAlt
    [ docSeq
      [ docLitS "("
      , docForceSingleline $ docSeq $ List.intersperse docCommaSep
                                                       (t1Doc : tRDocs)
      , docLitS ") =>"
      , docSeparator
      ]
    , docLines $ join
      [ [docSeq [docLitS "(", docSeparator, t1Doc]]
      , tRDocs
        <&> \tRDoc -> docSeq [docLitS ",", docSeparator, tRDoc]
      , [docLitS ") =>", docSeparator]
      ]
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
            [ docLitS "("
            , docLit vname
            , docSeparator
            , docLitS "::"
            , docSeparator
            , kind
            , docLitS ")"
            ]

createDerivingPar
  :: HsDeriving GhcPs -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
createDerivingPar derivs mainDoc = do
  case derivs of
#if MIN_VERSION_ghc(8,2,0)   /* ghc-8.2 */
    (L _ []) -> mainDoc
    (L _ types) ->
      docPar mainDoc
        $   docEnsureIndent BrIndentRegular
        $   docLines
        $   docWrapNode derivs
        $   derivingClauseDoc
        <$> types
#else
    Nothing -> mainDoc
    Just types ->
      docPar mainDoc
        $ docEnsureIndent BrIndentRegular
        $ derivingClauseDoc types
#endif

#if MIN_VERSION_ghc(8,2,0)   /* ghc-8.2 */
derivingClauseDoc :: LHsDerivingClause GhcPs -> ToBriDocM BriDocNumbered
#else
derivingClauseDoc :: Located [LHsSigType GhcPs] -> ToBriDocM BriDocNumbered
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
derivingClauseDoc (L _ (XHsDerivingClause ext)) = absurdExt ext
derivingClauseDoc (L _ (HsDerivingClause _ext mStrategy types)) = case types of
#elif MIN_VERSION_ghc(8,2,0)   /* ghc-8.2 */
derivingClauseDoc (L _ (HsDerivingClause mStrategy types)) = case types of
#else
derivingClauseDoc types = case types of
#endif
  (L _ []) -> docSeq []
  (L _ ts) ->
    let
      tsLength = length ts
      whenMoreThan1Type val =
        if tsLength > 1 then docLitS val else docLitS ""
#if MIN_VERSION_ghc(8,2,0)   /* ghc-8.2 */
      (lhsStrategy, rhsStrategy) = maybe (docEmpty, docEmpty) strategyLeftRight mStrategy
#else
      (lhsStrategy, rhsStrategy) = (docEmpty, docEmpty)
#endif
    in
      docSeq
        [ docDeriving
        , docWrapNodePrior types $ lhsStrategy
        , docSeparator
        , whenMoreThan1Type "("
        , docWrapNodeRest types
          $ docSeq
          $ List.intersperse docCommaSep
          $ ts <&> \case
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
            HsIB _ t -> layoutType t
            XHsImplicitBndrs x -> absurdExt x
#elif MIN_VERSION_ghc(8,2,0)   /* ghc-8.2 */
            HsIB _ t _ -> layoutType t
#else
            HsIB _ t -> layoutType t
#endif
        , whenMoreThan1Type ")"
        , rhsStrategy
        ]
#if MIN_VERSION_ghc(8,2,0)   /* ghc-8.6 */
 where
  strategyLeftRight = \case
    (L _ StockStrategy          ) -> (docLitS " stock", docEmpty)
    (L _ AnyclassStrategy       ) -> (docLitS " anyclass", docEmpty)
    (L _ NewtypeStrategy        ) -> (docLitS " newtype", docEmpty)
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    lVia@(L _ (ViaStrategy viaTypes) ) ->
      ( docEmpty
      , case viaTypes of
          HsIB _ext t -> docSeq
            [ docWrapNode lVia $ docLitS " via"
            , docSeparator
            , layoutType t
            ]
          XHsImplicitBndrs ext -> absurdExt ext
      )
#endif
#endif

docDeriving :: ToBriDocM BriDocNumbered
docDeriving = docLitS "deriving"

createDetailsDoc
  :: Text -> HsConDeclDetails GhcPs -> (ToBriDocM BriDocNumbered)
createDetailsDoc consNameStr details = case details of
  PrefixCon args -> do
    indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .>  confUnpack
    let
      singleLine = docSeq
        [ docLit consNameStr
        , docSeparator
        , docForceSingleline
          $ docSeq
          $ List.intersperse docSeparator
          $ args <&> layoutType
        ]
      leftIndented = docSetParSpacing
        . docAddBaseY BrIndentRegular
        . docPar (docLit consNameStr)
        . docLines
        $ layoutType <$> args
      multiAppended = docSeq
        [ docLit consNameStr
        , docSeparator
        , docSetBaseY $ docLines $ layoutType <$> args
        ]
      multiIndented = docSetBaseY $ docAddBaseY BrIndentRegular $ docPar
        (docLit consNameStr)
        (docLines $ layoutType <$> args)
    case indentPolicy of
      IndentPolicyLeft     -> docAlt [singleLine, leftIndented]
      IndentPolicyMultiple -> docAlt [singleLine, multiAppended, leftIndented]
      IndentPolicyFree ->
        docAlt [singleLine, multiAppended, multiIndented, leftIndented]
  RecCon (L _ []) -> docSeq [docLit consNameStr, docSeparator, docLit $ Text.pack "{}"]
  RecCon lRec@(L _ fields@(_:_)) -> do
    let ((fName1, fType1) : fDocR) = mkFieldDocs fields
    -- allowSingleline <- mAsk <&> _conf_layout .> _lconfig_allowSinglelineRecord .> confUnpack
    let allowSingleline = False
    docAddBaseY BrIndentRegular
      $ runFilteredAlternative
      $ do
        -- single-line: { i :: Int, b :: Bool }
        addAlternativeCond allowSingleline $ docSeq
          [ docLit consNameStr
          , docSeparator
          , docWrapNodePrior lRec $ docLitS "{"
          , docSeparator
          , docWrapNodeRest lRec
            $ docForceSingleline
            $ docSeq
            $ join
            $ [fName1, docSeparator, docLitS "::", docSeparator, fType1]
              : [ [ docLitS ","
                  , docSeparator
                  , fName
                  , docSeparator
                  , docLitS "::"
                  , docSeparator
                  , fType
                  ]
                | (fName, fType) <- fDocR
                ]
          , docSeparator
          , docLitS "}"
          ]
        addAlternative $ docPar
          (docLit consNameStr)
          (docWrapNodePrior lRec $ docNonBottomSpacingS $ docLines
            [ docAlt
              [ docCols ColRecDecl
                [ appSep (docLitS "{")
                , appSep $ docForceSingleline fName1
                , docSeq [docLitS "::", docSeparator]
                , docForceSingleline $ fType1
                ]
              , docSeq
                [ docLitS "{"
                , docSeparator
                , docSetBaseY $ docAddBaseY BrIndentRegular $ docPar
                    fName1
                    (docSeq [docLitS "::", docSeparator, fType1])
                ]
              ]
            , docWrapNodeRest lRec $ docLines $ fDocR <&> \(fName, fType) ->
                docAlt
                [ docCols ColRecDecl
                  [ docCommaSep
                  , appSep $ docForceSingleline fName
                  , docSeq [docLitS "::", docSeparator]
                  , docForceSingleline fType
                  ]
                , docSeq
                  [ docLitS ","
                  , docSeparator
                  , docSetBaseY $ docAddBaseY BrIndentRegular $ docPar
                      fName
                      (docSeq [docLitS "::", docSeparator, fType])
                  ]
                ]
            , docLitS "}"
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
  mkFieldDocs
    :: [LConDeclField GhcPs]
    -> [(ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)]
  mkFieldDocs = fmap $ \lField -> case lField of
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    L _ (ConDeclField _ext names t _) -> createNamesAndTypeDoc lField names t
    L _ (XConDeclField x) -> absurdExt x
#else
    L _ (ConDeclField names t _) -> createNamesAndTypeDoc lField names t
#endif

createForallDoc :: [LHsTyVarBndr GhcPs] -> Maybe (ToBriDocM BriDocNumbered)
createForallDoc []            = Nothing
createForallDoc lhsTyVarBndrs = Just $ docSeq
  [docLitS "forall ", createBndrDoc lhsTyVarBndrs]

createNamesAndTypeDoc
  :: Data.Data.Data ast
  => Located ast
  -> [GenLocated t (FieldOcc GhcPs)]
  -> Located (HsType GhcPs)
  -> (ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)
createNamesAndTypeDoc lField names t =
  ( docNodeAnnKW lField Nothing $ docWrapNodePrior lField $ docSeq
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
    ]
  , docWrapNodeRest lField $ layoutType t
  )
