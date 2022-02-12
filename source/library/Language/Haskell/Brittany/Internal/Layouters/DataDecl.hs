{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.DataDecl where

import qualified Data.Data
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import GHC (GenLocated(L), Located)
import qualified GHC
import GHC.Hs
import qualified GHC.OldList as List
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.LayouterBasics
import Language.Haskell.Brittany.Internal.Layouters.Type
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types



layoutDataDecl
  :: Data.Data.Data an1
  => LocatedAn an1 (TyClDecl GhcPs)
  -> LocatedAn an2 RdrName
  -> LHsQTyVars GhcPs
  -> HsDataDefn GhcPs
  -> ToBriDocM BriDocNumbered
layoutDataDecl ltycl name (HsQTvs _ bndrs) defn = case defn of
  -- newtype MyType a b = MyType ..
  HsDataDefn _ext NewType _ctxt _ctype Nothing [cons] mDerivs ->
    case cons of
      (L _ (ConDeclH98 _ext consName False _qvars (Just (L _ [])) details _conDoc))
        -> docWrapNode ltycl $ do
          nameStr <- lrdrNameToTextAnn name
          consNameStr <- lrdrNameToTextAnn consName
          tyVarLine <- return <$> createBndrDoc bndrs
          -- headDoc     <- fmap return $ docSeq
          --   [ appSep $ docLitS "newtype")
          --   , appSep $ docLit nameStr
          --   , appSep tyVarLine
          --   ]
          rhsDoc <- return <$> createDetailsDoc consNameStr details
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
  HsDataDefn _ext DataType mLhsContext _ctype Nothing [] mDerivs ->
    docWrapNode ltycl $ do
      lhsContextDoc <- docSharedWrapper createContextDoc mLhsContext
      nameStr <- lrdrNameToTextAnn name
      tyVarLine <- return <$> createBndrDoc bndrs
      createDerivingPar mDerivs $ docSeq
        [ appSep $ docLitS "data"
        , lhsContextDoc
        , appSep $ docLit nameStr
        , appSep tyVarLine
        ]

  -- data MyData = MyData ..
  -- data MyData = MyData { .. }
  HsDataDefn _ext DataType mLhsContext _ctype Nothing [cons] mDerivs ->
    case cons of
      (L _ (ConDeclH98 _ext consName _hasExt qvars mRhsContext details _conDoc))
        -> docWrapNode ltycl $ do
          lhsContextDoc <- docSharedWrapper createContextDoc mLhsContext
          nameStr <- lrdrNameToTextAnn name
          consNameStr <- lrdrNameToTextAnn consName
          tyVarLine <- return <$> createBndrDoc bndrs
          forallDocMay <- case createForallDoc qvars of
            Nothing -> pure Nothing
            Just x -> Just . pure <$> x
          rhsContextDocMay <- case mRhsContext of
            Nothing -> pure Nothing
            Just lctxt -> Just . pure <$> createContextDoc (Just lctxt)
          rhsDoc <- return <$> createDetailsDoc consNameStr details
          consDoc <-
            fmap pure
            $ docNonBottomSpacing
            $ case (forallDocMay, rhsContextDocMay) of
                (Just forallDoc, Just rhsContextDoc) -> docLines
                  [ docSeq
                    [docLitS "=", docSeparator, docForceSingleline forallDoc]
                  , docSeq
                    [ docLitS "."
                    , docSeparator
                    , docSetBaseY $ docLines [rhsContextDoc, docSetBaseY rhsDoc]
                    ]
                  ]
                (Just forallDoc, Nothing) -> docLines
                  [ docSeq
                    [docLitS "=", docSeparator, docForceSingleline forallDoc]
                  , docSeq [docLitS ".", docSeparator, rhsDoc]
                  ]
                (Nothing, Just rhsContextDoc) -> docSeq
                  [ docLitS "="
                  , docSeparator
                  , docSetBaseY $ docLines [rhsContextDoc, docSetBaseY rhsDoc]
                  ]
                (Nothing, Nothing) ->
                  docSeq [docLitS "=", docSeparator, rhsDoc]
          createDerivingPar mDerivs $ docAlt
            [ -- data D = forall a . Show a => D a
              docSeq
              [ docNodeAnnKW ltycl (Just GHC.AnnData) $ docSeq
                [ appSep $ docLitS "data"
                , docForceSingleline $ lhsContextDoc
                , appSep $ docLit nameStr
                , appSep tyVarLine
                , docSeparator
                ]
              , docLitS "="
              , docSeparator
              , docSetIndentLevel $ docSeq
                [ case forallDocMay of
                  Nothing -> docEmpty
                  Just forallDoc ->
                    docSeq
                      [ docForceSingleline forallDoc
                      , docSeparator
                      , docLitS "."
                      , docSeparator
                      ]
                , maybe docEmpty docForceSingleline rhsContextDocMay
                , rhsDoc
                ]
              ]
            , -- data D
              --   = forall a . Show a => D a
              docAddBaseY BrIndentRegular $ docPar
              (docNodeAnnKW ltycl (Just GHC.AnnData) $ docSeq
                [ appSep $ docLitS "data"
                , docForceSingleline lhsContextDoc
                , appSep $ docLit nameStr
                , tyVarLine
                ]
              )
              (docSeq
                [ docLitS "="
                , docSeparator
                , docSetIndentLevel $ docSeq
                  [ case forallDocMay of
                    Nothing -> docEmpty
                    Just forallDoc ->
                      docSeq
                        [ docForceSingleline forallDoc
                        , docSeparator
                        , docLitS "."
                        , docSeparator
                        ]
                  , maybe docEmpty docForceSingleline rhsContextDocMay
                  , rhsDoc
                  ]
                ]
              )
            , -- data D
              --   = forall a
              --   . Show a =>
              --     D a
              docAddBaseY BrIndentRegular $ docPar
              (docNodeAnnKW ltycl (Just GHC.AnnData) $ docSeq
                [ appSep $ docLitS "data"
                , docForceSingleline lhsContextDoc
                , appSep $ docLit nameStr
                , tyVarLine
                ]
              )
              consDoc
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
              (docLines
                [ lhsContextDoc
                , docNodeAnnKW ltycl (Just GHC.AnnData)
                  $ docSeq [appSep $ docLit nameStr, tyVarLine]
                , consDoc
                ]
              )
            ]
      _ -> briDocByExactNoComment ltycl

  _ -> briDocByExactNoComment ltycl

createContextDoc :: Maybe (LHsContext GhcPs) -> ToBriDocM BriDocNumbered
createContextDoc Nothing = docEmpty
createContextDoc (Just (L _ lhsContext)) = case lhsContext of
  [] -> docEmpty
  [t] -> docSeq [layoutType t, docSeparator, docLitS "=>", docSeparator]
  (t1 : tR) -> do
    t1Doc <- docSharedWrapper layoutType t1
    tRDocs <- tR `forM` docSharedWrapper layoutType
    docAlt
      [ docSeq
        [ docLitS "("
        , docForceSingleline $ docSeq $ List.intersperse
          docCommaSep
          (t1Doc : tRDocs)
        , docLitS ") =>"
        , docSeparator
        ]
      , docLines $ join
        [ [docSeq [docLitS "(", docSeparator, t1Doc]]
        , tRDocs <&> \tRDoc -> docSeq [docLitS ",", docSeparator, tRDoc]
        , [docLitS ") =>", docSeparator]
        ]
      ]

createBndrDoc :: [LHsTyVarBndr flag GhcPs] -> ToBriDocM BriDocNumbered
createBndrDoc bs = do
  tyVarDocs <- bs `forM` \case
    (L _ (UserTyVar _ _ext vname)) -> return $ (lrdrNameToText vname, Nothing)
    (L _ (KindedTyVar _ _ext lrdrName kind)) -> do
      d <- docSharedWrapper layoutType kind
      return $ (lrdrNameToText lrdrName, Just $ d)
  docSeq $ List.intersperse docSeparator $ tyVarDocs <&> \(vname, mKind) ->
    case mKind of
      Nothing -> docLit vname
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
    docPar mainDoc
      $ docEnsureIndent BrIndentRegular
      $ docLines
      $ docWrapNode (noLocA derivs)
      $ derivingClauseDoc
      <$> derivs

derivingClauseDoc :: LHsDerivingClause GhcPs -> ToBriDocM BriDocNumbered
derivingClauseDoc (L _ (HsDerivingClause _ext mStrategy types)) = case types of
  (L _ (DctSingle _ t)) -> derivingClauseDoc' [t]
  (L _ (DctMulti _ ts)) -> derivingClauseDoc' ts
 where
  derivingClauseDoc' [] = docSeq []
  derivingClauseDoc' ts = 
    let
      tsLength = length ts
      whenMoreThan1Type val = if tsLength > 1 then docLitS val else docLitS ""
      (lhsStrategy, rhsStrategy) =
        maybe (docEmpty, docEmpty) strategyLeftRight mStrategy
    in docSeq
      [ docDeriving
      , docWrapNodePrior types $ lhsStrategy
      , docSeparator
      , whenMoreThan1Type "("
      , docWrapNodeRest types
      $ docSeq
      $ List.intersperse docCommaSep
      $ ts
      <&> \case
            _ -> undefined
            -- HsIB _ t -> layoutType t
      , whenMoreThan1Type ")"
      , rhsStrategy
      ]
  strategyLeftRight 
    :: Located (DerivStrategy GhcPs) 
    -> (ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)
  strategyLeftRight = \case
    (L _ (StockStrategy _)) -> (docLitS " stock", docEmpty)
    (L _ (AnyclassStrategy _)) -> (docLitS " anyclass", docEmpty)
    (L _ (NewtypeStrategy _)) -> (docLitS " newtype", docEmpty)
    lVia@(L _ (ViaStrategy viaTypes)) ->
      ( docEmpty
      , case viaTypes of
          XViaStrategyPs _epann (L _span (HsSig _sig _bndrs t)) -> 
            docSeq [docWrapNode (reLocA lVia) $ docLitS " via", docSeparator, layoutType t]
      )

docDeriving :: ToBriDocM BriDocNumbered
docDeriving = docLitS "deriving"

createDetailsDoc
  :: Text -> HsConDeclH98Details GhcPs -> (ToBriDocM BriDocNumbered)
createDetailsDoc consNameStr details = case details of
  PrefixCon _ args -> do
    indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
    let
      singleLine = docSeq
        [ docLit consNameStr
        , docSeparator
        , docForceSingleline
        $ docSeq
        $ List.intersperse docSeparator
        $ fmap hsScaledThing args
        <&> layoutType
        ]
      leftIndented =
        docSetParSpacing
          . docAddBaseY BrIndentRegular
          . docPar (docLit consNameStr)
          . docLines
          $ layoutType
          <$> fmap hsScaledThing args
      multiAppended = docSeq
        [ docLit consNameStr
        , docSeparator
        , docSetBaseY $ docLines $ layoutType <$> fmap hsScaledThing args
        ]
      multiIndented = docSetBaseY $ docAddBaseY BrIndentRegular $ docPar
        (docLit consNameStr)
        (docLines $ layoutType <$> fmap hsScaledThing args)
    case indentPolicy of
      IndentPolicyLeft -> docAlt [singleLine, leftIndented]
      IndentPolicyMultiple -> docAlt [singleLine, multiAppended, leftIndented]
      IndentPolicyFree ->
        docAlt [singleLine, multiAppended, multiIndented, leftIndented]
  RecCon (L _ []) ->
    docSeq [docLit consNameStr, docSeparator, docLit $ Text.pack "{}"]
  RecCon lRec@(L _ fields@(_ : _)) -> do
    let ((fName1, fType1) : fDocR) = mkFieldDocs fields
    -- allowSingleline <- mAsk <&> _conf_layout .> _lconfig_allowSinglelineRecord .> confUnpack
    let allowSingleline = False
    docAddBaseY BrIndentRegular $ runFilteredAlternative $ do
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
            [ docCols
              ColRecDecl
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
              [ docCols
                ColRecDecl
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
    [ layoutType $ hsScaledThing arg1
    , docSeparator
    , docLit consNameStr
    , docSeparator
    , layoutType $ hsScaledThing arg2
    ]
 where
  mkFieldDocs
    :: [LConDeclField GhcPs]
    -> [(ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)]
  mkFieldDocs = fmap $ \lField -> case lField of
    L _ (ConDeclField _ext names t _) -> createNamesAndTypeDoc lField names t

createForallDoc
  :: [LHsTyVarBndr flag GhcPs] -> Maybe (ToBriDocM BriDocNumbered)
createForallDoc [] = Nothing
createForallDoc lhsTyVarBndrs =
  Just $ docSeq [docLitS "forall ", createBndrDoc lhsTyVarBndrs]

createNamesAndTypeDoc
  :: Data.Data.Data ast
  => LocatedAn an1 ast
  -> [GenLocated t (FieldOcc GhcPs)]
  -> LocatedAn AnnListItem (HsType GhcPs)
  -> (ToBriDocM BriDocNumbered, ToBriDocM BriDocNumbered)
createNamesAndTypeDoc lField names t =
  ( docNodeAnnKW lField Nothing $ docWrapNodePrior lField $ docSeq
    [ docSeq $ List.intersperse docCommaSep $ names <&> \case
        L _ (FieldOcc _ fieldName) -> docLit =<< lrdrNameToTextAnn fieldName
    ]
  , docWrapNodeRest lField $ layoutType t
  )
