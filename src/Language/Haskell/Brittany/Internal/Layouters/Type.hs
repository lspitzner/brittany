{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Internal.Layouters.Type
  ( layoutType
  , layoutTyVarBndrs
  , processTyVarBndrsSingleline
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics

import           GHC ( runGhc
                     , GenLocated(L)
                     , moduleNameString
                     , AnnKeywordId (..)
                     )
import           Language.Haskell.GHC.ExactPrint.Types ( mkAnnKey )
import           HsSyn
import           Name
import           Outputable ( ftext, showSDocUnsafe )
import           BasicTypes
import qualified SrcLoc

import           DataTreePrint



layoutType :: ToBriDoc HsType
layoutType ltype@(L _ typ) = docWrapNode ltype $ case typ of
  -- _ | traceShow (ExactPrint.Types.mkAnnKey ltype) False -> error "impossible"
#if MIN_VERSION_ghc(8,2,0)
#if MIN_VERSION_ghc(8,6,0)
  HsTyVar _ promoted name -> do
#else   /* ghc-8.2 ghc-8.4 */
  HsTyVar promoted name -> do
#endif
    t <- lrdrNameToTextAnnTypeEqualityIsSpecial name
    case promoted of
#if MIN_VERSION_ghc(8,8,0)
      IsPromoted -> docSeq
#else /* ghc-8.2 8.4 8.6 */
      Promoted -> docSeq
#endif
        [ docSeparator
        , docTick
        , docWrapNode name $ docLit t
        ]
      NotPromoted -> docWrapNode name $ docLit t
#else /* ghc-8.0 */
  HsTyVar name -> do
    t <- lrdrNameToTextAnnTypeEqualityIsSpecial name
    docWrapNode name $ docLit t
#endif
#if MIN_VERSION_ghc(8,6,0)
  HsForAllTy _ bndrs (L _ (HsQualTy _ (L _ cntxts) typ2)) -> do
#else
  HsForAllTy bndrs (L _ (HsQualTy (L _ cntxts) typ2)) -> do
#endif
    typeDoc <- docSharedWrapper layoutType typ2
    tyVarDocs <- layoutTyVarBndrs bndrs
    cntxtDocs <- cntxts `forM` docSharedWrapper layoutType
    let maybeForceML = case typ2 of
          (L _ HsFunTy{}) -> docForceMultiline
          _               -> id
    let
      tyVarDocLineList = processTyVarBndrsSingleline tyVarDocs
      forallDoc = docAlt
        [ let
            open = docLit $ Text.pack "forall"
            in docSeq ([open]++tyVarDocLineList)
        , docPar
            (docLit (Text.pack "forall"))
            (docLines
            $ tyVarDocs <&> \case
                (tname, Nothing) -> docEnsureIndent BrIndentRegular $ docLit tname
                (tname, Just doc) -> docEnsureIndent BrIndentRegular
                  $ docLines
                    [ docCols ColTyOpPrefix
                      [ docParenLSep
                      , docLit tname
                      ]
                    , docCols ColTyOpPrefix
                      [ docLit $ Text.pack ":: "
                      , doc
                      ]
                    , docLit $ Text.pack ")"
                    ])
        ]
      contextDoc = case cntxtDocs of
        [] -> docLit $ Text.pack "()"
        [x] -> x
        _ -> docAlt
          [ let
              open  = docLit $ Text.pack "("
              close = docLit $ Text.pack ")"
              list  = List.intersperse docCommaSep
                    $ docForceSingleline <$> cntxtDocs
              in docSeq ([open]++list++[close])
          , let
              open = docCols ColTyOpPrefix
                      [ docParenLSep
                      , docAddBaseY (BrIndentSpecial 2) $ head cntxtDocs
                      ]
              close = docLit $ Text.pack ")"
              list = List.tail cntxtDocs <&> \cntxtDoc ->
                     docCols ColTyOpPrefix
                      [ docCommaSep
                      , docAddBaseY (BrIndentSpecial 2) cntxtDoc
                      ]
            in docPar open $ docLines $ list ++ [close]
          ]
    docAlt
      -- :: forall a b c . (Foo a b c) => a b -> c
      [ docSeq
        [ if null bndrs
            then docEmpty
            else let
              open = docLit $ Text.pack "forall"
              close = docLit $ Text.pack " . "
              in docSeq ([open, docSeparator]++tyVarDocLineList++[close])
        , docForceSingleline contextDoc
        , docLit $ Text.pack " => "
        , docForceSingleline typeDoc
        ]
      -- :: forall a b c
      --  . (Foo a b c)
      -- => a b
      -- -> c
      , docPar
          forallDoc
          ( docLines
            [ docCols ColTyOpPrefix
              [ docWrapNodeRest ltype $ docLit $ Text.pack " . "
              , docAddBaseY (BrIndentSpecial 3)
              $ contextDoc
              ]
            , docCols ColTyOpPrefix
              [ docLit $ Text.pack "=> "
              , docAddBaseY (BrIndentSpecial 3) $ maybeForceML $ typeDoc
              ]
            ]
          )
      ]
#if MIN_VERSION_ghc(8,6,0)
  HsForAllTy _ bndrs typ2 -> do
#else
  HsForAllTy bndrs typ2 -> do
#endif
    typeDoc <- layoutType typ2
    tyVarDocs <- layoutTyVarBndrs bndrs
    let maybeForceML = case typ2 of
          (L _ HsFunTy{}) -> docForceMultiline
          _               -> id
    let tyVarDocLineList = processTyVarBndrsSingleline tyVarDocs
    docAlt
      -- forall x . x
      [ docSeq
        [ if null bndrs
            then docEmpty
            else let
              open = docLit $ Text.pack "forall"
              close = docLit $ Text.pack " . "
              in docSeq ([open]++tyVarDocLineList++[close])
        , docForceSingleline $ return $ typeDoc
        ]
      -- :: forall x
      --  . x
      , docPar
          (docSeq $ docLit (Text.pack "forall") : tyVarDocLineList)
          ( docCols ColTyOpPrefix
            [ docWrapNodeRest ltype $ docLit $ Text.pack " . "
            , maybeForceML $ return typeDoc
            ]
          )
      -- :: forall
      --      (x :: *)
      --  . x
      , docPar
          (docLit (Text.pack "forall"))
          (docLines
          $ (tyVarDocs <&> \case
              (tname, Nothing) -> docEnsureIndent BrIndentRegular $ docLit tname
              (tname, Just doc) -> docEnsureIndent BrIndentRegular
                $ docLines
                  [ docCols ColTyOpPrefix
                    [ docParenLSep
                    , docLit tname
                    ]
                  , docCols ColTyOpPrefix
                    [ docLit $ Text.pack ":: "
                    , doc
                    ]
                  , docLit $ Text.pack ")"
                  ]
            )
          ++[ docCols ColTyOpPrefix
              [ docWrapNodeRest ltype $ docLit $ Text.pack " . "
              , maybeForceML $ return typeDoc
              ]
            ]
          )
      ]
#if MIN_VERSION_ghc(8,6,0)
  HsQualTy _ lcntxts@(L _ cntxts) typ1 -> do
#else
  HsQualTy lcntxts@(L _ cntxts) typ1 -> do
#endif
    typeDoc <- docSharedWrapper layoutType typ1
    cntxtDocs <- cntxts `forM` docSharedWrapper layoutType
    let
      contextDoc = docWrapNode lcntxts $ case cntxtDocs of
        [] -> docLit $ Text.pack "()"
        [x] -> x
        _ -> docAlt
          [ let
              open  = docLit $ Text.pack "("
              close = docLit $ Text.pack ")"
              list  = List.intersperse docCommaSep
                    $ docForceSingleline <$> cntxtDocs
              in docSeq ([open]++list++[close])
          , let
              open = docCols ColTyOpPrefix
                      [ docParenLSep
                      , docAddBaseY (BrIndentSpecial 2)
                      $ head cntxtDocs
                      ]
              close = docLit $ Text.pack ")"
              list = List.tail cntxtDocs <&> \cntxtDoc ->
                     docCols ColTyOpPrefix
                      [ docCommaSep
                      , docAddBaseY (BrIndentSpecial 2)
                      $ cntxtDoc
                      ]
            in docPar open $ docLines $ list ++ [close]
          ]
    let maybeForceML = case typ1 of
          (L _ HsFunTy{}) -> docForceMultiline
          _               -> id
    docAlt
      -- (Foo a b c) => a b -> c
      [ docSeq
        [ docForceSingleline contextDoc
        , docLit $ Text.pack " => "
        , docForceSingleline typeDoc
        ]
      --    (Foo a b c)
      -- => a b
      -- -> c
      , docPar
          (docForceSingleline contextDoc)
          ( docCols ColTyOpPrefix
            [ docLit $ Text.pack "=> "
            , docAddBaseY (BrIndentSpecial 3) $ maybeForceML typeDoc
            ]
          )
      ]
#if MIN_VERSION_ghc(8,6,0)
  HsFunTy _ typ1 typ2 -> do
#else
  HsFunTy typ1 typ2 -> do
#endif
    typeDoc1 <- docSharedWrapper layoutType typ1
    typeDoc2 <- docSharedWrapper layoutType typ2
    let maybeForceML = case typ2 of
          (L _ HsFunTy{}) -> docForceMultiline
          _               -> id
    hasComments <- hasAnyCommentsBelow ltype
    docAlt $
      [ docSeq
        [ appSep $ docForceSingleline typeDoc1
        , appSep $ docLit $ Text.pack "->"
        , docForceSingleline typeDoc2
        ]
      | not hasComments
      ] ++
      [ docPar
        (docNodeAnnKW ltype Nothing typeDoc1)
        ( docCols ColTyOpPrefix
          [ docWrapNodeRest ltype $ appSep $ docLit $ Text.pack "->"
          , docAddBaseY (BrIndentSpecial 3)
          $ maybeForceML typeDoc2
          ]
        )
      ]
#if MIN_VERSION_ghc(8,6,0)
  HsParTy _ typ1 -> do
#else
  HsParTy typ1 -> do
#endif
    typeDoc1 <- docSharedWrapper layoutType typ1
    docAlt
      [ docSeq
        [ docWrapNodeRest ltype $ docLit $ Text.pack "("
        , docForceSingleline typeDoc1
        , docLit $ Text.pack ")"
        ]
      , docPar
          ( docCols ColTyOpPrefix
            [ docWrapNodeRest ltype $ docParenLSep
            , docAddBaseY (BrIndentSpecial 2) $ typeDoc1
            ])
          (docLit $ Text.pack ")")
      ]
#if MIN_VERSION_ghc(8,6,0)
  HsAppTy _ typ1@(L _ HsAppTy{}) typ2 -> do
    let gather :: [LHsType GhcPs] -> LHsType GhcPs -> (LHsType GhcPs, [LHsType GhcPs])
        gather list = \case
          L _ (HsAppTy _ ty1 ty2) -> gather (ty2:list) ty1
          final -> (final, list)
    let (typHead, typRest) = gather [typ2] typ1
    docHead <- docSharedWrapper layoutType typHead
    docRest <- docSharedWrapper layoutType `mapM` typRest
    docAlt
      [ docSeq
      $ docForceSingleline docHead : (docRest >>= \d ->
        [ docSeparator, docForceSingleline d ])
      , docPar docHead (docLines $ docEnsureIndent BrIndentRegular <$> docRest)
      ]
  HsAppTy _ typ1 typ2 -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    typeDoc2 <- docSharedWrapper layoutType typ2
    docAlt
      [ docSeq
        [ docForceSingleline typeDoc1
        , docSeparator
        , docForceSingleline typeDoc2
        ]
      , docPar
          typeDoc1
          (docEnsureIndent BrIndentRegular typeDoc2)
      ]
#else
  HsAppTy typ1 typ2 -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    typeDoc2 <- docSharedWrapper layoutType typ2
    docAlt
      [ docSeq
        [ docForceSingleline typeDoc1
        , docSeparator
        , docForceSingleline typeDoc2
        ]
      , docPar
          typeDoc1
          (docEnsureIndent BrIndentRegular typeDoc2)
      ]
  HsAppsTy [] -> error "HsAppsTy []"
  HsAppsTy [L _ (HsAppPrefix typ1)] -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    typeDoc1
  HsAppsTy [lname@(L _ (HsAppInfix name))] -> do
    -- this redirection is somewhat hacky, but whatever.
    -- TODO: a general problem when doing deep inspections on
    --       the type (and this is not the only instance)
    --       is that we potentially omit annotations on some of
    --       the middle constructors. i have no idea under which
    --       circumstances exactly important annotations (comments)
    --       would be assigned to such constructors.
    typeDoc1 <- -- docSharedWrapper layoutType $ (L l $ HsTyVar name)
      lrdrNameToTextAnnTypeEqualityIsSpecialAndRespectTick lname name
    docLit typeDoc1
  HsAppsTy (L _ (HsAppPrefix typHead):typRestA)
    | Just typRest <- mapM (\case L _ (HsAppPrefix t) -> Just t
                                  _ -> Nothing) typRestA -> do
    docHead <- docSharedWrapper layoutType typHead
    docRest <- docSharedWrapper layoutType `mapM` typRest
    docAlt
      [ docSeq
      $ docForceSingleline docHead : (docRest >>= \d ->
        [ docSeparator, docForceSingleline d ])
      , docPar docHead (docLines $ docEnsureIndent BrIndentRegular <$> docRest)
      ]
  HsAppsTy (typHead:typRest) -> do
    docHead <- docSharedWrapper layoutAppType typHead
    docRest <- docSharedWrapper layoutAppType `mapM` typRest
    docAlt
      [ docSeq
      $ docForceSingleline docHead : (docRest >>= \d ->
        [ docSeparator, docForceSingleline d ])
      , docPar docHead (docLines $ docEnsureIndent BrIndentRegular <$> docRest)
      ]
    where
      layoutAppType (L _ (HsAppPrefix t)) = layoutType t
      layoutAppType lt@(L _ (HsAppInfix t)) =
        docLit =<< lrdrNameToTextAnnTypeEqualityIsSpecialAndRespectTick lt t
#endif
#if MIN_VERSION_ghc(8,6,0)
  HsListTy _ typ1 -> do
#else
  HsListTy typ1 -> do
#endif
    typeDoc1 <- docSharedWrapper layoutType typ1
    docAlt
      [ docSeq
        [ docWrapNodeRest ltype $ docLit $ Text.pack "["
        , docForceSingleline typeDoc1
        , docLit $ Text.pack "]"
        ]
      , docPar
          ( docCols ColTyOpPrefix
            [ docWrapNodeRest ltype $ docLit $ Text.pack "[ "
            , docAddBaseY (BrIndentSpecial 2) $ typeDoc1
            ])
          (docLit $ Text.pack "]")
      ]
#if MIN_VERSION_ghc(8,6,0)
#else
  HsPArrTy typ1 -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    docAlt
      [ docSeq
        [ docWrapNodeRest ltype $ docLit $ Text.pack "[:"
        , docForceSingleline typeDoc1
        , docLit $ Text.pack ":]"
        ]
      , docPar
          ( docCols ColTyOpPrefix
            [ docWrapNodeRest ltype $ docLit $ Text.pack "[:"
            , docAddBaseY (BrIndentSpecial 2) $ typeDoc1
            ])
          (docLit $ Text.pack ":]")
      ]
#endif
#if MIN_VERSION_ghc(8,6,0)
  HsTupleTy _ tupleSort typs -> case tupleSort of
#else
  HsTupleTy tupleSort typs -> case tupleSort of
#endif
    HsUnboxedTuple           -> unboxed
    HsBoxedTuple             -> simple
    HsConstraintTuple        -> simple
    HsBoxedOrConstraintTuple -> simple
   where
    unboxed = if null typs then error "brittany internal error: unboxed unit"
                           else unboxedL
    simple = if null typs then unitL else simpleL
    unitL = docLit $ Text.pack "()"
    simpleL = do
      docs <- docSharedWrapper layoutType `mapM` typs
      let end = docLit $ Text.pack ")"
          lines = List.tail docs <&> \d ->
            docAddBaseY (BrIndentSpecial 2)
              $ docCols ColTyOpPrefix [docCommaSep, d]
          commaDocs = List.intersperse docCommaSep (docForceSingleline <$> docs)
      docAlt
        [ docSeq $ [docLit $ Text.pack "("]
               ++ docWrapNodeRest ltype commaDocs
               ++ [end]
        , let line1 = docCols ColTyOpPrefix [docParenLSep, head docs]
          in docPar
            (docAddBaseY (BrIndentSpecial 2) $ line1)
            (docLines $ docWrapNodeRest ltype lines ++ [end])
        ]
    unboxedL = do
      docs <- docSharedWrapper layoutType `mapM` typs
      let start = docParenHashLSep
          end   = docParenHashRSep
      docAlt
        [ docSeq $ [start]
                ++ docWrapNodeRest ltype (List.intersperse docCommaSep docs)
                ++ [end]
        , let
            line1 = docCols ColTyOpPrefix [start, head docs]
            lines  = List.tail docs <&> \d ->
              docAddBaseY (BrIndentSpecial 2)
                $ docCols ColTyOpPrefix [docCommaSep, d]
          in docPar
            (docAddBaseY (BrIndentSpecial 2) line1)
            (docLines $ lines ++ [end])
        ]
  HsOpTy{} -> -- TODO
    briDocByExactInlineOnly "HsOpTy{}" ltype
  -- HsOpTy typ1 opName typ2 -> do
  --   -- TODO: these need some proper fixing. precedences don't add up.
  --   --       maybe the parser just returns some trivial right recursion
  --   --       parse result for any type level operators.
  --   --       need to check how things are handled on the expression level.
  --   let opStr = lrdrNameToText opName
  --   let opLen = Text.length opStr
  --   layouter1@(Layouter desc1 _ _) <- layoutType typ1
  --   layouter2@(Layouter desc2 _ _) <- layoutType typ2
  --   let line = do -- Maybe
  --         l1 <- _ldesc_line desc1
  --         l2 <- _ldesc_line desc2
  --         let len1 = _lColumns_min l1
  --         let len2 = _lColumns_min l2
  --         let len = 2 + opLen + len1 + len2
  --         return $ LayoutColumns
  --           { _lColumns_key = ColumnKeyUnique
  --           , _lColumns_lengths = [len]
  --           , _lColumns_min = len
  --           }
  --   let block = do -- Maybe
  --         rol1 <- descToBlockStart desc1
  --         (min2, max2) <- descToMinMax (1+opLen) desc2
  --         let (minR, maxR) = case descToBlockMinMax desc1 of
  --               Nothing -> (min2, max2)
  --               Just (min1, max1) -> (max min1 min2, max max1 max2)
  --         return $ BlockDesc
  --           { _bdesc_blockStart = rol1
  --           , _bdesc_min = minR
  --           , _bdesc_max = maxR
  --           , _bdesc_opIndentFloatUp = Just (1+opLen)
  --           }
  --   return $ Layouter
  --     { _layouter_desc = LayoutDesc
  --       { _ldesc_line = line
  --       , _ldesc_block = block
  --       }
  --     , _layouter_func = \params -> do
  --         remaining <- getCurRemaining
  --         let allowSameLine = _params_sepLines params /= SepLineTypeOp
  --         case line of
  --           Just (LayoutColumns _ _ m) | m <= remaining && allowSameLine -> do
  --             applyLayouterRestore layouter1 defaultParams
  --             layoutWriteAppend $ Text.pack " " <> opStr <> Text.pack " "
  --             applyLayouterRestore layouter2 defaultParams
  --           _ -> do
  --             let upIndent   = maybe (1+opLen) (max (1+opLen)) $ _params_opIndent params
  --             let downIndent = maybe upIndent (max upIndent) $ _bdesc_opIndentFloatUp =<< _ldesc_block desc2
  --             layoutWithAddIndentN downIndent $ applyLayouterRestore layouter1 defaultParams
  --             layoutWriteNewline
  --             layoutWriteAppend $ opStr <> Text.pack " "
  --             layoutWriteEnsureBlockPlusN downIndent
  --             applyLayouterRestore layouter2 defaultParams
  --               { _params_sepLines = SepLineTypeOp
  --               , _params_opIndent = Just downIndent
  --               }
  --     , _layouter_ast = ltype
  --     }
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  HsIParamTy _ (L _ (HsIPName ipName)) typ1 -> do
#elif MIN_VERSION_ghc(8,2,0) /* ghc-8.2 8.4 */
  HsIParamTy (L _ (HsIPName ipName)) typ1 -> do
#else                        /* ghc-8.0 */
  HsIParamTy (HsIPName ipName) typ1 -> do
#endif
    typeDoc1 <- docSharedWrapper layoutType typ1
    docAlt
      [ docSeq
        [ docWrapNodeRest ltype
        $ docLit
        $ Text.pack ("?" ++ showSDocUnsafe (ftext ipName) ++ "::")
        , docForceSingleline typeDoc1
        ]
      , docPar
          ( docLit
          $ Text.pack ("?" ++ showSDocUnsafe (ftext ipName))
          )
          (docCols ColTyOpPrefix
            [ docWrapNodeRest ltype
            $ docLit $ Text.pack ":: "
            , docAddBaseY (BrIndentSpecial 2) typeDoc1
            ])
      ]
#if MIN_VERSION_ghc(8,6,0)
#else
  HsEqTy typ1 typ2 -> do
    typeDoc1 <- docSharedWrapper layoutType typ1
    typeDoc2 <- docSharedWrapper layoutType typ2
    docAlt
      [ docSeq
        [ docForceSingleline typeDoc1
        , docWrapNodeRest ltype
        $ docLit $ Text.pack " ~ "
        , docForceSingleline typeDoc2
        ]
      , docPar
          typeDoc1
          ( docCols ColTyOpPrefix
              [ docWrapNodeRest ltype
              $ docLit $ Text.pack "~ "
              , docAddBaseY (BrIndentSpecial 2) typeDoc2
              ])
      ]
#endif
  -- TODO: test KindSig
#if MIN_VERSION_ghc(8,6,0)
  HsKindSig _ typ1 kind1 -> do
#else
  HsKindSig typ1 kind1 -> do
#endif
    typeDoc1 <- docSharedWrapper layoutType typ1
    kindDoc1 <- docSharedWrapper layoutType kind1
    hasParens <- hasAnnKeyword ltype AnnOpenP
    docAlt
      [ if hasParens
        then docSeq
          [ docLit $ Text.pack "("
          , docForceSingleline typeDoc1
          , docSeparator
          , docLit $ Text.pack "::"
          , docSeparator
          , docForceSingleline kindDoc1
          , docLit $ Text.pack ")"
          ]
        else docSeq
          [ docForceSingleline typeDoc1
          , docSeparator
          , docLit $ Text.pack "::"
          , docSeparator
          , docForceSingleline kindDoc1
          ]
      , if hasParens
        then docLines
          [ docCols
            ColTyOpPrefix
            [ docWrapNodeRest ltype $ docParenLSep
            , docAddBaseY (BrIndentSpecial 3) $ typeDoc1
            ]
          , docCols
            ColTyOpPrefix
            [ docWrapNodeRest ltype $ docLit $ Text.pack ":: "
            , docAddBaseY (BrIndentSpecial 3) kindDoc1
            ]
          , (docLit $ Text.pack ")")
          ]
        else docPar
          typeDoc1
          ( docCols
            ColTyOpPrefix
            [ docWrapNodeRest ltype $ docLit $ Text.pack ":: "
            , docAddBaseY (BrIndentSpecial 3) kindDoc1
            ]
          )
      ]
  HsBangTy{} -> -- TODO
    briDocByExactInlineOnly "HsBangTy{}" ltype
  -- HsBangTy bang typ1 -> do
  --   let bangStr = case bang of
  --         HsSrcBang _ unpackness strictness ->
  --           (++)
  --             (case unpackness of
  --               SrcUnpack   -> "{-# UNPACK -#} "
  --               SrcNoUnpack -> "{-# NOUNPACK -#} "
  --               NoSrcUnpack -> ""
  --             )
  --             (case strictness of
  --               SrcLazy     -> "~"
  --               SrcStrict   -> "!"
  --               NoSrcStrict -> ""
  --             )
  --   let bangLen = length bangStr
  --   layouter@(Layouter desc _ _) <- layoutType typ1
  --   let line = do -- Maybe
  --         l <- _ldesc_line desc
  --         let len = bangLen + _lColumns_min l
  --         return $ LayoutColumns
  --           { _lColumns_key = ColumnKeyUnique
  --           , _lColumns_lengths = [len]
  --           , _lColumns_min = len
  --           }
  --   let block = do -- Maybe
  --         rol <- descToBlockStart desc
  --         (minR,maxR) <- descToBlockMinMax desc
  --         return $ BlockDesc
  --           { _bdesc_blockStart = rol
  --           , _bdesc_min = minR
  --           , _bdesc_max = maxR
  --           , _bdesc_opIndentFloatUp = Nothing
  --           }
  --   return $ Layouter
  --     { _layouter_desc = LayoutDesc
  --       { _ldesc_line = line
  --       , _ldesc_block = block
  --       }
  --     , _layouter_func = \_params -> do
  --         remaining <- getCurRemaining
  --         case line of
  --           Just (LayoutColumns _ _ m) | m <= remaining -> do
  --             layoutWriteAppend $ Text.pack $ bangStr
  --             applyLayouterRestore layouter defaultParams
  --           _ -> do
  --             layoutWriteAppend $ Text.pack $ bangStr
  --             layoutWritePostCommentsRestore ltype
  --             applyLayouterRestore layouter defaultParams
  --     , _layouter_ast = ltype
  --     }
  HsSpliceTy{} -> -- TODO
    briDocByExactInlineOnly "HsSpliceTy{}" ltype
  HsDocTy{} -> -- TODO
    briDocByExactInlineOnly "HsDocTy{}" ltype
  HsRecTy{} -> -- TODO
    briDocByExactInlineOnly "HsRecTy{}" ltype
#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
  HsExplicitListTy _ _ typs -> do
#else /* ghc-8.0 */
  HsExplicitListTy _ typs -> do
#endif
    typDocs <- docSharedWrapper layoutType `mapM` typs
    docAlt
      [ docSeq
      $  [docLit $ Text.pack "'["]
      ++ List.intersperse docCommaSep typDocs
      ++ [docLit $ Text.pack "]"]
      -- TODO
      ]
  HsExplicitTupleTy{} -> -- TODO
    briDocByExactInlineOnly "HsExplicitTupleTy{}" ltype
#if MIN_VERSION_ghc(8,6,0)
  HsTyLit _ lit -> case lit of
#else
  HsTyLit lit -> case lit of
#endif
#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
    HsNumTy (SourceText srctext) _ -> docLit $ Text.pack srctext
    HsNumTy NoSourceText _ ->
      error "overLitValBriDoc: literal with no SourceText"
    HsStrTy (SourceText srctext) _ -> docLit $ Text.pack srctext
    HsStrTy NoSourceText _ ->
      error "overLitValBriDoc: literal with no SourceText"
#else /* ghc-8.0 */
    HsNumTy srctext _ -> docLit $ Text.pack srctext
    HsStrTy srctext _ -> docLit $ Text.pack srctext
#endif
#if !MIN_VERSION_ghc(8,6,0)
  HsCoreTy{} -> -- TODO
    briDocByExactInlineOnly "HsCoreTy{}" ltype
#endif
  HsWildCardTy _ ->
    docLit $ Text.pack "_"
#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
  HsSumTy{} -> -- TODO
    briDocByExactInlineOnly "HsSumTy{}" ltype
#endif
#if MIN_VERSION_ghc(8,6,0)
  HsStarTy _ isUnicode -> do
    if isUnicode
      then docLit $ Text.pack "\x2605" -- Unicode star
      else docLit $ Text.pack "*"
  XHsType{} -> error "brittany internal error: XHsType"
#endif
#if MIN_VERSION_ghc(8,8,0)
  HsAppKindTy _ ty kind -> do
    t <- docSharedWrapper layoutType ty
    k <- docSharedWrapper layoutType kind
    docAlt
      [ docSeq
          [ docForceSingleline t
          , docSeparator
          , docLit $ Text.pack "@"
          , docForceSingleline k
          ]
      , docPar
          t
          (docSeq [docLit $ Text.pack "@", k ])
      ]
#endif

layoutTyVarBndrs
  :: [LHsTyVarBndr GhcPs]
  -> ToBriDocM [(Text, Maybe (ToBriDocM BriDocNumbered))]
layoutTyVarBndrs = mapM $ \case
#if MIN_VERSION_ghc(8,6,0)
  (L _ (UserTyVar _ name)) -> return $ (lrdrNameToText name, Nothing)
  (L _ (KindedTyVar _ lrdrName kind)) -> do
    d <- docSharedWrapper layoutType kind
    return $ (lrdrNameToText lrdrName, Just $ d)
  (L _ (XTyVarBndr{})) -> error "brittany internal error: XTyVarBndr"
#else
  (L _ (UserTyVar name)) -> return $ (lrdrNameToText name, Nothing)
  (L _ (KindedTyVar lrdrName kind)) -> do
    d <- docSharedWrapper layoutType kind
    return $ (lrdrNameToText lrdrName, Just $ d)
#endif

-- there is no specific reason this returns a list instead of a single
-- BriDoc node.
processTyVarBndrsSingleline
  :: [(Text, Maybe (ToBriDocM BriDocNumbered))] -> [ToBriDocM BriDocNumbered]
processTyVarBndrsSingleline bndrDocs = bndrDocs >>= \case
  (tname, Nothing) -> [docSeparator, docLit tname]
  (tname, Just doc) ->
    [ docSeparator
    , docLit $ Text.pack "(" <> tname <> Text.pack " :: "
    , docForceSingleline $ doc
    , docLit $ Text.pack ")"
    ]
