{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Layouters.Type
  ( layoutType
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Config.Types
import           Language.Haskell.Brittany.Types
import           Language.Haskell.Brittany.LayoutBasics

import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           Language.Haskell.GHC.ExactPrint.Types ( mkAnnKey )
import           SrcLoc ( SrcSpan )
import           HsSyn
import           Name
import           Outputable ( ftext, showSDocUnsafe )

import           DataTreePrint



layoutType :: ToBriDoc HsType
layoutType ltype@(L _ typ) = case typ of
  -- _ | traceShow (ExactPrint.Types.mkAnnKey ltype) False -> error "impossible"
  HsTyVar name -> do
    let t = lrdrNameToText name
    return $ docWrapNode ltype $ docLit t
  HsForAllTy bndrs (L _ (HsQualTy (L _ cntxts@(_:_)) typ2)) -> do
    typeDoc <- layoutType typ2
    tyVarDocs <- bndrs `forM` \case
      (L _ (UserTyVar name)) -> return $ (lrdrNameToText name, Nothing)
      (L _ (KindedTyVar lrdrName kind)) -> do
        d <- layoutType kind
        return $ (lrdrNameToText lrdrName, Just d)
    cntxtDocs <- cntxts `forM` layoutType
    let
      tyVarDocLineList = tyVarDocs >>= \case
        (tname, Nothing) -> [BDLit $ Text.pack " " <> tname]
        (tname, Just doc) -> [ BDLit $ Text.pack " ("
                                    <> tname
                                    <> Text.pack " :: "
                             , BDForceSingleline doc
                             , BDLit $ Text.pack ")"
                             ]
      forallDoc = BDAlt
        [ let
            open = BDLit $ Text.pack "forall"
            in BDSeq ([open]++tyVarDocLineList)
        , docPar
            (BDLit (Text.pack "forall"))
            (BDLines
            $ tyVarDocs <&> \case
                (tname, Nothing) -> BDEnsureIndent BrIndentRegular $ BDLit tname
                (tname, Just doc) -> BDEnsureIndent BrIndentRegular
                  $ BDLines
                    [ BDCols ColTyOpPrefix
                      [ docParenLSep
                      , BDLit tname
                      ]
                    , BDCols ColTyOpPrefix
                      [ BDLit $ Text.pack ":: "
                      , doc
                      ]
                    , BDLit $ Text.pack ")"
                    ])
        ]
      contextDoc = case cntxtDocs of
        [x] -> x
        _ -> BDAlt
          [ let
              open  = BDLit $ Text.pack "("
              close = BDLit $ Text.pack ")"
              list  = List.intersperse docCommaSep
                    $ BDForceSingleline <$> cntxtDocs
              in BDSeq ([open]++list++[close])
          , let
              open = BDCols ColTyOpPrefix
                      [ docParenLSep
                      , BDAddBaseY (BrIndentSpecial 2) $ head cntxtDocs
                      ]
              close = BDLit $ Text.pack ")"
              list = List.tail cntxtDocs <&> \cntxtDoc ->
                     BDCols ColTyOpPrefix
                      [ docCommaSep
                      , BDAddBaseY (BrIndentSpecial 2) cntxtDoc
                      ]
            in docPar open $ BDLines $ list ++ [close]
          ]
    return $ docWrapNode ltype $ BDAlt
      -- :: forall a b c . (Foo a b c) => a b -> c
      [ BDSeq
        [ if null bndrs
            then BDEmpty
            else let
              open = BDLit $ Text.pack "forall"
              close = BDLit $ Text.pack " . "
              in BDSeq ([open]++tyVarDocLineList++[close])
        , BDForceSingleline contextDoc
        , BDLit $ Text.pack " => "
        , typeDoc
        ]
      -- :: forall a b c
      --  . (Foo a b c)
      -- => a b
      -- -> c
      , docPar
          forallDoc
          ( BDLines
            [ BDCols ColTyOpPrefix
              [ docPostComment ltype $ BDLit $ Text.pack " . "
              , BDAddBaseY (BrIndentSpecial 3)
              $ BDForceSingleline contextDoc
              ]
            , BDCols ColTyOpPrefix
              [ BDLit $ Text.pack "=> "
              , BDAddBaseY (BrIndentSpecial 3) $ BDForceMultiline typeDoc
              ]
            ]
          )
      ]
  HsForAllTy bndrs typ2 -> do
    typeDoc <- layoutType typ2
    tyVarDocs <- bndrs `forM` \case
      (L _ (UserTyVar name)) -> return $ (lrdrNameToText name, Nothing)
      (L _ (KindedTyVar lrdrName kind)) -> do
        d <- layoutType kind
        return $ (lrdrNameToText lrdrName, Just d)
    let
      tyVarDocLineList = tyVarDocs >>= \case
        (tname, Nothing) -> [BDLit $ Text.pack " " <> tname]
        (tname, Just doc) -> [ BDLit $ Text.pack " ("
                                    <> tname
                                    <> Text.pack " :: "
                             , BDForceSingleline doc
                             , BDLit $ Text.pack ")"
                             ]
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
        [ if null bndrs
            then BDEmpty
            else let
              open = BDLit $ Text.pack "forall"
              close = BDLit $ Text.pack " . "
              in BDSeq ([open]++tyVarDocLineList++[close])
        , typeDoc
        ]
      , docPar
          (BDSeq $ BDLit (Text.pack "forall") : tyVarDocLineList)
          ( BDCols ColTyOpPrefix
            [ docPostComment ltype $ BDLit $ Text.pack ". "
            , typeDoc
            ]
          )
      , docPar
          (BDLit (Text.pack "forall"))
          (BDLines
          $ (tyVarDocs <&> \case
              (tname, Nothing) -> BDEnsureIndent BrIndentRegular $ BDLit tname
              (tname, Just doc) -> BDEnsureIndent BrIndentRegular
                $ BDLines
                  [ BDCols ColTyOpPrefix
                    [ docParenLSep
                    , BDLit tname
                    ]
                  , BDCols ColTyOpPrefix
                    [ BDLit $ Text.pack ":: "
                    , doc
                    ]
                  , BDLit $ Text.pack ")"
                  ]
            )
          ++[ BDCols ColTyOpPrefix
              [ docPostComment ltype $ BDLit $ Text.pack ". "
              , typeDoc
              ]
            ]
          )
      ]
  x@(HsQualTy (L _ []) _) ->
    unknownNodeError "HsQualTy [] _" x
  HsQualTy (L _ cntxts@(_:_)) typ1 -> do
    typeDoc <- layoutType typ1
    cntxtDocs <- cntxts `forM` layoutType
    let
      contextDoc = case cntxtDocs of
        [x] -> x
        _ -> BDAlt
          [ let
              open  = BDLit $ Text.pack "("
              close = BDLit $ Text.pack ")"
              list  = List.intersperse docCommaSep
                    $ BDForceSingleline <$> cntxtDocs
              in BDSeq ([open]++list++[close])
          , let
              open = BDCols ColTyOpPrefix
                      [ docParenLSep
                      , BDAddBaseY (BrIndentSpecial 2)
                      $ head cntxtDocs
                      ]
              close = BDLit $ Text.pack ")"
              list = List.tail cntxtDocs <&> \cntxtDoc ->
                     BDCols ColTyOpPrefix
                      [ docCommaSep
                      , BDAddBaseY (BrIndentSpecial 2) 
                      $ cntxtDoc
                      ]
            in docPar open $ BDLines $ list ++ [close]
          ]
    return $ docWrapNode ltype $ BDAlt
      -- (Foo a b c) => a b -> c
      [ BDSeq
        [ BDForceSingleline contextDoc
        , BDLit $ Text.pack " => "
        , typeDoc
        ]
      --    (Foo a b c)
      -- => a b
      -- -> c
      , docPar
          (BDForceSingleline contextDoc)
          ( BDCols ColTyOpPrefix
            [ BDLit $ Text.pack "=> "
            , BDAddBaseY (BrIndentSpecial 3) $ BDForceMultiline typeDoc
            ]
          )
      ]
  -- HsQualTy (L _ cntxts) typ2 -> do
  --   layouter@(Layouter desc _ _) <- layoutType typ2
  --   cntxtLayouters <- cntxts `forM` layoutType
  --   let mLine =
  --         [ LayoutColumns ColumnKeyUnique [len] len
  --         | -- (A a, B b) =>
  --           -- 1   2    6
  --           constraintLen <- if null cntxts
  --                 then return 0
  --                 else ( sequence
  --                      $ cntxtLayouters <&> _layouter_desc .> _ldesc_line)
  --                  <&> \cols -> 5
  --                             + 2 * length cols
  --                             + sum (_lColumns_min <$> cols)
  --         , tyLen <- _lColumns_min <$> _ldesc_line desc
  --         , let len = constraintLen + tyLen
  --         ]
  --   let mBlock =
  --         [ BlockDesc
  --           { _bdesc_blockStart = AllSameIndent -- this might not be accurate,
  --                                          -- but it should simply not matter.
  --                                          -- *lazy*
  --           , _bdesc_min = minR
  --           , _bdesc_max = maxR
  --           , _bdesc_opIndentFloatUp = Nothing
  --           }
  --         | (tyMin, tyMax) <- descToMinMax 0 desc
  --         , constrMinMaxs <- sequence $ cntxtLayouters <&> _layouter_desc .> descToMinMax 0
  --         , let constrMin = constrMinMaxs <&> fst & maximum
  --         , let constrMax = constrMinMaxs <&> snd & maximum
  --         , let minR = 3 + maximum [constrMin, tyMin]
  --         , let maxR = 3 + maximum [constrMax, tyMax]
  --         ]
  --   return $ Layouter
  --     { _layouter_desc = LayoutDesc
  --       { _ldesc_line  = mLine
  --       , _ldesc_block = mBlock
  --       }
  --     , _layouter_func = \params -> do
  --         layoutWritePriorCommentsRestore ltype
  --         remaining <- getCurRemaining
  --         case mLine of
  --           Just (LayoutColumns _ _ m) | m <= remaining -> do
  --             when (not $ null cntxts) $ do
  --               layoutWriteAppend $ Text.pack "("
  --               sequence_ $ intersperse (layoutWriteAppend $ Text.pack ", ")
  --                         $ cntxtLayouters <&> \lay -> applyLayouterRestore lay defaultParams
  --               layoutWriteAppend $ Text.pack ") => "
  --             applyLayouterRestore layouter defaultParams
  --           _ -> do
  --             if null cntxts
  --               then do
  --                 layoutWriteAppend $ Text.pack "()"
  --               else do
  --               layoutWithNonParamIndent params $ do
  --                 layoutWriteAppend $ Text.pack "( "
  --                 let iAct = do
  --                       layoutWriteNewline
  --                       layoutWriteAppend $ Text.pack ", "
  --                 sequence_ $ intersperse iAct
  --                           $ cntxtLayouters <&> \lay -> applyLayouter lay defaultParams
  --                 layoutWriteNewline
  --                 layoutWriteAppend $ Text.pack ")"
  --               layoutWriteNewline
  --               layoutWriteAppend $ Text.pack "=> "
  --             applyLayouterRestore layouter defaultParams 
  --               { _params_opIndent = _params_opIndent params
  --               }
  --     , _layouter_ast = ltype
  --     }
  HsFunTy typ1 typ2 -> do
    typeDoc1 <- layoutType typ1
    typeDoc2 <- layoutType typ2
    let shouldForceML = case typ2 of
          (L _ HsFunTy{}) -> True
          _ -> False
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
        [ BDForceSingleline typeDoc1
        , docPostComment ltype $ appSep $ BDLit $ Text.pack " ->"
        , BDForceSingleline typeDoc2
        ]
      , docPar
        typeDoc1
        ( BDCols ColTyOpPrefix
          [ docPostComment ltype $ appSep $ BDLit $ Text.pack "->"
          , BDAddBaseY (BrIndentSpecial 3)
          $ if shouldForceML then BDForceMultiline typeDoc2
                             else typeDoc2
          ]
        )
      ]
  HsParTy typ1 -> do
    typeDoc1 <- layoutType typ1
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
        [ docPostComment ltype $ BDLit $ Text.pack "("
        , BDForceSingleline typeDoc1
        , BDLit $ Text.pack ")"
        ]
      , docPar
          ( BDCols ColTyOpPrefix
            [ docPostComment ltype $ docParenLSep
            , BDAddBaseY (BrIndentSpecial 2) $ typeDoc1
            ])
          (BDLit $ Text.pack ")")
      ]
  HsAppTy typ1 typ2 -> do
    typeDoc1 <- layoutType typ1
    typeDoc2 <- layoutType typ2
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
        [ BDForceSingleline typeDoc1
        , BDLit $ Text.pack " "
        , BDForceSingleline typeDoc2
        ]
      , docPar
          typeDoc1
          (BDEnsureIndent BrIndentRegular typeDoc2)
      ]
  HsAppsTy [] -> error "HsAppsTy []"
  HsAppsTy [L _ (HsAppPrefix typ1)] -> do
    typeDoc1 <- layoutType typ1
    return $ docWrapNode ltype $ typeDoc1
  HsAppsTy [L l (HsAppInfix name)] -> do
    -- this redirection is somewhat hacky, but whatever.
    -- TODO: a general problem when doing deep inspections on
    --       the type (and this is not the only instance)
    --       is that we potentially omit annotations on some of
    --       the middle constructors. i have no idea under which
    --       circumstances exactly important annotations (comments)
    --       would be assigned to such constructors.
    typeDoc1 <- layoutType $ (L l $ HsTyVar name)
    return $ docWrapNode ltype $ typeDoc1
  HsAppsTy (L _ (HsAppPrefix typHead):typRestA)
    | Just typRest <- mapM (\case L _ (HsAppPrefix t) -> Just t
                                  _ -> Nothing) typRestA -> do
    docHead <- layoutType typHead
    docRest <- mapM layoutType typRest
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
      $ BDForceSingleline docHead : (docRest >>= \d ->
        [ BDLit $ Text.pack " ", BDForceSingleline d ])
      , docPar docHead (BDLines $ BDEnsureIndent BrIndentRegular <$> docRest)
      ]
  HsAppsTy (typHead:typRest) -> do
    docHead <- layoutAppType typHead
    docRest <- mapM layoutAppType typRest
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
      $ BDForceSingleline docHead : (docRest >>= \d ->
        [ BDLit $ Text.pack " ", BDForceSingleline d ])
      , docPar docHead (BDLines $ BDEnsureIndent BrIndentRegular <$> docRest)
      ]
    where
      layoutAppType (L _ (HsAppPrefix t)) = layoutType t
      layoutAppType (L _ (HsAppInfix t))  = BDLit <$> lrdrNameToTextAnn t
  HsListTy typ1 -> do
    typeDoc1 <- layoutType typ1
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
        [ docPostComment ltype $ BDLit $ Text.pack "["
        , BDForceSingleline typeDoc1
        , BDLit $ Text.pack "]"
        ]
      , docPar
          ( BDCols ColTyOpPrefix
            [ docPostComment ltype $ BDLit $ Text.pack "[ "
            , BDAddBaseY (BrIndentSpecial 2) $ typeDoc1
            ])
          (BDLit $ Text.pack "]")
      ]
  HsPArrTy typ1 -> do
    typeDoc1 <- layoutType typ1
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
        [ docPostComment ltype $ BDLit $ Text.pack "[:"
        , BDForceSingleline typeDoc1
        , BDLit $ Text.pack ":]"
        ]
      , docPar
          ( BDCols ColTyOpPrefix
            [ docPostComment ltype $ BDLit $ Text.pack "[:"
            , BDAddBaseY (BrIndentSpecial 2) $ typeDoc1
            ])
          (BDLit $ Text.pack ":]")
      ]
  HsTupleTy tupleSort typs -> docWrapNode ltype <$> case tupleSort of
    HsUnboxedTuple           -> unboxed
    HsBoxedTuple             -> simple
    HsConstraintTuple        -> simple
    HsBoxedOrConstraintTuple -> simple
   where
    unboxed = if null typs then error "unboxed unit?" else unboxedL
    simple = if null typs then unitL else simpleL
    unitL = return $ BDLit $ Text.pack "()"
    simpleL = do
      docs <- mapM layoutType typs
      return $ BDAlt
        [ BDSeq $ [BDLit $ Text.pack "("]
               ++ List.intersperse docCommaSep docs
               ++ [BDLit $ Text.pack ")"]
        , let
            start = BDCols ColTyOpPrefix [docParenLSep, head docs]
            lines = List.tail docs <&> \d ->
                    BDCols ColTyOpPrefix [docCommaSep, d]
            end   = BDLit $ Text.pack ")"
          in docPar
            (BDAddBaseY (BrIndentSpecial 2) $ start)
            (BDLines $ (BDAddBaseY (BrIndentSpecial 2) <$> lines) ++ [end])
        ]
    unboxedL = do
      docs <- mapM layoutType typs
      return $ BDAlt
        [ BDSeq $ [BDLit $ Text.pack "(#"]
               ++ List.intersperse docCommaSep docs
               ++ [BDLit $ Text.pack "#)"]
        , let
            start = BDCols ColTyOpPrefix [BDLit $ Text.pack "(#", head docs]
            lines = List.tail docs <&> \d ->
                    BDCols ColTyOpPrefix [docCommaSep, d]
            end   = BDLit $ Text.pack "#)"
          in docPar
            (BDAddBaseY (BrIndentSpecial 2) start)
            (BDLines $ (BDAddBaseY (BrIndentSpecial 2) <$> lines) ++ [end])
        ]
  HsOpTy{} -> -- TODO
    briDocByExact ltype
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
  HsIParamTy (HsIPName ipName) typ1 -> do
    typeDoc1 <- layoutType typ1
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
        [ docPostComment ltype
        $ BDLit
        $ Text.pack ("?" ++ showSDocUnsafe (ftext ipName) ++ "::")
        , BDForceSingleline typeDoc1
        ]
      , docPar
          ( BDLit
          $ Text.pack ("?" ++ showSDocUnsafe (ftext ipName))
          )
          (BDCols ColTyOpPrefix
            [ docPostComment ltype
            $ BDLit $ Text.pack "::"
            , BDAddBaseY (BrIndentSpecial 2) typeDoc1
            ])
      ]
  HsEqTy typ1 typ2 -> do
    typeDoc1 <- layoutType typ1
    typeDoc2 <- layoutType typ2
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
        [ BDForceSingleline typeDoc1
        , docPostComment ltype
        $ BDLit $ Text.pack " ~ "
        , BDForceSingleline typeDoc2
        ]
      , docPar
          typeDoc1
          ( BDCols ColTyOpPrefix
              [ docPostComment ltype
              $ BDLit $ Text.pack "~ "
              , BDAddBaseY (BrIndentSpecial 2) typeDoc2
              ])
      ]
  -- TODO: test KindSig
  HsKindSig typ1 kind1 -> do
    typeDoc1 <- layoutType typ1
    kindDoc1 <- layoutType kind1
    return $ docWrapNode ltype $ BDAlt
      [ BDSeq
        [ BDForceSingleline typeDoc1
        , BDLit $ Text.pack " :: "
        , BDForceSingleline kindDoc1
        ]
      , docPar
          typeDoc1
          ( BDCols ColTyOpPrefix
              [ docPostComment ltype
              $ BDLit $ Text.pack ":: "
              , BDAddBaseY (BrIndentSpecial 3) kindDoc1
              ])
      ]
  HsBangTy{} -> -- TODO
    briDocByExact ltype
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
    briDocByExact ltype
  HsDocTy{} -> -- TODO
    briDocByExact ltype
  HsRecTy{} -> -- TODO
    briDocByExact ltype
  HsExplicitListTy _ typs -> do
    typDocs <- typs `forM` layoutType
    return $ BDAlt
      [ BDSeq
      $  [BDLit $ Text.pack "'["]
      ++ List.intersperse docCommaSep typDocs
      ++ [BDLit $ Text.pack "]"]
      -- TODO
      ]
  HsExplicitTupleTy{} -> -- TODO
    briDocByExact ltype
  HsTyLit{} -> -- TODO
    briDocByExact ltype
  HsCoreTy{} -> -- TODO
    briDocByExact ltype
  HsWildCardTy{} -> -- TODO
    briDocByExact ltype
