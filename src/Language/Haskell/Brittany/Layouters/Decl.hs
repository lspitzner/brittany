{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Language.Haskell.Brittany.Layouters.Decl
  ( layoutSig
  , layoutBind
  , layoutLocalBinds
  , layoutGuardLStmt
  , layoutPatternBind
  , layoutGrhs
  , layoutPatternBindFinal
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Types
import           Language.Haskell.Brittany.LayoutBasics

import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           SrcLoc ( SrcSpan )
import           HsSyn
import           Name
import           Language.Haskell.GHC.ExactPrint.Types ( mkAnnKey )

import           Language.Haskell.Brittany.Layouters.Type
import {-# SOURCE #-} Language.Haskell.Brittany.Layouters.Expr
import {-# SOURCE #-} Language.Haskell.Brittany.Layouters.Stmt
import           Language.Haskell.Brittany.Layouters.Pattern

import           Bag ( mapBagM )



layoutSig :: ToBriDoc Sig
layoutSig lsig@(L _loc sig) = docWrapNode lsig $ case sig of
  TypeSig names (HsIB _ (HsWC _ _ typ)) -> do
    nameStrs <- names `forM` lrdrNameToTextAnn
    let nameStr = Text.intercalate (Text.pack ", ") $ nameStrs
    typeDoc <- docSharedWrapper layoutType typ
    docAlt
      [ docSeq
        [ docPostComment lsig $ docLit nameStr
        , docLit $ Text.pack " :: "
        , docForceSingleline typeDoc
        ]
      , docAddBaseY BrIndentRegular
      $ docPar
        (docPostComment lsig $ docLit nameStr)
        ( docCols ColTyOpPrefix
          [ docLit $ Text.pack ":: "
          , docAddBaseY (BrIndentSpecial 3) $ typeDoc
          ]
        )
      ]
  _ -> briDocByExact lsig -- TODO: should not be necessary

layoutGuardLStmt :: ToBriDoc' (Stmt RdrName (LHsExpr RdrName))
layoutGuardLStmt lgstmt@(L _ stmtLR) = docWrapNode lgstmt $ case stmtLR of
  BodyStmt body _ _ _ -> layoutExpr body
  BindStmt lPat expr _ _ _ -> do
    patDoc <- docSharedWrapper layoutPat lPat
    expDoc <- docSharedWrapper layoutExpr expr
    docCols ColBindStmt
      [patDoc, docSeq [docLit $ Text.pack " <- ", expDoc]]
  _ -> briDocByExact lgstmt -- TODO

layoutBind :: ToBriDocC (HsBindLR RdrName RdrName) (Either [BriDocNumbered] BriDocNumbered)
layoutBind lbind@(L _ bind) = case bind of
  FunBind fId (MG lmatches@(L _ matches) _ _ _) _ _ [] -> do
    -- funcPatDocs :: [BriDocNumbered] <- matches `forM` \(L _ match@(Match _
    --                                           pats
    --                                           _mType -- not an actual type sig
    --                                           (GRHSs grhss whereBinds))) -> do
    --   let isInfix = isInfixMatch match
    --   let mId     = fId
    --   idStr <- lrdrNameToTextAnn mId
    --   patDocs <- docSharedWrapper layoutPat `mapM` pats
    --   let funcPatternPartLine = case patDocs of
    --         (p1:pr) | isInfix -> docCols ColFuncPatternsInfix
    --           ( [ appSep $ docForceSingleline p1
    --             , appSep $ docLit idStr
    --             ]
    --           ++ (pr <&> (\p -> appSep $ docForceSingleline p))
    --           )
    --         ps -> docCols ColFuncPatternsPrefix
    --           $ appSep (docLit $ idStr)
    --           : (ps <&> (\p -> docSeq [docForceSingleline p, docSeparator]))
    --   grhssDocsNoInd :: ToBriDocM BriDocNumbered <- do
    --     case grhss of
    --       [grhs1] -> _ grhs1
    --       (grhs1:grhsr) -> do
    --         grhsDoc1 <- _ grhs1
    --         grhsDocr <- _ grhsr
    --         return $ docLines $ grhsDoc1 : grhsDocr
    --       [] -> error "layoutBind grhssDocsNoInd"
    --   let grhssDocs = docAlt [grhssDocsNoInd {-, grhssDocsInd TODO-}]
    --   layoutLocalBinds whereBinds >>= \case
    --     Nothing -> grhssDocs
    --     Just whereDocs -> do
    --       let defaultWhereDocs = docPar grhssDocs 
    --                            $ docEnsureIndent BrIndentRegular
    --                            $ docAddBaseY BrIndentRegular
    --                            $ docPar (docLit $ Text.pack "where")
    --                            $ docSetIndentLevel $ docLines $ return <$> whereDocs
    --       case whereDocs of
    --         [wd] -> docAlt
    --           [ docSeq [ appSep $ docForceSingleline grhssDocs
    --                    , appSep $ docLit $ Text.pack "where"
    --                    , docForceSingleline $ return wd
    --                    ]
    --           , defaultWhereDocs
    --           ]
    --         _ -> defaultWhereDocs
    idStr <- lrdrNameToTextAnn fId
    binderDoc <- docLit $ Text.pack "="
    funcPatDocs <- docWrapNode lbind $ docWrapNode lmatches $ layoutPatternBind (Just idStr) binderDoc `mapM` matches
    return $ Left $ funcPatDocs
  PatBind pat (GRHSs grhss whereBinds) _ _ ([], []) -> do
    patDoc <- layoutPat pat
    clauseDocs <- layoutGrhs `mapM` grhss
    mWhereDocs <- layoutLocalBinds whereBinds
    binderDoc <- docLit $ Text.pack "="
    fmap Right $ docWrapNode lbind $ layoutPatternBindFinal binderDoc (Just patDoc) clauseDocs mWhereDocs
    -- grhssDocsNoInd <- do
    --   case grhss of
    --     [grhs1] -> docSharedWrapper (layoutGrhs (Just $ appSep patDoc)) grhs1
    --     (grhs1:grhsr) -> do
    --       grhsDoc1 <- docSharedWrapper (layoutGrhs (Just $ appSep patDoc)) grhs1
    --       grhsDocr <- docSharedWrapper (layoutGrhs Nothing) `mapM` grhsr
    --       return $ docLines $ grhsDoc1 : grhsDocr
    --     [] -> error "layoutBind grhssDocsNoInd"
    -- let grhssDocs = docAlt [grhssDocsNoInd {-, grhssDocsInd TODO-}]
    -- case mWhereDocs of
    --   Nothing ->
    --     Right <$> grhssDocs
    --   Just whereDocs -> do
    --     let defaultWhereDocs = docAddBaseY BrIndentRegular
    --                          $ docPar grhssDocs 
    --                          $ docAddBaseY BrIndentRegular
    --                          $ docPar (docLit $ Text.pack "where")
    --                          $ docSetIndentLevel $ docLines $ return <$> whereDocs
    --     Right <$> case whereDocs of
    --       [wd] -> docAlt
    --         [ docSeq [ appSep $ docForceSingleline grhssDocs
    --                  , appSep $ docLit $ Text.pack "where"
    --                  , docForceSingleline $ return wd
    --                  ]
    --         , defaultWhereDocs
    --         ]
    --       _ -> defaultWhereDocs
  _ -> Right <$> briDocByExact lbind

data BagBindOrSig = BagBind (LHsBindLR RdrName RdrName)
                  | BagSig (LSig RdrName)

bindOrSigtoSrcSpan :: BagBindOrSig -> SrcSpan
bindOrSigtoSrcSpan (BagBind (L l _)) = l
bindOrSigtoSrcSpan (BagSig  (L l _)) = l

layoutLocalBinds :: ToBriDocC (HsLocalBindsLR RdrName RdrName) (Maybe [BriDocNumbered])
layoutLocalBinds lbinds@(L _ binds) = case binds of
  -- HsValBinds (ValBindsIn lhsBindsLR []) ->
  --   Just . (>>= either id return) . Data.Foldable.toList <$> mapBagM layoutBind lhsBindsLR -- TODO: fix ordering
  -- x@(HsValBinds (ValBindsIn{})) ->
  --   Just . (:[]) <$> unknownNodeError "HsValBinds (ValBindsIn _ (_:_))" x
  HsValBinds (ValBindsIn bindlrs sigs) -> do
    let unordered = [BagBind b | b <- Data.Foldable.toList bindlrs] ++ [BagSig s | s <- sigs]
        ordered = sortBy (comparing bindOrSigtoSrcSpan) unordered
    docs <- docWrapNode lbinds $ join <$> ordered `forM` \case
          BagBind b -> either id return <$> layoutBind b
          BagSig s  -> return <$> layoutSig s
    return $ Just $ docs
  x@(HsValBinds (ValBindsOut _binds _lsigs)) ->
    -- i _think_ this case never occurs in non-processed ast
    Just . (:[]) <$> unknownNodeError "HsValBinds ValBindsOut{}" x
  x@(HsIPBinds _ipBinds) ->
    Just . (:[]) <$> unknownNodeError "HsIPBinds" x
  EmptyLocalBinds     ->
    return $ Nothing

layoutGrhs :: LGRHS RdrName (LHsExpr RdrName) -> ToBriDocM ([BriDocNumbered], BriDocNumbered, LHsExpr RdrName)
layoutGrhs lgrhs@(L _ (GRHS guards body))
  = do
    guardDocs <- docWrapNode lgrhs $ layoutStmt `mapM` guards
    bodyDoc <- layoutExpr body
    return (guardDocs, bodyDoc, body)

layoutPatternBind :: Maybe Text -> BriDocNumbered -> LMatch RdrName (LHsExpr RdrName) -> ToBriDocM BriDocNumbered
layoutPatternBind mIdStr binderDoc lmatch@(L _ match@(Match _ pats _ (GRHSs grhss whereBinds)))
  = do
    patDocs <- docSharedWrapper layoutPat `mapM` pats
    let isInfix = isInfixMatch match
    patDoc <- docWrapNodePrior lmatch $ case (mIdStr, patDocs) of
      (Just idStr, p1:pr) | isInfix -> docCols ColPatternsFuncInfix
        ( [ appSep $ docForceSingleline p1
          , appSep $ docLit idStr
          ]
        ++ (spacifyDocs $ docForceSingleline <$> pr)
        )
      (Just idStr, []) -> docLit idStr
      (Just idStr, ps) -> docCols ColPatternsFuncPrefix
        $ appSep (docLit $ idStr)
        : (spacifyDocs $ docForceSingleline <$> ps)
      (Nothing, ps) -> docCols ColPatterns
        $ (List.intersperse docSeparator $ docForceSingleline <$> ps)
    clauseDocs <- docWrapNodePost lmatch $ layoutGrhs `mapM` grhss
    mWhereDocs <- layoutLocalBinds whereBinds
    layoutPatternBindFinal binderDoc (Just patDoc) clauseDocs mWhereDocs

layoutPatternBindFinal :: BriDocNumbered -> Maybe BriDocNumbered -> [([BriDocNumbered], BriDocNumbered, LHsExpr RdrName)] -> Maybe [BriDocNumbered] -> ToBriDocM BriDocNumbered
layoutPatternBindFinal binderDoc mPatDoc clauseDocs mWhereDocs = do
  let patPartInline = case mPatDoc of
        Nothing -> []
        Just patDoc -> [appSep $ docForceSingleline $ return patDoc]
      patPartParWrap = case mPatDoc of
        Nothing -> id
        Just patDoc -> docPar (return patDoc)
  docAlt $
    -- one-line solution
    [ docCols ColBindingLine
      [ docSeq
        (patPartInline ++ [guardPart])
      , docSeq
        [ appSep $ return binderDoc
        , lineMod $ return body
        , wherePart
        ]
      ]
    | [(guards, body, bodyRaw)] <- [clauseDocs]
    , let lineMod = case mWhereDocs of
            Nothing | isExpressionTypeHeadPar bodyRaw ->
              docAddBaseY BrIndentRegular
            _ -> docForceSingleline
    , let guardPart = case guards of
            [] -> docEmpty
            [g] -> docSeq [appSep $ docLit $ Text.pack "|", return g, docSeparator]
            gs -> docSeq
              $ [appSep $ docLit $ Text.pack "|"]
              ++ List.intersperse docCommaSep (return <$> gs)
              ++ [docSeparator]
    , wherePart <- case mWhereDocs of
        Nothing -> pure docEmpty
        Just [w] -> pure $ docSeq
          [ docSeparator
          , appSep $ docLit $ Text.pack "where"
          , docSetBaseY $ docSetIndentLevel $ return w
          ]
        _ -> []
    ] ++
    -- pattern and exactly one clause in single line, body and where
    -- indented if necessary.
    [ docAddBaseY BrIndentRegular
    $ docPar
      ( docCols ColBindingLine
        [ docSeq
          (patPartInline ++ [appSep guardPart])
        , docSeq
          [ appSep $ return binderDoc
          , lineMod $ docAddBaseY BrIndentRegular $ return body
          ]
        ])
      wherePart
    | [(guards, body, bodyRaw)] <- [clauseDocs]
    , let lineMod = case () of
            _ | isExpressionTypeHeadPar bodyRaw -> id
            _ -> docForceSingleline
    , let guardPart = case guards of
            [] -> docEmpty
            [g] -> docSeq [appSep $ docLit $ Text.pack "|", return g]
            gs -> docSeq
              $ [appSep $ docLit $ Text.pack "|"]
              ++ List.intersperse docCommaSep (return <$> gs)
    , wherePart <- case mWhereDocs of
        Nothing -> []
        Just ws -> pure $ docAddBaseY BrIndentRegular $ docPar
          (docLit $ Text.pack "where")
          (docSetIndentLevel $ docLines $ return <$> ws)
    ] ++
    -- pattern and exactly one clause in single line, body in new line.
    [ docAddBaseY BrIndentRegular
    $ docPar
      (docSeq (patPartInline ++ [appSep $ guardPart, return binderDoc]))
      (docLines $ [ return body ] ++ wherePart)
    | [(guards, body, _)] <- [clauseDocs]
    , let guardPart = case guards of
            [] -> docEmpty
            [g] -> docSeq [appSep $ docLit $ Text.pack "|", return g]
            gs -> docSeq
              $ [appSep $ docLit $ Text.pack "|"]
              ++ List.intersperse docCommaSep (return <$> gs)
    , let wherePart = case mWhereDocs of
            Nothing -> []
            Just ws -> pure $ docAddBaseY BrIndentRegular $ docPar
              (docLit $ Text.pack "where")
              (docSetIndentLevel $ docLines $ return <$> ws)
    ] ++
    [ docAddBaseY BrIndentRegular
    $ patPartParWrap
    $ docLines $
        (clauseDocs >>= \(guardDocs, bodyDoc, _) ->
          (case guardDocs of
            [] -> []
            [g] -> [docSeq [appSep $ docLit $ Text.pack "|", return g]]
            (g1:gr) ->
              ( docSeq [appSep $ docLit $ Text.pack "|", return g1]
              : ( gr <&> \g ->
                  docSeq [appSep $ docLit $ Text.pack ",", return g]
                )
              )
          ) ++
          [docCols ColOpPrefix
            [ appSep $ return binderDoc
            , docAddBaseY BrIndentRegular $ return bodyDoc]
            ]
        ) ++
        (case mWhereDocs of
          Nothing -> []
          Just whereDocs ->
            [ docAddBaseY BrIndentRegular
            $ docPar (docLit $ Text.pack "where")
            $ docSetIndentLevel $ docLines (return <$> whereDocs)
            ]
        )
    ]

-- layoutBind :: LayouterFType' (HsBindLR RdrName RdrName)
-- layoutBind lbind@(L _ bind) = case bind of
-- #if MIN_VERSION_ghc(8,0,0)
--   FunBind fId (MG (L _ matches) _ _ _) _ _ [] -> do
-- #else
--   FunBind fId fInfix (MG matches _ _ _) _ _ [] -> do
-- #endif
--     return $ Layouter
--       { _layouter_desc = LayoutDesc
--         { _ldesc_line  = Nothing -- no parent
--         , _ldesc_block = Nothing -- no parent
--         }
--       , _layouter_func = \_params -> do
--           layoutWritePriorCommentsRestore lbind
--           moveToExactAnn lbind
--           -- remaining <- getCurRemaining
-- #if MIN_VERSION_ghc(8,0,0)
--           matches `forM_` \(L _ match@(Match _
--                                              pats
--                                              mType
--                                              (GRHSs grhss (L _ whereBinds)))) -> do
--             let isInfix = isInfixMatch match
--             let mId     = fId
-- #else
--           matches `forM_` \(L _ (Match mIdInfix
--                                        pats
--                                        mType
--                                        (GRHSs grhss whereBinds))) -> do
--             let isInfix = maybe fInfix snd mIdInfix
--             let mId     = maybe fId fst mIdInfix
-- #endif
--             idStr <- lrdrNameToTextAnn mId
--             patLays <- pats `forM` \p -> layouterFToLayouterM $ layoutPat p
--             case patLays of
--               (p1:pr) | isInfix -> do
--                 applyLayouter p1 defaultParams
--                 layoutWriteAppend $ (Text.pack " ") <> idStr
--                 pr `forM_` \p -> do
--                   layoutWriteAppend $ Text.pack " "
--                   applyLayouter p defaultParams
--               ps                -> do
--                 layoutWriteAppend $ idStr
--                 ps `forM_` \p -> do
--                   layoutWriteAppend $ Text.pack " "
--                   applyLayouter p defaultParams
--             case mType of
--               Nothing -> return ()
--               Just t  -> do
--                 tLay <- layouterFToLayouterM $ layoutType t
--                 layoutWriteAppend $ Text.pack " :: "
--                 applyLayouter tLay defaultParams
--             grhss `forM_` \case
--               L _ (GRHS [] body) -> do
--                 layoutWriteAppend $ Text.pack " = "
--                 l <- layouterFToLayouterM $ layoutExpr body
--                 layoutWithAddIndent $ do
--                   applyLayouter l defaultParams
--               grhs -> do
--                 l <- layoutByExact grhs
--                 applyLayouter l defaultParams
--             case whereBinds of
--               HsValBinds valBinds -> undefined valBinds -- TODO
--               HsIPBinds ipBinds   -> undefined ipBinds -- TODO
--               EmptyLocalBinds     -> return ()
--           layoutWritePostCommentsRestore lbind
--       , _layouter_ast = lbind
--       }
--   _ -> layoutByExact lbind
