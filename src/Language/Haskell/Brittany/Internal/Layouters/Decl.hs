{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Language.Haskell.Brittany.Internal.Layouters.Decl
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

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Config.Types

import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           SrcLoc ( SrcSpan )
import           HsSyn
import           Name
import           BasicTypes ( InlinePragma(..)
                            , Activation(..)
                            , InlineSpec(..)
                            , RuleMatchInfo(..)
                            )
import           Language.Haskell.GHC.ExactPrint.Types ( mkAnnKey )

import           Language.Haskell.Brittany.Internal.Layouters.Type
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Stmt
import           Language.Haskell.Brittany.Internal.Layouters.Pattern

import           Bag ( mapBagM )



layoutSig :: ToBriDoc Sig
layoutSig lsig@(L _loc sig) = case sig of
#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
  TypeSig names (HsWC _ (HsIB _ typ _)) -> docWrapNode lsig $ do
#else /* ghc-8.0 */
  TypeSig names (HsIB _ (HsWC _ _ typ)) -> docWrapNode lsig $ do
#endif
    nameStrs <- names `forM` lrdrNameToTextAnn
    let nameStr = Text.intercalate (Text.pack ", ") $ nameStrs
    typeDoc     <- docSharedWrapper layoutType typ
    hasComments <- hasAnyCommentsBelow lsig
    shouldBeHanging <- mAsk
      <&> _conf_layout
      .>  _lconfig_hangingTypeSignature
      .>  confUnpack
    if shouldBeHanging
      then docSeq
        [ appSep $ docWrapNodeRest lsig $ docLit nameStr
        , docSetBaseY $ docLines
          [ docCols
            ColTyOpPrefix
            [ docLit $ Text.pack ":: "
            , docAddBaseY (BrIndentSpecial 3) $ typeDoc
            ]
          ]
        ]
      else
        docAlt
          $  [ docSeq
               [ appSep $ docWrapNodeRest lsig $ docLit nameStr
               , appSep $ docLit $ Text.pack "::"
               , docForceSingleline typeDoc
               ]
             | not hasComments
             ]
          ++ [ docAddBaseY BrIndentRegular $ docPar
               (docWrapNodeRest lsig $ docLit nameStr)
               ( docCols
                 ColTyOpPrefix
                 [ docLit $ Text.pack ":: "
                 , docAddBaseY (BrIndentSpecial 3) $ typeDoc
                 ]
               )
             ]
  InlineSig name (InlinePragma _ spec _arity phaseAct conlike) ->
    docWrapNode lsig $ do
      nameStr <- lrdrNameToTextAnn name
      let specStr = case spec of
            Inline          -> "INLINE "
            Inlinable       -> "INLINABLE "
            NoInline        -> "NOINLINE "
            EmptyInlineSpec -> "" -- i have no idea if this is correct.
      let phaseStr = case phaseAct of
            NeverActive      -> "" -- not [] - for NOINLINE NeverActive is
                                   -- in fact the default
            AlwaysActive     -> ""
            ActiveBefore _ i -> "[~" ++ show i ++ "] "
            ActiveAfter  _ i -> "[" ++ show i ++ "] "
      let conlikeStr = case conlike of
            FunLike -> ""
            ConLike -> "CONLIKE "
      docLit
        $  Text.pack ("{-# " ++ specStr ++ conlikeStr ++ phaseStr)
        <> nameStr
        <> Text.pack " #-}"
  _ -> briDocByExactNoComment lsig -- TODO

layoutGuardLStmt :: ToBriDoc' (Stmt RdrName (LHsExpr RdrName))
layoutGuardLStmt lgstmt@(L _ stmtLR) = docWrapNode lgstmt $ case stmtLR of
  BodyStmt body _ _ _      -> layoutExpr body
  BindStmt lPat expr _ _ _ -> do
    patDoc <- docSharedWrapper layoutPat lPat
    expDoc <- docSharedWrapper layoutExpr expr
    docCols ColBindStmt
            [ appSep $ colsWrapPat =<< patDoc
            , docSeq [appSep $ docLit $ Text.pack "<-", expDoc]
            ]
  _                        -> unknownNodeError "" lgstmt -- TODO

layoutBind
  :: ToBriDocC
       (HsBindLR RdrName RdrName)
       (Either [BriDocNumbered] BriDocNumbered)
layoutBind lbind@(L _ bind) = case bind of
  FunBind fId (MG lmatches@(L _ matches) _ _ _) _ _ [] -> do
    idStr       <- lrdrNameToTextAnn fId
    binderDoc   <- docLit $ Text.pack "="
    funcPatDocs <-
      docWrapNode lbind
        $      docWrapNode lmatches
        $      layoutPatternBind (Just idStr) binderDoc
        `mapM` matches
    return $ Left $ funcPatDocs
  PatBind pat (GRHSs grhss whereBinds) _ _ ([], []) -> do
    patDocs    <- colsWrapPat =<< layoutPat pat
    clauseDocs <- layoutGrhs `mapM` grhss
    mWhereDocs <- layoutLocalBinds whereBinds
    binderDoc  <- docLit $ Text.pack "="
    hasComments <- hasAnyCommentsBelow lbind
    fmap Right $ docWrapNode lbind $ layoutPatternBindFinal Nothing
                                                            binderDoc
                                                            (Just patDocs)
                                                            clauseDocs
                                                            mWhereDocs
                                                            hasComments
  _ -> Right <$> unknownNodeError "" lbind

data BagBindOrSig = BagBind (LHsBindLR RdrName RdrName)
                  | BagSig (LSig RdrName)

bindOrSigtoSrcSpan :: BagBindOrSig -> SrcSpan
bindOrSigtoSrcSpan (BagBind (L l _)) = l
bindOrSigtoSrcSpan (BagSig  (L l _)) = l

layoutLocalBinds
  :: ToBriDocC (HsLocalBindsLR RdrName RdrName) (Maybe [BriDocNumbered])
layoutLocalBinds lbinds@(L _ binds) = case binds of
  -- HsValBinds (ValBindsIn lhsBindsLR []) ->
  --   Just . (>>= either id return) . Data.Foldable.toList <$> mapBagM layoutBind lhsBindsLR -- TODO: fix ordering
  -- x@(HsValBinds (ValBindsIn{})) ->
  --   Just . (:[]) <$> unknownNodeError "HsValBinds (ValBindsIn _ (_:_))" x
  HsValBinds (ValBindsIn bindlrs sigs) -> do
    let
      unordered
        =  [ BagBind b | b <- Data.Foldable.toList bindlrs ]
        ++ [ BagSig s | s <- sigs ]
      ordered = sortBy (comparing bindOrSigtoSrcSpan) unordered
    docs <- docWrapNode lbinds $ join <$> ordered `forM` \case
      BagBind b -> either id return <$> layoutBind b
      BagSig  s -> return <$> layoutSig s
    return $ Just $ docs
  x@(HsValBinds (ValBindsOut _binds _lsigs)) ->
    -- i _think_ this case never occurs in non-processed ast
    Just . (:[]) <$> unknownNodeError "HsValBinds ValBindsOut{}" x
  x@(HsIPBinds _ipBinds) -> Just . (:[]) <$> unknownNodeError "HsIPBinds" x
  EmptyLocalBinds        -> return $ Nothing

-- TODO: we don't need the `LHsExpr RdrName` anymore, now that there is
-- parSpacing stuff.B
layoutGrhs
  :: LGRHS RdrName (LHsExpr RdrName)
  -> ToBriDocM ([BriDocNumbered], BriDocNumbered, LHsExpr RdrName)
layoutGrhs lgrhs@(L _ (GRHS guards body)) = do
  guardDocs <- docWrapNode lgrhs $ layoutStmt `mapM` guards
  bodyDoc   <- layoutExpr body
  return (guardDocs, bodyDoc, body)

layoutPatternBind
  :: Maybe Text
  -> BriDocNumbered
  -> LMatch RdrName (LHsExpr RdrName)
  -> ToBriDocM BriDocNumbered
layoutPatternBind mIdStr binderDoc lmatch@(L _ match@(Match fixityOrCtx pats _ (GRHSs grhss whereBinds))) = do
  patDocs <- pats `forM` \p -> fmap return $ colsWrapPat =<< layoutPat p
  let isInfix = isInfixMatch match
  let mIdStr' = fixPatternBindIdentifier fixityOrCtx <$> mIdStr
  patDoc <- docWrapNodePrior lmatch $ case (mIdStr', patDocs) of
    (Just idStr, p1 : pr) | isInfix -> docCols
      ColPatternsFuncInfix
      (  [appSep $ docForceSingleline p1, appSep $ docLit idStr]
      ++ (spacifyDocs $ docForceSingleline <$> pr)
      )
    (Just idStr, []) -> docLit idStr
    (Just idStr, ps) ->
      docCols ColPatternsFuncPrefix
        $ appSep (docLit $ idStr)
        : (spacifyDocs $ docForceSingleline <$> ps)
    (Nothing, ps) ->
      docCols ColPatterns
        $ (List.intersperse docSeparator $ docForceSingleline <$> ps)
  clauseDocs <- docWrapNodeRest lmatch $ layoutGrhs `mapM` grhss
  mWhereDocs <- layoutLocalBinds whereBinds
  let alignmentToken = if null pats then Nothing else mIdStr
  hasComments <- hasAnyCommentsBelow lmatch
  layoutPatternBindFinal alignmentToken
                         binderDoc
                         (Just patDoc)
                         clauseDocs
                         mWhereDocs
                         hasComments

#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
fixPatternBindIdentifier
  :: HsMatchContext (NameOrRdrName RdrName) -> Text -> Text
fixPatternBindIdentifier ctx idStr = case ctx of
  (FunRhs _ _ SrcLazy    ) -> Text.cons '~' idStr
  (FunRhs _ _ SrcStrict  ) -> Text.cons '!' idStr
  (FunRhs _ _ NoSrcStrict) -> idStr
  (StmtCtxt ctx1         ) -> fixPatternBindIdentifier' ctx1
  _                        -> idStr
 where
  -- I have really no idea if this path ever occurs, but better safe than
  -- risking another "drop bangpatterns" bugs.
  fixPatternBindIdentifier' = \case
    (PatGuard      ctx1) -> fixPatternBindIdentifier ctx1 idStr
    (ParStmtCtxt   ctx1) -> fixPatternBindIdentifier' ctx1
    (TransStmtCtxt ctx1) -> fixPatternBindIdentifier' ctx1
    _                    -> idStr
#else                      /* ghc-8.0 */
fixPatternBindIdentifier :: MatchFixity RdrName -> Text -> Text
fixPatternBindIdentifier _ x = x
#endif

layoutPatternBindFinal
  :: Maybe Text
  -> BriDocNumbered
  -> Maybe BriDocNumbered
  -> [([BriDocNumbered], BriDocNumbered, LHsExpr RdrName)]
  -> Maybe [BriDocNumbered]
  -> Bool
  -> ToBriDocM BriDocNumbered
layoutPatternBindFinal alignmentToken binderDoc mPatDoc clauseDocs mWhereDocs hasComments = do
  let patPartInline  = case mPatDoc of
        Nothing     -> []
        Just patDoc -> [appSep $ docForceSingleline $ return patDoc]
      patPartParWrap = case mPatDoc of
        Nothing     -> id
        Just patDoc -> docPar (return patDoc)
  whereIndent <- do
    shouldSpecial <- mAsk
      <&> _conf_layout
      .>  _lconfig_indentWhereSpecial
      .>  confUnpack
    regularIndentAmount <- mAsk
      <&> _conf_layout
      .>  _lconfig_indentAmount
      .>  confUnpack
    pure $ if shouldSpecial
      then BrIndentSpecial (max 1 (regularIndentAmount `div` 2))
      else BrIndentRegular
  -- TODO: apart from this, there probably are more nodes below which could
  --       be shared between alternatives.
  wherePartMultiLine :: [ToBriDocM BriDocNumbered] <- case mWhereDocs of
    Nothing  -> return $ []
    Just [w] -> fmap (pure . pure) $ docAlt
      [ docEnsureIndent BrIndentRegular
        $ docSeq
            [ docLit $ Text.pack "where"
            , docSeparator
            , docForceSingleline $ return w
            ]
      , docEnsureIndent whereIndent $ docLines
        [ docLit $ Text.pack "where"
        , docEnsureIndent whereIndent
          $ docSetIndentLevel
          $ docNonBottomSpacing
          $ return w
        ]
      ]
    Just ws  -> fmap (pure . pure) $ docEnsureIndent whereIndent $ docLines
      [ docLit $ Text.pack "where"
      , docEnsureIndent whereIndent
        $   docSetIndentLevel
        $   docNonBottomSpacing
        $   docLines
        $   return
        <$> ws
      ]
  let singleLineGuardsDoc guards = appSep $ case guards of
        []  -> docEmpty
        [g] -> docSeq
               [appSep $ docLit $ Text.pack "|", docForceSingleline $ return g]
        gs  -> docSeq
            $  [appSep $ docLit $ Text.pack "|"]
            ++ (List.intersperse docCommaSep
                                 (docForceSingleline . return <$> gs)
               )

  indentPolicy <- mAsk
    <&> _conf_layout
    .>  _lconfig_indentPolicy
    .>  confUnpack
  runFilteredAlternative $ do

    let wherePart = case mWhereDocs of
          Nothing  -> Just docEmpty
          Just [w] -> Just $ docSeq
            [ docSeparator
            , appSep $ docLit $ Text.pack "where"
            , docSetIndentLevel $ docForceSingleline $ return w
            ]
          _        -> Nothing
    case clauseDocs of
      [(guards, body, _bodyRaw)] -> do
        let guardPart = singleLineGuardsDoc guards
        forM_ wherePart $ \wherePart' ->
          -- one-line solution
          addAlternativeCond (not hasComments) $ docCols
            (ColBindingLine alignmentToken)
            [ docSeq (patPartInline ++ [guardPart])
            , docSeq
              [ appSep $ return binderDoc
              , docForceSingleline $ return body
              , wherePart'
              ]
            ]
        -- one-line solution + where in next line(s)
        addAlternativeCond (Data.Maybe.isJust mWhereDocs)
          $ docLines
          $  [ docCols
               (ColBindingLine alignmentToken)
               [ docSeq (patPartInline ++ [guardPart])
               , docSeq
                 [appSep $ return binderDoc, docForceParSpacing $ return body]
               ]
             ]
          ++ wherePartMultiLine
        -- two-line solution + where in next line(s)
        addAlternative
          $ docLines
          $  [ docForceSingleline
             $ docSeq (patPartInline ++ [guardPart, return binderDoc])
             , docEnsureIndent BrIndentRegular $ docForceSingleline $ return body
             ]
          ++ wherePartMultiLine
        -- pattern and exactly one clause in single line, body as par;
        -- where in following lines
        addAlternative
          $ docLines
          $  [ docCols
               (ColBindingLine alignmentToken)
               [ docSeq (patPartInline ++ [guardPart])
               , docSeq
                 [ appSep $ return binderDoc
                 , docForceParSpacing $ docAddBaseY BrIndentRegular $ return body
                 ]
               ]
             ]
           -- , lineMod $ docAlt
           --   [ docSetBaseY $ return body
           --   , docAddBaseY BrIndentRegular $ return body
           --   ]
          ++ wherePartMultiLine
        -- pattern and exactly one clause in single line, body in new line.
        addAlternative
          $ docLines
          $  [ docSeq (patPartInline ++ [guardPart, return binderDoc])
             , docEnsureIndent BrIndentRegular
             $ docNonBottomSpacing
             $ docAddBaseY BrIndentRegular
             $ return body
             ]
          ++ wherePartMultiLine

      _ -> return ()

    case mPatDoc of
      Nothing     -> return ()
      Just patDoc ->
        -- multiple clauses added in-paragraph, each in a single line
        -- example: foo | bar = baz
        --              | lll = asd
        addAlternativeCond (indentPolicy /= IndentPolicyLeft)
          $ docLines
          $  [ docSeq
               [ appSep $ docForceSingleline $ return patDoc
               , docSetBaseY
               $   docLines
               $   clauseDocs
               <&> \(guardDocs, bodyDoc, _) -> do
                     let guardPart = singleLineGuardsDoc guardDocs
                     -- the docForceSingleline might seems superflous, but it
                     -- helps the alternative resolving impl.
                     docForceSingleline $ docCols
                       ColGuardedBody
                       [ guardPart
                       , docSeq
                         [ appSep $ return binderDoc
                         , docForceSingleline $ return bodyDoc
                         -- i am not sure if there is a benefit to using
                         -- docForceParSpacing additionally here:
                         -- , docAddBaseY BrIndentRegular $ return bodyDoc
                         ]
                       ]
               ]
             ]
          ++ wherePartMultiLine
    -- multiple clauses, each in a separate, single line
    addAlternative
      $ docLines
      $  [ docAddBaseY BrIndentRegular
           $   patPartParWrap
           $   docLines
           $   map docSetBaseY
           $   clauseDocs
           <&> \(guardDocs, bodyDoc, _) -> do
                 let guardPart = singleLineGuardsDoc guardDocs
                 -- the docForceSingleline might seems superflous, but it
                 -- helps the alternative resolving impl.
                 docForceSingleline $ docCols
                   ColGuardedBody
                   [ guardPart
                   , docSeq
                     [ appSep $ return binderDoc
                     , docForceSingleline $ return bodyDoc
                     -- i am not sure if there is a benefit to using
                     -- docForceParSpacing additionally here:
                     -- , docAddBaseY BrIndentRegular $ return bodyDoc
                     ]
                   ]
         ]
      ++ wherePartMultiLine
    -- multiple clauses, each with the guard(s) in a single line, body
    -- as a paragraph
    addAlternative
      $ docLines
      $  [ docAddBaseY BrIndentRegular
           $   patPartParWrap
           $   docLines
           $   map docSetBaseY
           $   clauseDocs
           <&> \(guardDocs, bodyDoc, _) ->
                 docSeq
                 $ ( case guardDocs of
                     [] -> []
                     [g] ->
                       [ docForceSingleline
                       $ docSeq [appSep $ docLit $ Text.pack "|", return g]
                       ]
                     gs ->
                       [  docForceSingleline
                       $  docSeq
                       $  [appSep $ docLit $ Text.pack "|"]
                       ++ List.intersperse docCommaSep (return <$> gs)
                       ]
                   )
                   ++ [ docSeparator
                      , docCols
                        ColOpPrefix
                        [ appSep $ return binderDoc
                        , docAddBaseY BrIndentRegular
                        $ docForceParSpacing
                        $ return bodyDoc
                        ]
                      ]
         ]
      ++ wherePartMultiLine
    -- multiple clauses, each with the guard(s) in a single line, body
    -- in a new line as a paragraph
    addAlternative
      $ docLines
      $  [ docAddBaseY BrIndentRegular
           $   patPartParWrap
           $   docLines
           $   map docSetBaseY
           $   clauseDocs
           >>= \(guardDocs, bodyDoc, _) ->
                 ( case guardDocs of
                   [] -> []
                   [g] ->
                     [ docForceSingleline
                     $ docSeq [appSep $ docLit $ Text.pack "|", return g]
                     ]
                   gs ->
                     [  docForceSingleline
                     $  docSeq
                     $  [appSep $ docLit $ Text.pack "|"]
                     ++ List.intersperse docCommaSep (return <$> gs)
                     ]
                 )
                 ++ [ docCols
                      ColOpPrefix
                      [ appSep $ return binderDoc
                      , docAddBaseY BrIndentRegular
                      $ docForceParSpacing
                      $ return bodyDoc
                      ]
                    ]
         ]
      ++ wherePartMultiLine
    -- conservative approach: everything starts on the left.
    addAlternative
      $ docLines
      $  [ docAddBaseY BrIndentRegular
           $   patPartParWrap
           $   docLines
           $   map docSetBaseY
           $   clauseDocs
           >>= \(guardDocs, bodyDoc, _) ->
                 ( case guardDocs of
                     [] -> []
                     [g] ->
                       [docSeq [appSep $ docLit $ Text.pack "|", return g]]
                     (g1:gr) ->
                       ( docSeq [appSep $ docLit $ Text.pack "|", return g1]
                       : (   gr
                         <&> \g ->
                               docSeq
                                 [appSep $ docLit $ Text.pack ",", return g]
                         )
                       )
                   )
                   ++ [ docCols
                        ColOpPrefix
                        [ appSep $ return binderDoc
                        , docAddBaseY BrIndentRegular $ return bodyDoc
                        ]
                      ]
         ]
      ++ wherePartMultiLine
