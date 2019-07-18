{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Language.Haskell.Brittany.Internal.Layouters.Decl
  ( layoutDecl
  , layoutSig
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

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import           Language.Haskell.Brittany.Internal.ExactPrintUtils
import           Language.Haskell.Brittany.Internal.Utils

import           GHC                            ( runGhc
                                                , GenLocated(L)
                                                , moduleNameString
                                                , AnnKeywordId(..)
                                                )
import           SrcLoc ( SrcSpan, noSrcSpan, Located , getLoc, unLoc )
import qualified FastString
import           HsSyn
#if MIN_VERSION_ghc(8,6,0)
import           HsExtension (NoExt (..))
#endif
import           Name
import           BasicTypes ( InlinePragma(..)
                            , Activation(..)
                            , InlineSpec(..)
                            , RuleMatchInfo(..)
#if MIN_VERSION_ghc(8,2,0)
                            , LexicalFixity(..)
#endif
                            )
import           Language.Haskell.GHC.ExactPrint.Types ( mkAnnKey )

import           Language.Haskell.Brittany.Internal.Layouters.Type
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Stmt
import           Language.Haskell.Brittany.Internal.Layouters.Pattern

import           Bag ( mapBagM, bagToList, emptyBag )
import           Data.Char (isUpper)



layoutDecl :: ToBriDoc HsDecl
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
layoutDecl d@(L loc decl) = case decl of
  SigD _ sig  -> withTransformedAnns d $ layoutSig (L loc sig)
  ValD _ bind -> withTransformedAnns d $ layoutBind (L loc bind) >>= \case
    Left  ns -> docLines $ return <$> ns
    Right n  -> return n
  TyClD _ tycl           -> withTransformedAnns d $ layoutTyCl (L loc tycl)
  InstD _ (TyFamInstD _ tfid) ->
    withTransformedAnns d $ layoutTyFamInstDecl False (L loc tfid)
  InstD _ (ClsInstD _ inst) ->
    withTransformedAnns d $ layoutClsInst (L loc inst)
  _ -> briDocByExactNoComment d
#else
layoutDecl d@(L loc decl) = case decl of
  SigD sig  -> withTransformedAnns d $ layoutSig (L loc sig)
  ValD bind -> withTransformedAnns d $ layoutBind (L loc bind) >>= \case
    Left  ns -> docLines $ return <$> ns
    Right n  -> return n
  TyClD tycl -> withTransformedAnns d $ layoutTyCl (L loc tycl)
  InstD (TyFamInstD tfid) ->
    withTransformedAnns d $ layoutTyFamInstDecl False (L loc tfid)
  InstD (ClsInstD inst) -> withTransformedAnns d $ layoutClsInst (L loc inst)
  _                    -> briDocByExactNoComment d
#endif


--------------------------------------------------------------------------------
-- Sig
--------------------------------------------------------------------------------

layoutSig :: ToBriDoc Sig
layoutSig lsig@(L _loc sig) = case sig of
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  TypeSig _ names (HsWC _ (HsIB _ typ)) -> layoutNamesAndType names typ
#elif MIN_VERSION_ghc(8,2,0) /* ghc-8.2 8.4 */
  TypeSig names (HsWC _ (HsIB _ typ _)) -> layoutNamesAndType names typ
#else /* ghc-8.0 */
  TypeSig names (HsIB _ (HsWC _ _ typ)) -> layoutNamesAndType names typ
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  InlineSig _ name (InlinePragma _ spec _arity phaseAct conlike) ->
#else
  InlineSig name (InlinePragma _ spec _arity phaseAct conlike) ->
#endif
    docWrapNode lsig $ do
      nameStr <- lrdrNameToTextAnn name
      specStr <- specStringCompat lsig spec
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
#if MIN_VERSION_ghc(8,6,0) /* ghc-8.6 */
  ClassOpSig _ False names (HsIB _ typ) -> layoutNamesAndType names typ
#elif MIN_VERSION_ghc(8,2,0) /* ghc-8.2 8.4 */
  ClassOpSig False names (HsIB _ typ _) -> layoutNamesAndType names typ
#else /* ghc-8.0 */
  ClassOpSig False names (HsIB _ typ) -> layoutNamesAndType names typ
#endif
  _ -> briDocByExactNoComment lsig -- TODO
 where
  layoutNamesAndType names typ = docWrapNode lsig $ do
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
      else layoutLhsAndType
        hasComments
        (appSep . docWrapNodeRest lsig $ docLit nameStr)
        "::"
        typeDoc

specStringCompat
  :: MonadMultiWriter [BrittanyError] m => LSig GhcPs -> InlineSpec -> m String
#if MIN_VERSION_ghc(8,4,0)
specStringCompat ast = \case
  NoUserInline    -> mTell [ErrorUnknownNode "NoUserInline" ast] $> ""
  Inline          -> pure "INLINE "
  Inlinable       -> pure "INLINABLE "
  NoInline        -> pure "NOINLINE "
#else
specStringCompat _ = \case
  Inline          -> pure "INLINE "
  Inlinable       -> pure "INLINABLE "
  NoInline        -> pure "NOINLINE "
  EmptyInlineSpec -> pure ""
#endif

layoutGuardLStmt :: ToBriDoc' (Stmt GhcPs (LHsExpr GhcPs))
layoutGuardLStmt lgstmt@(L _ stmtLR) = docWrapNode lgstmt $ case stmtLR of
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  BodyStmt _ body _ _      -> layoutExpr body
#else
  BodyStmt body _ _ _      -> layoutExpr body
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  BindStmt _ lPat expr _ _ -> do
#else
  BindStmt lPat expr _ _ _ -> do
#endif
    patDoc <- docSharedWrapper layoutPat lPat
    expDoc <- docSharedWrapper layoutExpr expr
    docCols ColBindStmt
            [ appSep $ colsWrapPat =<< patDoc
            , docSeq [appSep $ docLit $ Text.pack "<-", expDoc]
            ]
  _                        -> unknownNodeError "" lgstmt -- TODO


--------------------------------------------------------------------------------
-- HsBind
--------------------------------------------------------------------------------

layoutBind
  :: ToBriDocC
       (HsBindLR GhcPs GhcPs)
       (Either [BriDocNumbered] BriDocNumbered)
layoutBind lbind@(L _ bind) = case bind of
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  FunBind _ fId (MG _ lmatches@(L _ matches) _) _ [] -> do
#else
  FunBind fId (MG lmatches@(L _ matches) _ _ _) _ _ [] -> do
#endif
    idStr       <- lrdrNameToTextAnn fId
    binderDoc   <- docLit $ Text.pack "="
    funcPatDocs <-
      docWrapNode lbind
        $      docWrapNode lmatches
        $      layoutPatternBind (Just idStr) binderDoc
        `mapM` matches
    return $ Left $ funcPatDocs
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  PatBind _ pat (GRHSs _ grhss whereBinds) ([], []) -> do
#else
  PatBind pat (GRHSs grhss whereBinds) _ _ ([], []) -> do
#endif
    patDocs    <- colsWrapPat =<< layoutPat pat
    clauseDocs <- layoutGrhs `mapM` grhss
    mWhereDocs <- layoutLocalBinds whereBinds
    let mWhereArg = mWhereDocs <&> \d -> (mkAnnKey lbind, d) -- TODO: is this the right AnnKey?
    binderDoc  <- docLit $ Text.pack "="
    hasComments <- hasAnyCommentsBelow lbind
    fmap Right $ docWrapNode lbind $ layoutPatternBindFinal Nothing
                                                            binderDoc
                                                            (Just patDocs)
                                                            clauseDocs
                                                            mWhereArg
                                                            hasComments
  _ -> Right <$> unknownNodeError "" lbind

layoutIPBind :: ToBriDoc IPBind
layoutIPBind lipbind@(L _ bind) = case bind of
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  XIPBind{} -> unknownNodeError "XIPBind" lipbind
  IPBind _ (Right _) _ -> error "brittany internal error: IPBind Right"
  IPBind _ (Left (L _ (HsIPName name))) expr -> do
#else
  IPBind (Right _) _ -> error "brittany internal error: IPBind Right"
  IPBind (Left (L _ (HsIPName name))) expr -> do
#endif
    ipName <- docLit $ Text.pack $ '?' : FastString.unpackFS name
    binderDoc <- docLit $ Text.pack "="
    exprDoc <- layoutExpr expr
    hasComments <- hasAnyCommentsBelow lipbind
    layoutPatternBindFinal Nothing binderDoc (Just ipName) [([], exprDoc, expr)] Nothing hasComments


data BagBindOrSig = BagBind (LHsBindLR GhcPs GhcPs)
                  | BagSig (LSig GhcPs)

bindOrSigtoSrcSpan :: BagBindOrSig -> SrcSpan
bindOrSigtoSrcSpan (BagBind (L l _)) = l
bindOrSigtoSrcSpan (BagSig  (L l _)) = l

layoutLocalBinds
  :: ToBriDocC (HsLocalBindsLR GhcPs GhcPs) (Maybe [BriDocNumbered])
layoutLocalBinds lbinds@(L _ binds) = case binds of
  -- HsValBinds (ValBindsIn lhsBindsLR []) ->
  --   Just . (>>= either id return) . Data.Foldable.toList <$> mapBagM layoutBind lhsBindsLR -- TODO: fix ordering
  -- x@(HsValBinds (ValBindsIn{})) ->
  --   Just . (:[]) <$> unknownNodeError "HsValBinds (ValBindsIn _ (_:_))" x
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  HsValBinds _ (ValBinds _ bindlrs sigs) -> do
#else
  HsValBinds (ValBindsIn bindlrs sigs) -> do
#endif
    let unordered =
          [ BagBind b | b <- Data.Foldable.toList bindlrs ]
            ++ [ BagSig s | s <- sigs ]
        ordered = sortBy (comparing bindOrSigtoSrcSpan) unordered
    docs <- docWrapNode lbinds $ join <$> ordered `forM` \case
      BagBind b -> either id return <$> layoutBind b
      BagSig  s -> return <$> layoutSig s
    return $ Just $ docs
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
--  x@(HsValBinds (ValBindsOut _binds _lsigs)) ->
  HsValBinds _ (XValBindsLR{}) -> error "brittany internal error: XValBindsLR"
  XHsLocalBindsLR{} -> error "brittany internal error: XHsLocalBindsLR"
#else
  x@(HsValBinds (ValBindsOut _binds _lsigs)) ->
    -- i _think_ this case never occurs in non-processed ast
    Just . (: []) <$> unknownNodeError "HsValBinds ValBindsOut{}"
                                       (L noSrcSpan x)
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  x@(HsIPBinds _ XHsIPBinds{}) ->
    Just . (: []) <$> unknownNodeError "XHsIPBinds" (L noSrcSpan x)
  HsIPBinds _ (IPBinds _ bb) ->
#else
  HsIPBinds (IPBinds bb _) ->
#endif
    Just <$> mapM layoutIPBind bb
  EmptyLocalBinds{} -> return $ Nothing

-- TODO: we don't need the `LHsExpr GhcPs` anymore, now that there is
-- parSpacing stuff.B
layoutGrhs
  :: LGRHS GhcPs (LHsExpr GhcPs)
  -> ToBriDocM ([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs)
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
layoutGrhs lgrhs@(L _ (GRHS _ guards body)) = do
#else
layoutGrhs lgrhs@(L _ (GRHS guards body)) = do
#endif
  guardDocs <- docWrapNode lgrhs $ layoutStmt `mapM` guards
  bodyDoc   <- layoutExpr body
  return (guardDocs, bodyDoc, body)
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
layoutGrhs (L _ (XGRHS{})) = error "brittany internal error: XGRHS"
#endif

layoutPatternBind
  :: Maybe Text
  -> BriDocNumbered
  -> LMatch GhcPs (LHsExpr GhcPs)
  -> ToBriDocM BriDocNumbered
layoutPatternBind funId binderDoc lmatch@(L _ match) = do
  let pats                     = m_pats match
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
  let (GRHSs _ grhss whereBinds) = m_grhss match
#else
  let (GRHSs grhss whereBinds) = m_grhss match
#endif
  patDocs <- pats `forM` \p -> fmap return $ colsWrapPat =<< layoutPat p
  let isInfix = isInfixMatch match
  mIdStr <- case match of
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    Match _ (FunRhs matchId _ _) _ _ -> Just <$> lrdrNameToTextAnn matchId
#elif MIN_VERSION_ghc(8,4,0) /* ghc-8.4 */
    Match (FunRhs matchId _ _) _ _ -> Just <$> lrdrNameToTextAnn matchId
#elif MIN_VERSION_ghc(8,2,0) /* ghc-8.4 */
    Match (FunRhs matchId _ _) _ _ _ -> Just <$> lrdrNameToTextAnn matchId
#else
    Match (FunBindMatch matchId _) _ _ _ -> Just <$> lrdrNameToTextAnn matchId
#endif
    _ -> pure Nothing
  let mIdStr' = fixPatternBindIdentifier match <$> mIdStr
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
  let mWhereArg = mWhereDocs <&> \d -> (mkAnnKey lmatch, d)
  let alignmentToken = if null pats then Nothing else funId
  hasComments <- hasAnyCommentsBelow lmatch
  layoutPatternBindFinal alignmentToken
                         binderDoc
                         (Just patDoc)
                         clauseDocs
                         mWhereArg
                         hasComments

#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 && ghc-8.4 */
fixPatternBindIdentifier
  :: Match GhcPs (LHsExpr GhcPs) -> Text -> Text
fixPatternBindIdentifier match idStr = go $ m_ctxt match
 where
  go = \case
    (FunRhs _ _ SrcLazy    ) -> Text.cons '~' idStr
    (FunRhs _ _ SrcStrict  ) -> Text.cons '!' idStr
    (FunRhs _ _ NoSrcStrict) -> idStr
    (StmtCtxt ctx1         ) -> goInner ctx1
    _                        -> idStr
  -- I have really no idea if this path ever occurs, but better safe than
  -- risking another "drop bangpatterns" bugs.
  goInner = \case
    (PatGuard      ctx1) -> go ctx1
    (ParStmtCtxt   ctx1) -> goInner ctx1
    (TransStmtCtxt ctx1) -> goInner ctx1
    _                    -> idStr
#else                       /* ghc-8.0 */
fixPatternBindIdentifier :: Match GhcPs (LHsExpr GhcPs) -> Text -> Text
fixPatternBindIdentifier _ x = x
#endif

layoutPatternBindFinal
  :: Maybe Text
  -> BriDocNumbered
  -> Maybe BriDocNumbered
  -> [([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs)]
  -> Maybe (ExactPrint.AnnKey, [BriDocNumbered])
     -- ^ AnnKey for the node that contains the AnnWhere position annotation
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
    Just (annKeyWhere, [w]) -> fmap (pure . pure) $ docAlt
      [ docEnsureIndent BrIndentRegular
        $ docSeq
            [ docLit $ Text.pack "where"
            , docSeparator
            , docForceSingleline $ return w
            ]
      , docMoveToKWDP annKeyWhere AnnWhere False
        $ docEnsureIndent whereIndent
        $ docLines
          [ docLit $ Text.pack "where"
          , docEnsureIndent whereIndent
            $ docSetIndentLevel
            $ docNonBottomSpacing
            $ return w
          ]
      ]
    Just (annKeyWhere, ws) ->
      fmap (pure . pure)
        $ docMoveToKWDP annKeyWhere AnnWhere False
        $ docEnsureIndent whereIndent
        $ docLines
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
      wherePart = case mWhereDocs of
        Nothing  -> Just docEmpty
        Just (_, [w]) -> Just $ docSeq
          [ docSeparator
          , appSep $ docLit $ Text.pack "where"
          , docSetIndentLevel $ docForceSingleline $ return w
          ]
        _        -> Nothing

  indentPolicy <- mAsk
    <&> _conf_layout
    .>  _lconfig_indentPolicy
    .>  confUnpack

  runFilteredAlternative $ do

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
             , docNonBottomSpacing
             $ docEnsureIndent BrIndentRegular
             $ docAddBaseY BrIndentRegular
             $ return body
             ]
          ++ wherePartMultiLine

      _ -> return () -- no alternatives exclusively when `length clauseDocs /= 1`

    case mPatDoc of
      Nothing     -> return ()
      Just patDoc ->
        -- multiple clauses added in-paragraph, each in a single line
        -- example: foo | bar = baz
        --              | lll = asd
        addAlternativeCond (indentPolicy == IndentPolicyFree)
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


--------------------------------------------------------------------------------
-- TyClDecl
--------------------------------------------------------------------------------

layoutTyCl :: ToBriDoc TyClDecl
layoutTyCl ltycl@(L _loc tycl) = case tycl of
#if MIN_VERSION_ghc(8,6,0)
  SynDecl _ name vars fixity typ -> do
    let isInfix = case fixity of
          Prefix -> False
          Infix  -> True
#elif MIN_VERSION_ghc(8,2,0)
  SynDecl name vars fixity typ _ -> do
    let isInfix = case fixity of
          Prefix -> False
          Infix  -> True
#else
  SynDecl name vars typ _ -> do
    nameStr <- lrdrNameToTextAnn name
    let isInfixTypeOp = case Text.uncons nameStr of
          Nothing -> False
          Just (c, _) -> not (c == '(' || isUpper c)
    isInfix <- (isInfixTypeOp ||) <$> hasAnnKeyword name AnnBackquote
#endif
    -- hasTrailingParen <- hasAnnKeywordComment ltycl AnnCloseP
    -- let parenWrapper = if hasTrailingParen
    --       then appSep . docWrapNodeRest ltycl
    --       else id
    let wrapNodeRest = docWrapNodeRest ltycl
    docWrapNodePrior ltycl
      $ layoutSynDecl isInfix wrapNodeRest name (hsq_explicit vars) typ
  _ -> briDocByExactNoComment ltycl

layoutSynDecl
  :: Bool
  -> (ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered)
  -> Located (IdP GhcPs)
  -> [LHsTyVarBndr GhcPs]
  -> LHsType GhcPs
  -> ToBriDocM BriDocNumbered
layoutSynDecl isInfix wrapNodeRest name vars typ = do
  nameStr <- lrdrNameToTextAnn name
  let
    lhs = appSep . wrapNodeRest $ if isInfix
      then do
        let (a : b : rest) = vars
        hasOwnParens <- hasAnnKeywordComment a AnnOpenP
        -- This isn't quite right, but does give syntactically valid results
        let needsParens = not $ null rest || hasOwnParens
        docSeq
          $  [ docLit $ Text.pack "type"
             , docSeparator
             ]
          ++ [ docParenL | needsParens ]
          ++ [ layoutTyVarBndr False a
             , docSeparator
             , docLit nameStr
             , docSeparator
             , layoutTyVarBndr False b
             ]
          ++ [ docParenR | needsParens ]
          ++ fmap (layoutTyVarBndr True) rest
      else
        docSeq
        $  [ docLit $ Text.pack "type"
           , docSeparator
           , docWrapNode name $ docLit nameStr
           ]
        ++ fmap (layoutTyVarBndr True) vars
  sharedLhs   <- docSharedWrapper id lhs
  typeDoc     <- docSharedWrapper layoutType typ
  hasComments <- hasAnyCommentsConnected typ
  layoutLhsAndType hasComments sharedLhs "=" typeDoc

layoutTyVarBndr :: Bool -> ToBriDoc HsTyVarBndr
layoutTyVarBndr needsSep lbndr@(L _ bndr) = do
  docWrapNodePrior lbndr $ case bndr of
#if MIN_VERSION_ghc(8,6,0)    /* 8.6 */
    XTyVarBndr{} -> error "brittany internal error: XTyVarBndr"
    UserTyVar _ name -> do
#else                         /* 8.0 8.2 8.4 */
    UserTyVar name -> do
#endif
      nameStr <- lrdrNameToTextAnn name
      docSeq $ [docSeparator | needsSep] ++ [docLit nameStr]
#if MIN_VERSION_ghc(8,6,0)    /* 8.6 */
    KindedTyVar _ name kind -> do
#else                         /* 8.0 8.2 8.4 */
    KindedTyVar name kind -> do
#endif
      nameStr <- lrdrNameToTextAnn name
      docSeq
        $  [ docSeparator | needsSep ]
        ++ [ docLit $ Text.pack "("
           , appSep $ docLit nameStr
           , appSep . docLit $ Text.pack "::"
           , docForceSingleline $ layoutType kind
           , docLit $ Text.pack ")"
           ]


--------------------------------------------------------------------------------
-- TyFamInstDecl
--------------------------------------------------------------------------------

layoutTyFamInstDecl :: Bool -> ToBriDoc TyFamInstDecl
layoutTyFamInstDecl inClass (L loc tfid) = do
  let
#if MIN_VERSION_ghc(8,6,0)
    linst = L loc (TyFamInstD NoExt tfid)
    feqn@(FamEqn _ name pats _fixity typ) = hsib_body $ tfid_eqn tfid
    lfeqn = L loc feqn
#elif MIN_VERSION_ghc(8,4,0)
    linst = L loc (TyFamInstD tfid)
    feqn@(FamEqn name pats _fixity typ) = hsib_body $ tfid_eqn tfid
    lfeqn = L loc feqn
#elif MIN_VERSION_ghc(8,2,0)
    linst = L loc (TyFamInstD tfid)
    lfeqn@(L _ (TyFamEqn name boundPats _fixity typ)) = tfid_eqn tfid
    pats = hsib_body boundPats
#else
    linst = L loc (TyFamInstD tfid)
    lfeqn@(L _ (TyFamEqn name boundPats typ)) = tfid_eqn tfid
    pats = hsib_body boundPats
#endif
  docWrapNodePrior linst $ do
    nameStr   <- lrdrNameToTextAnn name
    needsParens <- hasAnnKeyword lfeqn AnnOpenP
    let
      instanceDoc = if inClass
        then docLit $ Text.pack "type"
        else docSeq
          [appSep . docLit $ Text.pack "type", docLit $ Text.pack "instance"]
      lhs =
        docWrapNode lfeqn
          .  appSep
          .  docWrapNodeRest linst
          .  docSeq
          $  (appSep instanceDoc :)
          $  [ docParenL | needsParens ]
          ++ [appSep $ docWrapNode name $ docLit nameStr]
          ++ intersperse docSeparator (layoutType <$> pats)
          ++ [ docParenR | needsParens ]
    hasComments <- (||)
      <$> hasAnyRegularCommentsConnected lfeqn
      <*> hasAnyRegularCommentsRest linst
    typeDoc <- docSharedWrapper layoutType typ
    layoutLhsAndType hasComments lhs "=" typeDoc


--------------------------------------------------------------------------------
-- ClsInstDecl
--------------------------------------------------------------------------------

-- | Layout an @instance@ declaration
--
--   Layout signatures and bindings using the corresponding layouters from the
--   top-level. Layout the instance head, type family instances, and data family
--   instances using ExactPrint.
layoutClsInst :: ToBriDoc ClsInstDecl
layoutClsInst lcid@(L _ cid) = docLines
  [ layoutInstanceHead
  , docEnsureIndent BrIndentRegular
  $  docSortedLines
  $  fmap layoutAndLocateSig          (cid_sigs cid)
  ++ fmap layoutAndLocateBind         (bagToList $ cid_binds cid)
  ++ fmap layoutAndLocateTyFamInsts   (cid_tyfam_insts cid)
  ++ fmap layoutAndLocateDataFamInsts (cid_datafam_insts cid)
  ]
 where
  layoutInstanceHead :: ToBriDocM BriDocNumbered
#if MIN_VERSION_ghc(8,6,0)    /* 8.6 */
  layoutInstanceHead =
    briDocByExactNoComment
      $   InstD NoExt
      .   ClsInstD NoExt
      .   removeChildren
      <$> lcid
#else
  layoutInstanceHead =
    briDocByExactNoComment
      $   InstD
      .   ClsInstD
      .   removeChildren
      <$> lcid
#endif

  removeChildren :: ClsInstDecl p -> ClsInstDecl p
  removeChildren c = c
    { cid_binds         = emptyBag
    , cid_sigs          = []
    , cid_tyfam_insts   = []
    , cid_datafam_insts = []
    }

  -- | Like 'docLines', but sorts the lines based on location
  docSortedLines
    :: [ToBriDocM (Located BriDocNumbered)] -> ToBriDocM BriDocNumbered
  docSortedLines l =
    allocateNode . BDFLines . fmap unLoc . List.sortOn getLoc =<< sequence l

  layoutAndLocateSig :: ToBriDocC (Sig GhcPs) (Located BriDocNumbered)
  layoutAndLocateSig lsig@(L loc _) = L loc <$> layoutSig lsig

  layoutAndLocateBind :: ToBriDocC (HsBind GhcPs) (Located BriDocNumbered)
  layoutAndLocateBind lbind@(L loc _) =
    L loc <$> (joinBinds =<< layoutBind lbind)

  joinBinds
    :: Either [BriDocNumbered] BriDocNumbered -> ToBriDocM BriDocNumbered
  joinBinds = \case
    Left  ns -> docLines $ return <$> ns
    Right n  -> return n

  layoutAndLocateTyFamInsts
    :: ToBriDocC (TyFamInstDecl GhcPs) (Located BriDocNumbered)
  layoutAndLocateTyFamInsts ltfid@(L loc _) =
    L loc <$> layoutTyFamInstDecl True ltfid

  layoutAndLocateDataFamInsts
    :: ToBriDocC (DataFamInstDecl GhcPs) (Located BriDocNumbered)
  layoutAndLocateDataFamInsts ldfid@(L loc _) =
    L loc <$> layoutDataFamInstDecl ldfid

  -- | Send to ExactPrint then remove unecessary whitespace
  layoutDataFamInstDecl :: ToBriDoc DataFamInstDecl
  layoutDataFamInstDecl ldfid =
    fmap stripWhitespace <$> briDocByExactNoComment ldfid

  -- | ExactPrint adds indentation/newlines to @data@/@type@ declarations
  stripWhitespace :: BriDocF f -> BriDocF f
  stripWhitespace (BDFExternal ann anns b t) =
    BDFExternal ann anns b $ stripWhitespace' t
  stripWhitespace b = b

  -- | This fixes two issues of output coming from Exactprinting
  --   associated (data) type decls. Firstly we place the output into docLines,
  --   so one newline coming from Exactprint is superfluous, so we drop the
  --   first (empty) line. The second issue is Exactprint indents the first
  --   member in a strange fashion:
  --
  --   input:
  --
  --   > instance MyClass Int where
  --   >   -- | This data is very important
  --   >   data MyData = IntData
  --   >     { intData  :: String
  --   >     , intData2 :: Int
  --   >     }
  --
  --   output of just exactprinting the associated data type syntax node
  --
  --   >
  --   >   -- | This data is very important
  --   >   data MyData = IntData
  --   >   { intData  :: String
  --   >   , intData2 :: Int
  --   >   }
  --
  --   To fix this, we strip whitespace from the start of the comments and the
  --   first line of the declaration, stopping when we see "data" or "type" at
  --   the start of a line. I.e., this function yields
  --
  --   > -- | This data is very important
  --   > data MyData = IntData
  --   >   { intData  :: String
  --   >   , intData2 :: Int
  --   >   }
  --
  --   Downside apart from being a hacky and brittle fix is that this removes
  --   possible additional indentation from comments before the first member.
  --
  --   But the whole thing is just a temporary measure until brittany learns
  --   to layout data/type decls.
  stripWhitespace' :: Text -> Text
  stripWhitespace' t =
    Text.intercalate (Text.pack "\n") $ go $ List.drop 1 $ Text.lines t
   where
    go []              = []
    go (line1 : lineR) = case Text.stripStart line1 of
      st | isTypeOrData st -> st : lineR
         | otherwise       -> st : go lineR
    isTypeOrData t' =
      (Text.pack "type" `Text.isPrefixOf` t')
        || (Text.pack "data" `Text.isPrefixOf` t')


--------------------------------------------------------------------------------
-- Common Helpers
--------------------------------------------------------------------------------

layoutLhsAndType
  :: Bool
  -> ToBriDocM BriDocNumbered
  -> String
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
layoutLhsAndType hasComments lhs sep typeDoc = do
  let sepDoc = appSep . docLit $ Text.pack sep
  runFilteredAlternative $ do
    -- (separators probably are "=" or "::")
    -- lhs = type
    -- lhs :: type
    addAlternativeCond (not hasComments)
      $ docSeq [lhs, sepDoc, docForceSingleline typeDoc]
    -- lhs
    --   :: typeA
    --   -> typeB
    -- lhs
    --   =  typeA
    --   -> typeB
    addAlternative $ docAddBaseY BrIndentRegular $ docPar lhs $ docCols
      ColTyOpPrefix
      [sepDoc, docAddBaseY (BrIndentSpecial (length sep + 1)) typeDoc]
