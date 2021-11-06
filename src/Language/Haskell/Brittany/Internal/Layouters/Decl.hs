{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

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
import           Language.Haskell.Brittany.Internal.Layouters.Type

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint
import           Language.Haskell.Brittany.Internal.ExactPrintUtils
import           Language.Haskell.Brittany.Internal.Utils

import           GHC                            ( runGhc
                                                , GenLocated(L)
                                                , moduleNameString
                                                , AnnKeywordId(..)
                                                )
import           GHC.Types.SrcLoc ( SrcSpan, noSrcSpan, Located , getLoc, unLoc )
import qualified GHC.Data.FastString as FastString
import           GHC.Hs
import           GHC.Hs.Extension (NoExtField (..))
import           GHC.Types.Name
import           GHC.Types.Basic ( InlinePragma(..)
                            , Activation(..)
                            , InlineSpec(..)
                            , RuleMatchInfo(..)
                            , LexicalFixity(..)
                            )
import           Language.Haskell.GHC.ExactPrint.Types ( mkAnnKey )

import           Language.Haskell.Brittany.Internal.Layouters.Type
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Stmt
import           Language.Haskell.Brittany.Internal.Layouters.Pattern
import           Language.Haskell.Brittany.Internal.Layouters.DataDecl

import           GHC.Data.Bag ( mapBagM, bagToList, emptyBag )
import           Data.Char (isUpper)



layoutDecl :: ToBriDoc HsDecl
layoutDecl d@(L loc decl) = case decl of
  SigD _ sig  -> withTransformedAnns d $ layoutSig (L loc sig)
  ValD _ bind -> withTransformedAnns d $ layoutBind (L loc bind) >>= \case
    Left  ns -> docLines $ return <$> ns
    Right n  -> return n
  TyClD _ tycl           -> withTransformedAnns d $ layoutTyCl (L loc tycl)
  InstD _ (TyFamInstD _ tfid) ->
    withTransformedAnns d $ layoutTyFamInstDecl False d tfid
  InstD _ (ClsInstD _ inst) ->
    withTransformedAnns d $ layoutClsInst (L loc inst)
  _ -> briDocByExactNoComment d

--------------------------------------------------------------------------------
-- Sig
--------------------------------------------------------------------------------

layoutSig :: ToBriDoc Sig
layoutSig lsig@(L _loc sig) = case sig of
  TypeSig _ names (HsWC _ (HsIB _ typ)) -> layoutNamesAndType Nothing names typ
  InlineSig _ name (InlinePragma _ spec _arity phaseAct conlike) ->
    docWrapNode lsig $ do
      nameStr <- lrdrNameToTextAnn name
      specStr <- specStringCompat lsig spec
      let phaseStr = case phaseAct of
            NeverActive      -> "" -- not [] - for NOINLINE NeverActive is
                                   -- in fact the default
            AlwaysActive     -> ""
            ActiveBefore _ i -> "[~" ++ show i ++ "] "
            ActiveAfter  _ i -> "[" ++ show i ++ "] "
            FinalActive      -> error "brittany internal error: FinalActive"
      let conlikeStr = case conlike of
            FunLike -> ""
            ConLike -> "CONLIKE "
      docLit
        $  Text.pack ("{-# " ++ specStr ++ conlikeStr ++ phaseStr)
        <> nameStr
        <> Text.pack " #-}"
  ClassOpSig _ False names (HsIB _ typ) -> layoutNamesAndType Nothing names typ
  PatSynSig _ names (HsIB _ typ) -> layoutNamesAndType (Just "pattern") names typ
  _ -> briDocByExactNoComment lsig -- TODO
 where
  layoutNamesAndType mKeyword names typ = docWrapNode lsig $ do
    let keyDoc = case mKeyword of
          Just key -> [appSep . docLit $ Text.pack key]
          Nothing -> []
    nameStrs <- names `forM` lrdrNameToTextAnn
    let nameStr = Text.intercalate (Text.pack ", ") $ nameStrs
    typeDoc     <- docSharedWrapper layoutType typ
    hasComments <- hasAnyCommentsBelow lsig
    shouldBeHanging <- mAsk
      <&> _conf_layout
      .>  _lconfig_hangingTypeSignature
      .>  confUnpack
    if shouldBeHanging
      then docSeq $
        [ appSep $ docWrapNodeRest lsig $ docSeq $ keyDoc <> [docLit nameStr]
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
        (appSep . docWrapNodeRest lsig . docSeq $ keyDoc <> [docLit nameStr])
        "::"
        typeDoc

specStringCompat
  :: MonadMultiWriter [BrittanyError] m => LSig GhcPs -> InlineSpec -> m String
specStringCompat ast = \case
  NoUserInline    -> mTell [ErrorUnknownNode "NoUserInline" ast] $> ""
  Inline          -> pure "INLINE "
  Inlinable       -> pure "INLINABLE "
  NoInline        -> pure "NOINLINE "

layoutGuardLStmt :: ToBriDoc' (Stmt GhcPs (LHsExpr GhcPs))
layoutGuardLStmt lgstmt@(L _ stmtLR) = docWrapNode lgstmt $ case stmtLR of
  BodyStmt _ body _ _      -> layoutExpr body
  BindStmt _ lPat expr -> do
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
  FunBind _ fId (MG _ lmatches@(L _ matches) _) [] -> do
    idStr       <- lrdrNameToTextAnn fId
    binderDoc   <- docLit $ Text.pack "="
    funcPatDocs <-
      docWrapNode lbind
        $      docWrapNode lmatches
        $      layoutPatternBind (Just idStr) binderDoc
        `mapM` matches
    return $ Left $ funcPatDocs
  PatBind _ pat (GRHSs _ grhss whereBinds) ([], []) -> do
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
  PatSynBind _ (PSB _ patID lpat rpat dir) -> do
    fmap Right $ docWrapNode lbind $ layoutPatSynBind patID
                                                      lpat
                                                      dir
                                                      rpat
  _ -> Right <$> unknownNodeError "" lbind
layoutIPBind :: ToBriDoc IPBind
layoutIPBind lipbind@(L _ bind) = case bind of
  IPBind _ (Right _) _ -> error "brittany internal error: IPBind Right"
  IPBind _ (Left (L _ (HsIPName name))) expr -> do
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
  HsValBinds _ (ValBinds _ bindlrs sigs) -> do
    let unordered =
          [ BagBind b | b <- Data.Foldable.toList bindlrs ]
            ++ [ BagSig s | s <- sigs ]
        ordered = sortBy (comparing $ ExactPrint.rs . bindOrSigtoSrcSpan) unordered
    docs <- docWrapNode lbinds $ join <$> ordered `forM` \case
      BagBind b -> either id return <$> layoutBind b
      BagSig  s -> return <$> layoutSig s
    return $ Just $ docs
--  x@(HsValBinds (ValBindsOut _binds _lsigs)) ->
  HsValBinds _ (XValBindsLR{}) -> error "brittany internal error: XValBindsLR"
  HsIPBinds _ (IPBinds _ bb) ->
    Just <$> mapM layoutIPBind bb
  EmptyLocalBinds{} -> return $ Nothing

-- TODO: we don't need the `LHsExpr GhcPs` anymore, now that there is
-- parSpacing stuff.B
layoutGrhs
  :: LGRHS GhcPs (LHsExpr GhcPs)
  -> ToBriDocM ([BriDocNumbered], BriDocNumbered, LHsExpr GhcPs)
layoutGrhs lgrhs@(L _ (GRHS _ guards body)) = do
  guardDocs <- docWrapNode lgrhs $ layoutStmt `mapM` guards
  bodyDoc   <- layoutExpr body
  return (guardDocs, bodyDoc, body)

layoutPatternBind
  :: Maybe Text
  -> BriDocNumbered
  -> LMatch GhcPs (LHsExpr GhcPs)
  -> ToBriDocM BriDocNumbered
layoutPatternBind funId binderDoc lmatch@(L _ match) = do
  let pats                     = m_pats match
  let (GRHSs _ grhss whereBinds) = m_grhss match
  patDocs <- pats `forM` \p -> fmap return $ colsWrapPat =<< layoutPat p
  let isInfix = isInfixMatch match
  mIdStr <- case match of
    Match _ (FunRhs matchId _ _) _ _ -> Just <$> lrdrNameToTextAnn matchId
    _ -> pure Nothing
  let mIdStr' = fixPatternBindIdentifier match <$> mIdStr
  patDoc <- docWrapNodePrior lmatch $ case (mIdStr', patDocs) of
    (Just idStr, p1:p2:pr) | isInfix -> if null pr
      then
        docCols ColPatternsFuncInfix
          [ appSep $ docForceSingleline p1
          , appSep $ docLit $ idStr
          , docForceSingleline p2
          ]
      else
        docCols ColPatternsFuncInfix
          (  [docCols ColPatterns
               [ docParenL
               , appSep $ docForceSingleline p1
               , appSep $ docLit $ idStr
               , docForceSingleline p2
               , appSep $ docParenR
               ]
             ]
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
                 [ appSep $ return binderDoc
                 , docForceParSpacing $ docAddBaseY BrIndentRegular $ return body
                 ]
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

-- | Layout a pattern synonym binding
layoutPatSynBind
  :: Located (IdP GhcPs)
  -> HsPatSynDetails (Located (IdP GhcPs))
  -> HsPatSynDir GhcPs
  -> LPat GhcPs
  -> ToBriDocM BriDocNumbered
layoutPatSynBind name patSynDetails patDir rpat = do
  let patDoc = docLit $ Text.pack "pattern"
      binderDoc = case patDir of
        ImplicitBidirectional -> docLit $ Text.pack "="
        _ -> docLit $ Text.pack "<-"
      body = colsWrapPat =<< layoutPat rpat
      whereDoc = docLit $ Text.pack "where"
  mWhereDocs <- layoutPatSynWhere patDir
  headDoc <- fmap pure $ docSeq $
    [ patDoc
    , docSeparator
    , layoutLPatSyn name patSynDetails
    , docSeparator
    , binderDoc
    ]
  runFilteredAlternative $ do
    addAlternative $
      -- pattern .. where
      --   ..
      --   ..
      docAddBaseY BrIndentRegular $ docSeq
        (  [headDoc, docSeparator, body]
        ++ case mWhereDocs of
            Just ds -> [docSeparator, docPar whereDoc (docLines ds)]
            Nothing -> []
        )
    addAlternative $
      -- pattern .. =
      --   ..
      -- pattern .. <-
      --   .. where
      --   ..
      --   ..
      docAddBaseY BrIndentRegular $ docPar
        headDoc
        (case mWhereDocs of
          Nothing -> body
          Just ds -> docLines ([ docSeq [body, docSeparator, whereDoc] ] ++ ds)
        )

-- | Helper method for the left hand side of a pattern synonym
layoutLPatSyn
  :: Located (IdP GhcPs)
  -> HsPatSynDetails (Located (IdP GhcPs))
  -> ToBriDocM BriDocNumbered
layoutLPatSyn name (PrefixCon vars) = do
  docName <- lrdrNameToTextAnn name
  names <- mapM lrdrNameToTextAnn vars
  docSeq . fmap appSep $ docLit docName : (docLit <$> names)
layoutLPatSyn name (InfixCon left right) = do
  leftDoc <- lrdrNameToTextAnn left
  docName <- lrdrNameToTextAnn name
  rightDoc <- lrdrNameToTextAnn right
  docSeq . fmap (appSep . docLit) $ [leftDoc, docName, rightDoc]
layoutLPatSyn name (RecCon recArgs) = do
  docName <- lrdrNameToTextAnn name
  args <- mapM (lrdrNameToTextAnn . recordPatSynSelectorId) recArgs
  docSeq . fmap docLit
    $  [docName, Text.pack " { " ]
    <> intersperse (Text.pack ", ") args
    <> [Text.pack " }"]

-- | Helper method to get the where clause from of explicitly bidirectional
-- pattern synonyms
layoutPatSynWhere :: HsPatSynDir GhcPs -> ToBriDocM (Maybe [ToBriDocM BriDocNumbered])
layoutPatSynWhere hs = case hs of
  ExplicitBidirectional (MG _ (L _ lbinds) _) -> do
    binderDoc <- docLit $ Text.pack "="
    Just <$> mapM (docSharedWrapper $ layoutPatternBind Nothing binderDoc) lbinds
  _ -> pure Nothing

--------------------------------------------------------------------------------
-- TyClDecl
--------------------------------------------------------------------------------

layoutTyCl :: ToBriDoc TyClDecl
layoutTyCl ltycl@(L _loc tycl) = case tycl of
  SynDecl _ name vars fixity typ -> do
    let isInfix = case fixity of
          Prefix -> False
          Infix  -> True
    -- hasTrailingParen <- hasAnnKeywordComment ltycl AnnCloseP
    -- let parenWrapper = if hasTrailingParen
    --       then appSep . docWrapNodeRest ltycl
    --       else id
    let wrapNodeRest = docWrapNodeRest ltycl
    docWrapNodePrior ltycl
      $ layoutSynDecl isInfix wrapNodeRest name (hsq_explicit vars) typ
  DataDecl _ext name tyVars _ dataDefn ->
    layoutDataDecl ltycl name tyVars dataDefn
  _ -> briDocByExactNoComment ltycl

layoutSynDecl
  :: Bool
  -> (ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered)
  -> Located (IdP GhcPs)
  -> [LHsTyVarBndr () GhcPs]
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
        let needsParens = not (null rest) || hasOwnParens
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

layoutTyVarBndr :: Bool -> ToBriDoc (HsTyVarBndr ())
layoutTyVarBndr needsSep lbndr@(L _ bndr) = do
  docWrapNodePrior lbndr $ case bndr of
    UserTyVar _ _ name -> do
      nameStr <- lrdrNameToTextAnn name
      docSeq $ [docSeparator | needsSep] ++ [docLit nameStr]
    KindedTyVar _ _ name kind -> do
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



layoutTyFamInstDecl
  :: Data.Data.Data a
  => Bool
  -> Located a
  -> TyFamInstDecl GhcPs
  -> ToBriDocM BriDocNumbered
layoutTyFamInstDecl inClass outerNode tfid = do
  let
    FamEqn _ name bndrsMay pats _fixity typ = hsib_body $ tfid_eqn tfid
    -- bndrsMay isJust e.g. with
    --   type instance forall a . MyType (Maybe a) = Either () a
    innerNode = outerNode
  docWrapNodePrior outerNode $ do
    nameStr   <- lrdrNameToTextAnn name
    needsParens <- hasAnnKeyword outerNode AnnOpenP
    let
      instanceDoc = if inClass
        then docLit $ Text.pack "type"
        else docSeq
          [appSep . docLit $ Text.pack "type", docLit $ Text.pack "instance"]
      makeForallDoc :: [LHsTyVarBndr () GhcPs] -> ToBriDocM BriDocNumbered
      makeForallDoc bndrs = do
        bndrDocs <- layoutTyVarBndrs bndrs
        docSeq
          (  [docLit (Text.pack "forall")]
          ++ processTyVarBndrsSingleline bndrDocs
          )
      lhs =
        docWrapNode innerNode
          .  docSeq
          $  [appSep instanceDoc]
          ++ [ makeForallDoc foralls | Just foralls <- [bndrsMay] ]
          ++ [ docParenL | needsParens ]
          ++ [appSep $ docWrapNode name $ docLit nameStr]
          ++ intersperse docSeparator (layoutHsTyPats pats)
          ++ [ docParenR | needsParens ]
    hasComments <- (||)
      <$> hasAnyRegularCommentsConnected outerNode
      <*> hasAnyRegularCommentsRest innerNode
    typeDoc <- docSharedWrapper layoutType typ
    layoutLhsAndType hasComments lhs "=" typeDoc


layoutHsTyPats :: [HsArg (LHsType GhcPs) (LHsKind GhcPs)] -> [ToBriDocM BriDocNumbered]
layoutHsTyPats pats = pats <&> \case
  HsValArg tm     -> layoutType tm
  HsTypeArg _l ty -> docSeq [docLit $ Text.pack "@", layoutType ty]
    -- we ignore the SourceLoc here.. this LPat not being (L _ Pat{}) change
    -- is a bit strange. Hopefully this does not ignore any important
    -- annotations.
  HsArgPar _l     -> error "brittany internal error: HsArgPar{}"

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
  $  docSetIndentLevel
  $  docSortedLines
  $  fmap layoutAndLocateSig          (cid_sigs cid)
  ++ fmap layoutAndLocateBind         (bagToList $ cid_binds cid)
  ++ fmap layoutAndLocateTyFamInsts   (cid_tyfam_insts cid)
  ++ fmap layoutAndLocateDataFamInsts (cid_datafam_insts cid)
  ]
 where
  layoutInstanceHead :: ToBriDocM BriDocNumbered
  layoutInstanceHead =
    briDocByExactNoComment
      $   InstD NoExtField
      .   ClsInstD NoExtField
      .   removeChildren
      <$> lcid

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
    allocateNode . BDFLines . fmap unLoc . List.sortOn (ExactPrint.rs . getLoc) =<< sequence l

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
  layoutAndLocateTyFamInsts ltfid@(L loc tfid) =
    L loc <$> layoutTyFamInstDecl True ltfid tfid

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
        || (Text.pack "newtype" `Text.isPrefixOf` t')
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
  runFilteredAlternative $ do
    -- (separators probably are "=" or "::")
    -- lhs = type
    -- lhs :: type
    addAlternativeCond (not hasComments) $ docSeq
      [lhs, docSeparator, docLitS sep, docSeparator, docForceSingleline typeDoc]
    -- lhs
    --   :: typeA
    --   -> typeB
    -- lhs
    --   =  typeA
    --   -> typeB
    addAlternative $ docAddBaseY BrIndentRegular $ docPar lhs $ docCols
      ColTyOpPrefix
      [ appSep $ docLitS sep
      , docAddBaseY (BrIndentSpecial (length sep + 1)) typeDoc
      ]
