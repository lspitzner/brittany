{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr
  ( layoutExpr
  , litBriDoc
  , overLitValBriDoc
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Config.Types

import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types

import           GHC ( runGhc, GenLocated(L), SrcSpan, moduleNameString, AnnKeywordId(..), RdrName(..) )
import           HsSyn
import           Name
import qualified FastString
import           BasicTypes

import           Language.Haskell.Brittany.Internal.Utils
import           Language.Haskell.Brittany.Internal.Layouters.Pattern
import           Language.Haskell.Brittany.Internal.Layouters.Decl
import           Language.Haskell.Brittany.Internal.Layouters.Stmt
import           Language.Haskell.Brittany.Internal.Layouters.Type



layoutExpr :: ToBriDoc HsExpr
layoutExpr lexpr@(L _ expr) = do
  indentPolicy <- mAsk
    <&> _conf_layout
    .>  _lconfig_indentPolicy
    .>  confUnpack
  let allowFreeIndent = indentPolicy == IndentPolicyFree
  docWrapNode lexpr $ case expr of
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsVar _ vname -> do
#else
    HsVar vname -> do
#endif
      docLit =<< lrdrNameToTextAnn vname
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsUnboundVar _ var -> case var of
#else
    HsUnboundVar var -> case var of
#endif
      OutOfScope oname _ -> docLit $ Text.pack $ occNameString oname
      TrueExprHole oname -> docLit $ Text.pack $ occNameString oname
    HsRecFld{} -> do
      -- TODO
      briDocByExactInlineOnly "HsRecFld" lexpr
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsOverLabel _ext _reboundFromLabel name ->
#elif MIN_VERSION_ghc(8,2,0) /* ghc-8.2 8.4 */
    HsOverLabel _reboundFromLabel name ->
#else                        /* ghc-8.0 */
    HsOverLabel name ->
#endif
      let label = FastString.unpackFS name
      in docLit . Text.pack $ '#' : label
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsIPVar _ext (HsIPName name) ->
#else
    HsIPVar (HsIPName name) ->
#endif
      let label = FastString.unpackFS name
      in docLit . Text.pack $ '?' : label
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsOverLit _ olit -> do
#else
    HsOverLit olit -> do
#endif
      allocateNode $ overLitValBriDoc $ ol_val olit
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsLit _ lit -> do
#else
    HsLit lit -> do
#endif
      allocateNode $ litBriDoc lit
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsLam _ (MG _ (L _ [lmatch@(L _ match)]) _)
#else
    HsLam (MG (L _ [lmatch@(L _ match)]) _ _ _)
#endif
      |  pats                  <- m_pats match
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
      ,  GRHSs _ [lgrhs] llocals <- m_grhss match
#else
      ,  GRHSs [lgrhs] llocals  <- m_grhss match
#endif
      ,  L _ EmptyLocalBinds {} <- llocals
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
      ,  L _ (GRHS _ [] body)     <- lgrhs
#else
      ,  L _ (GRHS [] body)     <- lgrhs
#endif
      -> do
      patDocs <- zip (True : repeat False) pats `forM` \(isFirst, p) ->
        fmap return $ do
          -- this code could be as simple as `colsWrapPat =<< layoutPat p`
          -- if it was not for the following two cases:
          -- \ !x -> x
          -- \ ~x -> x
          -- These make it necessary to special-case an additional separator.
          -- (TODO: we create a BDCols here, but then make it ineffective
          -- by wrapping it in docSeq below. We _could_ add alignments for
          -- stuff like lists-of-lambdas. Nothing terribly important..)
          let shouldPrefixSeparator = case p of
                (ghcDL -> L _ LazyPat{}) -> isFirst
                (ghcDL -> L _ BangPat{}) -> isFirst
                _               -> False
          patDocSeq <- layoutPat p
          fixed <- case Seq.viewl patDocSeq of
            p1 Seq.:< pr | shouldPrefixSeparator -> do
              p1' <- docSeq [docSeparator, pure p1]
              pure (p1' Seq.<| pr)
            _ -> pure patDocSeq
          colsWrapPat fixed
      bodyDoc <- docAddBaseY BrIndentRegular <$> docSharedWrapper layoutExpr body
      let funcPatternPartLine =
            docCols ColCasePattern
              (patDocs <&> (\p -> docSeq [docForceSingleline p, docSeparator]))
      docAlt
        [ -- single line
          docSeq
          [ docLit $ Text.pack "\\"
          , docWrapNode lmatch $ docForceSingleline funcPatternPartLine
          , appSep $ docLit $ Text.pack "->"
          , docWrapNode lgrhs $ docForceSingleline bodyDoc
          ]
          -- double line
        , docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ docPar
          (docSeq
            [ docLit $ Text.pack "\\"
            , docWrapNode lmatch $ appSep $ docForceSingleline funcPatternPartLine
            , docLit $ Text.pack "->"
            ])
          (docWrapNode lgrhs $ docForceSingleline bodyDoc)
          -- wrapped par spacing
        , docSetParSpacing
        $ docSeq
          [ docLit $ Text.pack "\\"
          , docWrapNode lmatch $ docForceSingleline funcPatternPartLine
          , appSep $ docLit $ Text.pack "->"
          , docWrapNode lgrhs $ docForceParSpacing bodyDoc
          ]
          -- conservative
        , docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ docPar
          (docSeq
            [ docLit $ Text.pack "\\"
            , docWrapNode lmatch $ appSep $ docForceSingleline funcPatternPartLine
            , docLit $ Text.pack "->"
            ])
          (docWrapNode lgrhs $ docNonBottomSpacing bodyDoc)
        ]
    HsLam{} ->
      unknownNodeError "HsLam too complex" lexpr
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsLamCase _ XMatchGroup{} ->
      error "brittany internal error: HsLamCase XMatchGroup"
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsLamCase _ (MG _ (L _ []) _) -> do
#elif MIN_VERSION_ghc(8,2,0) /* ghc-8.2 8.4*/
    HsLamCase (MG (L _ []) _ _ _) -> do
#else                        /* ghc-8.0 */
    HsLamCase _ (MG (L _ []) _ _ _) -> do
#endif
      docSetParSpacing $ docAddBaseY BrIndentRegular $
        (docLit $ Text.pack "\\case {}")
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsLamCase _ (MG _ lmatches@(L _ matches) _) -> do
#elif MIN_VERSION_ghc(8,2,0) /* ghc-8.2 8.4*/
    HsLamCase (MG lmatches@(L _ matches) _ _ _) -> do
#else                        /* ghc-8.0 */
    HsLamCase _ (MG lmatches@(L _ matches) _ _ _) -> do
#endif
      binderDoc   <- docLit $ Text.pack "->"
      funcPatDocs <- docWrapNode lmatches
        $ layoutPatternBind Nothing binderDoc `mapM` matches
      docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
        (docLit $ Text.pack "\\case")
        (docSetBaseAndIndent $ docNonBottomSpacing $ docLines $ return <$> funcPatDocs)
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsApp _ exp1@(L _ HsApp{}) exp2 -> do
#else
    HsApp exp1@(L _ HsApp{}) exp2 -> do
#endif
      let gather :: [LHsExpr GhcPs] -> LHsExpr GhcPs -> (LHsExpr GhcPs, [LHsExpr GhcPs])
          gather list = \case
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
            L _ (HsApp _ l r) -> gather (r:list) l
#else
            L _ (HsApp l r) -> gather (r:list) l
#endif
            x               -> (x, list)
      let (headE, paramEs) = gather [exp2] exp1
      let colsOrSequence = case headE of
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
            L _ (HsVar _ (L _ (Unqual occname))) ->
#else
            L _ (HsVar (L _ (Unqual occname))) ->
#endif
              docCols (ColApp $ Text.pack $ occNameString occname)
            _ -> docSeq
      headDoc <- docSharedWrapper layoutExpr headE
      paramDocs <- docSharedWrapper layoutExpr `mapM` paramEs
      hasComments <- hasAnyCommentsConnected exp2
      runFilteredAlternative $ do
        -- foo x y
        addAlternativeCond (not hasComments)
          $ colsOrSequence
          $ appSep (docForceSingleline headDoc)
          : spacifyDocs (docForceSingleline <$> paramDocs)
        -- foo x
        --     y
        addAlternativeCond allowFreeIndent
          $ docSeq
          [ appSep (docForceSingleline headDoc)
          , docSetBaseY
          $ docAddBaseY BrIndentRegular
          $ docLines
          $ docForceSingleline <$> paramDocs
          ]
        -- foo
        --   x
        --   y
        addAlternative
          $ docSetParSpacing
          $ docAddBaseY BrIndentRegular
          $ docPar
            (docForceSingleline headDoc)
            ( docNonBottomSpacing
            $ docLines paramDocs
            )
        -- ( multi
        --   line
        --   function
        -- )
        --   x
        --   y
        addAlternative
          $ docAddBaseY BrIndentRegular
          $ docPar
            headDoc
            ( docNonBottomSpacing
            $ docLines paramDocs
            )
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsApp _ exp1 exp2 -> do
#else
    HsApp exp1 exp2 -> do
#endif
      -- TODO: if expDoc1 is some literal, we may want to create a docCols here.
      expDoc1 <- docSharedWrapper layoutExpr exp1
      expDoc2 <- docSharedWrapper layoutExpr exp2
      docAlt
        [ -- func arg
          docSeq [appSep $ docForceSingleline expDoc1, docForceSingleline expDoc2]
        , -- func argline1
          --   arglines
          -- e.g.
          -- func Abc
          --   { member1 = True
          --   , member2 = 13
          --   }
          docSetParSpacing -- this is most likely superfluous because
                           -- this is a sequence of a one-line and a par-space
                           -- anyways, so it is _always_ par-spaced.
        $ docAddBaseY BrIndentRegular
        $ docSeq
          [ appSep $ docForceSingleline expDoc1
          , docForceParSpacing expDoc2
          ]
        , -- func
          --   arg
          docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ docPar
          (docForceSingleline expDoc1)
          (docNonBottomSpacing expDoc2)
        , -- fu
          --   nc
          --   ar
          --     gument
          docAddBaseY BrIndentRegular
        $ docPar
          expDoc1
          expDoc2
        ]
#if MIN_VERSION_ghc(8,8,0)   /* ghc-8.8 */
    HsAppType _ _ XHsWildCardBndrs{} ->
      error "brittany internal error: HsAppType XHsWildCardBndrs"
    HsAppType _ exp1 (HsWC _ ty1) -> do
#elif MIN_VERSION_ghc(8,6,0) /* ghc-8.6 */
    HsAppType XHsWildCardBndrs{} _ ->
      error "brittany internal error: HsAppType XHsWildCardBndrs"
    HsAppType (HsWC _ ty1) exp1 -> do
#elif MIN_VERSION_ghc(8,2,0) /* ghc-8.2 8.4 */
    HsAppType exp1 (HsWC _ ty1) -> do
#else                        /* ghc-8.0 */
    HsAppType exp1 (HsWC _ _ ty1) -> do
#endif
      t <- docSharedWrapper layoutType ty1
      e <- docSharedWrapper layoutExpr exp1
      docAlt
        [ docSeq
            [ docForceSingleline e
            , docSeparator
            , docLit $ Text.pack "@"
            , docForceSingleline t
            ]
        , docPar
            e
            (docSeq [docLit $ Text.pack "@", t ])
        ]
#if !MIN_VERSION_ghc(8,6,0)   /* ghc-8.0 8.2 8.4 */
    HsAppTypeOut{} -> do
      -- TODO
      briDocByExactInlineOnly "HsAppTypeOut{}" lexpr
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    OpApp _ expLeft@(L _ OpApp{}) expOp expRight -> do
#else
    OpApp expLeft@(L _ OpApp{}) expOp _ expRight -> do
#endif
      let gather :: [(LHsExpr GhcPs, LHsExpr GhcPs)] -> LHsExpr GhcPs -> (LHsExpr GhcPs, [(LHsExpr GhcPs, LHsExpr GhcPs)])
          gather opExprList = \case
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
            (L _ (OpApp _ l1 op1 r1)) -> gather ((op1, r1): opExprList) l1
#else
            (L _ (OpApp l1 op1 _ r1)) -> gather ((op1, r1): opExprList) l1
#endif
            final -> (final, opExprList)
          (leftOperand, appList) = gather [] expLeft
      leftOperandDoc <- docSharedWrapper layoutExpr leftOperand
      appListDocs <- appList `forM` \(x,y) -> [ (xD, yD)
                                              | xD <- docSharedWrapper layoutExpr x
                                              , yD <- docSharedWrapper layoutExpr y
                                              ]
      opLastDoc   <- docSharedWrapper layoutExpr expOp
      expLastDoc  <- docSharedWrapper layoutExpr expRight
      allowSinglelinePar <- do
        hasComLeft <- hasAnyCommentsConnected expLeft
        hasComOp   <- hasAnyCommentsConnected expOp
        pure $ not hasComLeft && not hasComOp
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
      let allowPar = case (expOp, expRight) of
            (L _ (HsVar _ (L _ (Unqual occname))), _)
              | occNameString occname == "$" -> True
            (_, L _ (HsApp _ _ (L _ HsVar{}))) -> False
            _ -> True
#else
      let allowPar = case (expOp, expRight) of
            (L _ (HsVar (L _ (Unqual occname))), _)
              | occNameString occname == "$" -> True
            (_, L _ (HsApp _ (L _ HsVar{}))) -> False
            _ -> True
#endif
      runFilteredAlternative $ do
        -- > one + two + three
        -- or
        -- > one + two + case x of
        -- >   _ -> three
        addAlternativeCond allowSinglelinePar
          $ docSeq
          [ appSep $ docForceSingleline leftOperandDoc
          , docSeq
          $ appListDocs <&> \(od, ed) -> docSeq
              [ appSep $ docForceSingleline od
              , appSep $ docForceSingleline ed
              ]
          , appSep $ docForceSingleline opLastDoc
          , (if allowPar then docForceParSpacing else docForceSingleline)
              expLastDoc
          ]
        -- this case rather leads to some unfortunate layouting than to anything
        -- useful; disabling for now. (it interfers with cols stuff.)
        -- addAlternative
        --   $ docSetBaseY
        --   $ docPar
        --     leftOperandDoc
        --     ( docLines
        --      $ (appListDocs <&> \(od, ed) -> docCols ColOpPrefix [appSep od, docSetBaseY ed])
        --       ++ [docCols ColOpPrefix [appSep opLastDoc, docSetBaseY expLastDoc]]
        --     )
        -- > one
        -- >   + two
        -- >   + three
        addAlternative $
          docPar
            leftOperandDoc
            ( docLines
            $ (appListDocs <&> \(od, ed) -> docCols ColOpPrefix [appSep od, docSetBaseY ed])
              ++ [docCols ColOpPrefix [appSep opLastDoc, docSetBaseY expLastDoc]]
            )
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    OpApp _ expLeft expOp expRight -> do
#else
    OpApp expLeft expOp _ expRight -> do
#endif
      expDocLeft  <- docSharedWrapper layoutExpr expLeft
      expDocOp    <- docSharedWrapper layoutExpr expOp
      expDocRight <- docSharedWrapper layoutExpr expRight
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
      let allowPar = case (expOp, expRight) of
            (L _ (HsVar _ (L _ (Unqual occname))), _)
              | occNameString occname == "$" -> True
            (_, L _ (HsApp _ _ (L _ HsVar{}))) -> False
            _ -> True
#else
      let allowPar = case (expOp, expRight) of
            (L _ (HsVar (L _ (Unqual occname))), _)
              | occNameString occname == "$" -> True
            (_, L _ (HsApp _ (L _ HsVar{}))) -> False
            _ -> True
#endif
      runFilteredAlternative $ do
        -- one-line
        addAlternative
          $ docSeq
          [ appSep $ docForceSingleline expDocLeft
          , appSep $ docForceSingleline expDocOp
          , docForceSingleline expDocRight
          ]
        -- -- line + freely indented block for right expression
        -- addAlternative
        --   $ docSeq
        --   [ appSep $ docForceSingleline expDocLeft
        --   , appSep $ docForceSingleline expDocOp
        --   , docSetBaseY $ docAddBaseY BrIndentRegular expDocRight
        --   ]
        -- two-line
        addAlternative
          $ docAddBaseY BrIndentRegular
          $ docPar
              expDocLeft -- TODO: this is not forced to single-line, which has
                         -- certain.. interesting consequences.
                         -- At least, the "two-line" label is not entirely
                         -- accurate.
              ( docForceSingleline
              $ docCols ColOpPrefix [appSep $ expDocOp, docSetBaseY expDocRight]
              )
        -- one-line + par
        addAlternativeCond allowPar
          $ docSeq
          [ appSep $ docForceSingleline expDocLeft
          , appSep $ docForceSingleline expDocOp
          , docForceParSpacing expDocRight
          ]
        -- more lines
        addAlternative
          $ docAddBaseY BrIndentRegular
          $ docPar
              expDocLeft
              (docCols ColOpPrefix [appSep expDocOp, docSetBaseY expDocRight])
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    NegApp _ op _ -> do
#else
    NegApp op _ -> do
#endif
      opDoc <- docSharedWrapper layoutExpr op
      docSeq [ docLit $ Text.pack "-"
             , opDoc
             ]
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsPar _ innerExp -> do
#else
    HsPar innerExp -> do
#endif
      innerExpDoc <- docSharedWrapper (docWrapNode lexpr . layoutExpr) innerExp
      docAlt
        [ docSeq
          [ docLit $ Text.pack "("
          , docForceSingleline innerExpDoc
          , docLit $ Text.pack ")"
          ]
        , docSetBaseY $ docLines
          [ docCols ColOpPrefix
            [ docLit $ Text.pack "("
            , docAddBaseY (BrIndentSpecial 2) innerExpDoc
            ]
          , docLit $ Text.pack ")"
          ]
        ]
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    SectionL _ left op -> do -- TODO: add to testsuite
#else
    SectionL left op -> do -- TODO: add to testsuite
#endif
      leftDoc <- docSharedWrapper layoutExpr left
      opDoc   <- docSharedWrapper layoutExpr op
      docSeq [leftDoc, docSeparator, opDoc]
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    SectionR _ op right -> do -- TODO: add to testsuite
#else
    SectionR op right -> do -- TODO: add to testsuite
#endif
      opDoc    <- docSharedWrapper layoutExpr op
      rightDoc <- docSharedWrapper layoutExpr right
      docSeq [opDoc, docSeparator, rightDoc]
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    ExplicitTuple _ args boxity -> do
#else
    ExplicitTuple args boxity -> do
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
      let argExprs = args <&> \arg -> case arg of
            (L _ (Present _ e)) -> (arg, Just e);
            (L _ (Missing NoExt)) -> (arg, Nothing)
            (L _ XTupArg{}) -> error "brittany internal error: XTupArg"
#else
      let argExprs = args <&> \arg -> case arg of
            (L _ (Present e)) -> (arg, Just e);
            (L _ (Missing PlaceHolder)) -> (arg, Nothing)
#endif
      argDocs <- forM argExprs
        $ docSharedWrapper
        $ \(arg, exprM) -> docWrapNode arg $ maybe docEmpty layoutExpr exprM
      hasComments <- orM
        ( hasCommentsBetween lexpr  AnnOpenP AnnCloseP
        : map hasAnyCommentsBelow args
        )
      let (openLit, closeLit) = case boxity of
            Boxed   -> (docLit $ Text.pack "(", docLit $ Text.pack ")")
            Unboxed -> (docParenHashLSep, docParenHashRSep)
      case splitFirstLast argDocs of
        FirstLastEmpty -> docSeq
          [ openLit
          , docNodeAnnKW lexpr (Just AnnOpenP) closeLit
          ]
        FirstLastSingleton e -> docAlt
          [ docCols ColTuple
            [ openLit
            , docNodeAnnKW lexpr (Just AnnOpenP) $ docForceSingleline e
            , closeLit
            ]
          , docSetBaseY $ docLines
            [ docSeq
              [ openLit
              , docNodeAnnKW lexpr (Just AnnOpenP) $ docForceSingleline e
              ]
            , closeLit
            ]
          ]
        FirstLast e1 ems eN -> runFilteredAlternative $ do
          addAlternativeCond (not hasComments)
            $  docCols ColTuple
            $  [docSeq [openLit, docForceSingleline e1]]
            ++ (ems <&> \e -> docSeq [docCommaSep, docForceSingleline e])
            ++ [docSeq [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenP) (docForceSingleline eN), closeLit]]
          addAlternative $
            let
              start = docCols ColTuples
                        [appSep openLit, e1]
              linesM = ems <&> \d ->
                      docCols ColTuples [docCommaSep, d]
              lineN = docCols ColTuples [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenP) eN]
              end   = closeLit
            in docSetBaseY $ docLines $ [start] ++ linesM ++ [lineN, end]
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsCase _ _ XMatchGroup{} ->
      error "brittany internal error: HsCase XMatchGroup"
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsCase _ cExp (MG _ (L _ []) _) -> do
#else
    HsCase cExp (MG (L _ []) _ _ _) -> do
#endif
      cExpDoc <- docSharedWrapper layoutExpr cExp
      docAlt
        [ docAddBaseY BrIndentRegular
        $ docSeq
            [ appSep $ docLit $ Text.pack "case"
            , appSep $ docForceSingleline cExpDoc
            , docLit $ Text.pack "of {}"
            ]
        , docPar
            ( docAddBaseY BrIndentRegular
            $ docPar (docLit $ Text.pack "case") cExpDoc
            )
            (docLit $ Text.pack "of {}")
        ]
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsCase _ cExp (MG _ lmatches@(L _ matches) _) -> do
#else
    HsCase cExp (MG lmatches@(L _ matches) _ _ _) -> do
#endif
      cExpDoc <- docSharedWrapper layoutExpr cExp
      binderDoc <- docLit $ Text.pack "->"
      funcPatDocs <- docWrapNode lmatches
        $ layoutPatternBind Nothing binderDoc `mapM` matches
      docAlt
        [ docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ docPar
          ( docSeq
            [ appSep $ docLit $ Text.pack "case"
            , appSep $ docForceSingleline cExpDoc
            , docLit $ Text.pack "of"
            ])
          (docSetBaseAndIndent $ docNonBottomSpacing $ docLines $ return <$> funcPatDocs)
        , docPar
            ( docAddBaseY BrIndentRegular
            $ docPar (docLit $ Text.pack "case") cExpDoc
            )
            ( docAddBaseY BrIndentRegular
            $ docPar (docLit $ Text.pack "of")
              (docSetBaseAndIndent $ docNonBottomSpacing $ docLines $ return <$> funcPatDocs)
            )
        ]
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsIf _ _ ifExpr thenExpr elseExpr -> do
#else
    HsIf _ ifExpr thenExpr elseExpr -> do
#endif
      ifExprDoc   <- docSharedWrapper layoutExpr ifExpr
      thenExprDoc <- docSharedWrapper layoutExpr thenExpr
      elseExprDoc <- docSharedWrapper layoutExpr elseExpr
      hasComments <- hasAnyCommentsBelow lexpr
      let maySpecialIndent =
            case indentPolicy of
              IndentPolicyLeft -> BrIndentRegular
              IndentPolicyMultiple -> BrIndentRegular
              IndentPolicyFree -> BrIndentSpecial 3
      -- TODO: some of the alternatives (especially last and last-but-one)
      -- overlap.
      docSetIndentLevel $ runFilteredAlternative $ do
        -- if _ then _ else _
        addAlternativeCond (not hasComments)
          $ docSeq
          [ appSep $ docLit $ Text.pack "if"
          , appSep $ docForceSingleline ifExprDoc
          , appSep $ docLit $ Text.pack "then"
          , appSep $ docForceSingleline thenExprDoc
          , appSep $ docLit $ Text.pack "else"
          , docForceSingleline elseExprDoc
          ]
        -- either
        --   if expr
        --   then foo
        --     bar
        --   else foo
        --     bar
        -- or
        --   if expr
        --   then
        --     stuff
        --   else
        --     stuff
        -- note that this has par-spacing
        addAlternative
          $ docSetParSpacing
          $ docAddBaseY BrIndentRegular
          $ docPar
              ( docSeq
                [ docNodeAnnKW lexpr Nothing $ appSep $ docLit $ Text.pack "if"
                , docNodeAnnKW lexpr (Just AnnIf) $ docForceSingleline ifExprDoc
                ])
              (docLines
                [ docAddBaseY BrIndentRegular
                $ docNodeAnnKW lexpr (Just AnnThen)
                $ docNonBottomSpacing $ docAlt
                  [ docSeq [appSep $ docLit $ Text.pack "then", docForceParSpacing thenExprDoc]
                  , docAddBaseY BrIndentRegular
                  $ docPar (docLit $ Text.pack "then") thenExprDoc
                  ]
                , docAddBaseY BrIndentRegular
                $ docNonBottomSpacing $ docAlt
                  [ docSeq [appSep $ docLit $ Text.pack "else", docForceParSpacing elseExprDoc]
                  , docAddBaseY BrIndentRegular
                  $ docPar (docLit $ Text.pack "else") elseExprDoc
                  ]
                ])
        -- either
        --   if multi
        --      line
        --      condition
        --   then foo
        --     bar
        --   else foo
        --     bar
        -- or
        --   if multi
        --      line
        --      condition
        --   then
        --     stuff
        --   else
        --     stuff
        -- note that this does _not_ have par-spacing
        addAlternative
          $ docAddBaseY BrIndentRegular
          $ docPar
              ( docAddBaseY maySpecialIndent
              $ docSeq
                [ docNodeAnnKW lexpr Nothing $ appSep $ docLit $ Text.pack "if"
                , docNodeAnnKW lexpr (Just AnnIf) $ ifExprDoc
                ])
              (docLines
                [ docAddBaseY BrIndentRegular
                $ docNodeAnnKW lexpr (Just AnnThen)
                $ docAlt
                  [ docSeq [appSep $ docLit $ Text.pack "then", docForceParSpacing thenExprDoc]
                  , docAddBaseY BrIndentRegular
                  $ docPar (docLit $ Text.pack "then") thenExprDoc
                  ]
                , docAddBaseY BrIndentRegular
                $ docAlt
                  [ docSeq [appSep $ docLit $ Text.pack "else", docForceParSpacing elseExprDoc]
                  , docAddBaseY BrIndentRegular
                  $ docPar (docLit $ Text.pack "else") elseExprDoc
                  ]
                ])
        addAlternative
          $ docSetBaseY
          $ docLines
            [ docAddBaseY maySpecialIndent
            $ docSeq
              [ docNodeAnnKW lexpr Nothing $ appSep $ docLit $ Text.pack "if"
              , docNodeAnnKW lexpr (Just AnnIf) $ ifExprDoc
              ]
            , docNodeAnnKW lexpr (Just AnnThen)
            $ docAddBaseY BrIndentRegular
            $ docPar (docLit $ Text.pack "then") thenExprDoc
            , docAddBaseY BrIndentRegular
            $ docPar (docLit $ Text.pack "else") elseExprDoc
            ]
    HsMultiIf _ cases -> do
      clauseDocs  <- cases `forM` layoutGrhs
      binderDoc   <- docLit $ Text.pack "->"
      hasComments <- hasAnyCommentsBelow lexpr
      docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
        (docLit $ Text.pack "if")
        (layoutPatternBindFinal Nothing binderDoc Nothing clauseDocs Nothing hasComments)
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsLet _ binds exp1 -> do
#else
    HsLet binds exp1 -> do
#endif
      expDoc1     <- docSharedWrapper layoutExpr exp1
      -- We jump through some ugly hoops here to ensure proper sharing.
      hasComments <- hasAnyCommentsBelow lexpr
      mBindDocs   <- fmap (fmap (fmap pure)) $ layoutLocalBinds binds
      let
        ifIndentFreeElse :: a -> a -> a
        ifIndentFreeElse x y =
          case indentPolicy of
            IndentPolicyLeft -> y
            IndentPolicyMultiple -> y
            IndentPolicyFree -> x
      -- this `docSetBaseAndIndent` might seem out of place (especially the
      -- Indent part; setBase is necessary due to the use of docLines below),
      -- but is here due to ghc-exactprint's DP handling of "let" in
      -- particular.
      -- Just pushing another indentation level is a straightforward approach
      -- to making brittany idempotent, even though the result is non-optimal
      -- if "let" is moved horizontally as part of the transformation, as the
      -- comments before the first let item are moved horizontally with it.
      docSetBaseAndIndent $ case mBindDocs of
        Just [bindDoc] -> runFilteredAlternative $ do
          addAlternativeCond (not hasComments) $ docSeq
            [ appSep $ docLit $ Text.pack "let"
            , docNodeAnnKW lexpr (Just AnnLet)
              $ appSep $ docForceSingleline bindDoc
            , appSep $ docLit $ Text.pack "in"
            , docForceSingleline expDoc1
            ]
          addAlternative $ docLines
            [ docNodeAnnKW lexpr (Just AnnLet)
            $ docAlt
                [ docSeq
                    [ appSep $ docLit $ Text.pack "let"
                    , ifIndentFreeElse docSetBaseAndIndent docForceSingleline
                    $ bindDoc
                    ]
                , docAddBaseY BrIndentRegular
                $ docPar
                  (docLit $ Text.pack "let")
                  (docSetBaseAndIndent bindDoc)
                ]
            , docAlt
                [ docSeq
                    [ appSep $ docLit $ Text.pack $ ifIndentFreeElse "in " "in"
                    , ifIndentFreeElse docSetBaseAndIndent docForceSingleline expDoc1
                    ]
                , docAddBaseY BrIndentRegular
                $ docPar
                  (docLit $ Text.pack "in")
                  (docSetBaseY expDoc1)
                ]
            ]
        Just bindDocs@(_:_) -> runFilteredAlternative $ do
          --either
          --  let
          --    a = b
          --    c = d
          --  in foo
          --    bar
          --    baz
          --or
          --  let
          --    a = b
          --    c = d
          --  in
          --    fooooooooooooooooooo
          let noHangingBinds =
                [ docNonBottomSpacing $ docAddBaseY BrIndentRegular
                $ docPar
                  (docLit $ Text.pack "let")
                  (docSetBaseAndIndent $ docLines bindDocs)
                , docSeq
                  [ docLit $ Text.pack "in "
                  , docAddBaseY BrIndentRegular
                  $ docForceParSpacing expDoc1
                  ]
                ]
          addAlternative $ case indentPolicy of
            IndentPolicyLeft     -> docLines noHangingBinds
            IndentPolicyMultiple -> docLines noHangingBinds
            IndentPolicyFree     -> docLines
              [ docNodeAnnKW lexpr (Just AnnLet)
              $ docSeq
                [ appSep $ docLit $ Text.pack "let"
                , docSetBaseAndIndent $ docLines bindDocs
                ]
              , docSeq
                [ appSep $ docLit $ Text.pack "in "
                , docSetBaseY expDoc1
                ]
              ]
          addAlternative
            $ docLines
            [ docNodeAnnKW lexpr (Just AnnLet)
            $ docAddBaseY BrIndentRegular
            $ docPar
              (docLit $ Text.pack "let")
              (docSetBaseAndIndent $ docLines $ bindDocs)
            , docAddBaseY BrIndentRegular
            $ docPar
              (docLit $ Text.pack "in")
              (docSetBaseY $ expDoc1)
            ]
        _ -> docSeq [appSep $ docLit $ Text.pack "let in", expDoc1]
      -- docSeq [appSep $ docLit "let in", expDoc1]
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsDo _ stmtCtx (L _ stmts) -> case stmtCtx of
#else
    HsDo stmtCtx (L _ stmts) _ -> case stmtCtx of
#endif
      DoExpr -> do
        stmtDocs <- docSharedWrapper layoutStmt `mapM` stmts
        docSetParSpacing
          $ docAddBaseY BrIndentRegular
          $ docPar
              (docLit $ Text.pack "do")
              (docSetBaseAndIndent $ docNonBottomSpacing $ docLines stmtDocs)
      MDoExpr -> do
        stmtDocs <- docSharedWrapper layoutStmt `mapM` stmts
        docSetParSpacing
          $ docAddBaseY BrIndentRegular
          $ docPar
              (docLit $ Text.pack "mdo")
              (docSetBaseAndIndent $ docNonBottomSpacing $ docLines stmtDocs)
      x | case x of { ListComp -> True
                    ; MonadComp -> True
                    ; _ -> False } -> do
        stmtDocs    <- docSharedWrapper layoutStmt `mapM` stmts
        hasComments <- hasAnyCommentsBelow lexpr
        runFilteredAlternative $ do
          addAlternativeCond (not hasComments)
            $ docSeq
            [ docNodeAnnKW lexpr Nothing
            $ appSep
            $ docLit
            $ Text.pack "["
            , docNodeAnnKW lexpr (Just AnnOpenS)
            $ appSep
            $ docForceSingleline
            $ List.last stmtDocs
            , appSep $ docLit $ Text.pack "|"
            , docSeq $ List.intersperse docCommaSep
            $ docForceSingleline <$> List.init stmtDocs
            , docLit $ Text.pack " ]"
            ]
          addAlternative $
            let
              start = docCols ColListComp
                        [ docNodeAnnKW lexpr Nothing
                        $ appSep $ docLit $ Text.pack "["
                        , docSetBaseY
                        $ docNodeAnnKW lexpr (Just AnnOpenS)
                        $ List.last stmtDocs
                        ]
              (s1:sM) = List.init stmtDocs
              line1 = docCols ColListComp
                        [appSep $ docLit $ Text.pack "|", s1]
              lineM = sM <&> \d ->
                      docCols ColListComp [docCommaSep, d]
              end   = docLit $ Text.pack "]"
            in docSetBaseY $ docLines $ [start, line1] ++ lineM ++ [end]
      _ -> do
        -- TODO
        unknownNodeError "HsDo{} unknown stmtCtx" lexpr
    ExplicitList _ _ elems@(_:_) -> do
      elemDocs    <- elems `forM` docSharedWrapper layoutExpr
      hasComments <- hasAnyCommentsBelow lexpr
      case splitFirstLast elemDocs of
        FirstLastEmpty -> docSeq
          [ docLit $ Text.pack "["
          , docNodeAnnKW lexpr (Just AnnOpenS) $ docLit $ Text.pack "]"
          ]
        FirstLastSingleton e -> docAlt
          [ docSeq
            [ docLit $ Text.pack "["
            , docNodeAnnKW lexpr (Just AnnOpenS) $ docForceSingleline e
            , docLit $ Text.pack "]"
            ]
          , docSetBaseY $ docLines
            [ docSeq
              [ docLit $ Text.pack "["
              , docSeparator
              , docSetBaseY $ docNodeAnnKW lexpr (Just AnnOpenS) e
              ]
            , docLit $ Text.pack "]"
            ]
          ]
        FirstLast e1 ems eN -> runFilteredAlternative $ do
          addAlternativeCond (not hasComments)
            $  docSeq
            $  [docLit $ Text.pack "["]
            ++ List.intersperse docCommaSep (docForceSingleline <$> (e1:ems ++ [docNodeAnnKW lexpr (Just AnnOpenS) eN]))
            ++ [docLit $ Text.pack "]"]
          addAlternative $
            let
              start = docCols ColList
                        [appSep $ docLit $ Text.pack "[", e1]
              linesM = ems <&> \d ->
                      docCols ColList [docCommaSep, d]
              lineN = docCols ColList [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenS) eN]
              end   = docLit $ Text.pack "]"
            in docSetBaseY $ docLines $ [start] ++ linesM ++ [lineN] ++ [end]
    ExplicitList _ _ [] ->
      docLit $ Text.pack "[]"
#if !MIN_VERSION_ghc(8,6,0)  /* ghc-8.0 8.2 8.4 */
    ExplicitPArr{} -> do
      -- TODO
      briDocByExactInlineOnly "ExplicitPArr{}" lexpr
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    RecordCon _ lname fields ->
#else
    RecordCon lname _ _ fields ->
#endif
      case fields of
        HsRecFields fs Nothing -> do
          let nameDoc = docWrapNode lname $ docLit $ lrdrNameToText lname
          rFs <- fs
            `forM` \lfield@(L _ (HsRecField (L _ fieldOcc) rFExpr pun)) -> do
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
              let FieldOcc _ lnameF = fieldOcc
#else
              let FieldOcc lnameF _ = fieldOcc
#endif
              rFExpDoc <- if pun
                then return Nothing
                else Just <$> docSharedWrapper layoutExpr rFExpr
              return $ (lfield, lrdrNameToText lnameF, rFExpDoc)
          recordExpression False indentPolicy lexpr nameDoc rFs
        HsRecFields [] (Just 0) -> do
          let t = lrdrNameToText lname
          docWrapNode lname $ docLit $ t <> Text.pack " { .. }"
        HsRecFields fs@(_:_) (Just dotdoti) | dotdoti == length fs -> do
          let nameDoc = docWrapNode lname $ docLit $ lrdrNameToText lname
          fieldDocs <- fs `forM` \fieldl@(L _ (HsRecField (L _ fieldOcc) fExpr pun)) -> do
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
            let FieldOcc _ lnameF = fieldOcc
#else
            let FieldOcc lnameF _ = fieldOcc
#endif
            fExpDoc <- if pun
              then return Nothing
              else Just <$> docSharedWrapper layoutExpr fExpr
            return (fieldl, lrdrNameToText lnameF, fExpDoc)
          recordExpression True indentPolicy lexpr nameDoc fieldDocs
        _ -> unknownNodeError "RecordCon with puns" lexpr
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    RecordUpd _ rExpr fields -> do
#else
    RecordUpd rExpr fields _ _ _ _ -> do
#endif
      rExprDoc <- docSharedWrapper layoutExpr rExpr
      rFs <- fields
        `forM` \lfield@(L _ (HsRecField (L _ ambName) rFExpr pun)) -> do
          rFExpDoc <- if pun
            then return Nothing
            else Just <$> docSharedWrapper layoutExpr rFExpr
          return $ case ambName of
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
            Unambiguous _ n -> (lfield, lrdrNameToText n, rFExpDoc)
            Ambiguous   _ n -> (lfield, lrdrNameToText n, rFExpDoc)
            XAmbiguousFieldOcc{} ->
              error "brittany internal error: XAmbiguousFieldOcc"
#else
            Unambiguous n _ -> (lfield, lrdrNameToText n, rFExpDoc)
            Ambiguous   n _ -> (lfield, lrdrNameToText n, rFExpDoc)
#endif
      recordExpression False indentPolicy lexpr rExprDoc rFs
#if MIN_VERSION_ghc(8,8,0)   /* ghc-8.6 */
    ExprWithTySig _ _ (HsWC _ XHsImplicitBndrs{}) ->
      error "brittany internal error: ExprWithTySig HsWC XHsImplicitBndrs"
    ExprWithTySig _ _ XHsWildCardBndrs{} ->
      error "brittany internal error: ExprWithTySig XHsWildCardBndrs"
    ExprWithTySig _ exp1 (HsWC _ (HsIB _ typ1)) -> do
#elif MIN_VERSION_ghc(8,6,0) /* ghc-8.6 */
    ExprWithTySig (HsWC _ XHsImplicitBndrs{}) _ ->
      error "brittany internal error: ExprWithTySig HsWC XHsImplicitBndrs"
    ExprWithTySig XHsWildCardBndrs{} _ ->
      error "brittany internal error: ExprWithTySig XHsWildCardBndrs"
    ExprWithTySig (HsWC _ (HsIB _ typ1)) exp1 -> do
#elif MIN_VERSION_ghc(8,2,0) /* ghc-8.2 8,4 */
    ExprWithTySig exp1 (HsWC _ (HsIB _ typ1 _)) -> do
#else /* ghc-8.0 */
    ExprWithTySig exp1 (HsIB _ (HsWC _ _ typ1)) -> do
#endif
      expDoc <- docSharedWrapper layoutExpr exp1
      typDoc <- docSharedWrapper layoutType typ1
      docSeq
        [ appSep expDoc
        , appSep $ docLit $ Text.pack "::"
        , typDoc
        ]
#if !MIN_VERSION_ghc(8,6,0)  /* ghc-8.0 8.2 8.4 */
    ExprWithTySigOut{} -> do
      -- TODO
      briDocByExactInlineOnly "ExprWithTySigOut{}" lexpr
#endif
    ArithSeq _ Nothing info ->
      case info of
        From e1 -> do
          e1Doc <- docSharedWrapper layoutExpr e1
          docSeq
            [ docLit $ Text.pack "["
            , appSep $ docForceSingleline e1Doc
            , docLit $ Text.pack "..]"
            ]
        FromThen e1 e2 -> do
          e1Doc <- docSharedWrapper layoutExpr e1
          e2Doc <- docSharedWrapper layoutExpr e2
          docSeq
            [ docLit $ Text.pack "["
            , docForceSingleline e1Doc
            , appSep $ docLit $ Text.pack ","
            , appSep $ docForceSingleline e2Doc
            , docLit $ Text.pack "..]"
            ]
        FromTo e1 eN -> do
          e1Doc <- docSharedWrapper layoutExpr e1
          eNDoc <- docSharedWrapper layoutExpr eN
          docSeq
            [ docLit $ Text.pack "["
            , appSep $ docForceSingleline e1Doc
            , appSep $ docLit $ Text.pack ".."
            , docForceSingleline eNDoc
            , docLit $ Text.pack "]"
            ]
        FromThenTo e1 e2 eN -> do
          e1Doc <- docSharedWrapper layoutExpr e1
          e2Doc <- docSharedWrapper layoutExpr e2
          eNDoc <- docSharedWrapper layoutExpr eN
          docSeq
            [ docLit $ Text.pack "["
            , docForceSingleline e1Doc
            , appSep $ docLit $ Text.pack ","
            , appSep $ docForceSingleline e2Doc
            , appSep $ docLit $ Text.pack ".."
            , docForceSingleline eNDoc
            , docLit $ Text.pack "]"
            ]
    ArithSeq{} ->
      briDocByExactInlineOnly "ArithSeq" lexpr
#if !MIN_VERSION_ghc(8,6,0)  /* ghc-8.0 8.2 8.4 */
    PArrSeq{} -> do
      -- TODO
      briDocByExactInlineOnly "PArrSeq{}" lexpr
#endif
    HsSCC{} -> do
      -- TODO
      briDocByExactInlineOnly "HsSCC{}" lexpr
    HsCoreAnn{} -> do
      -- TODO
      briDocByExactInlineOnly "HsCoreAnn{}" lexpr
    HsBracket{} -> do
      -- TODO
      briDocByExactInlineOnly "HsBracket{}" lexpr
    HsRnBracketOut{} -> do
      -- TODO
      briDocByExactInlineOnly "HsRnBracketOut{}" lexpr
    HsTcBracketOut{} -> do
      -- TODO
      briDocByExactInlineOnly "HsTcBracketOut{}" lexpr
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    HsSpliceE _ (HsQuasiQuote _ _ quoter _loc content) -> do
#else
    HsSpliceE (HsQuasiQuote _ quoter _loc content) -> do
#endif
      allocateNode $ BDFPlain
        (Text.pack
          $  "["
          ++ showOutputable quoter
          ++ "|"
          ++ showOutputable content
          ++ "|]")
    HsSpliceE{} -> do
      -- TODO
      briDocByExactInlineOnly "HsSpliceE{}" lexpr
    HsProc{} -> do
      -- TODO
      briDocByExactInlineOnly "HsProc{}" lexpr
    HsStatic{} -> do
      -- TODO
      briDocByExactInlineOnly "HsStatic{}" lexpr
    HsArrApp{} -> do
      -- TODO
      briDocByExactInlineOnly "HsArrApp{}" lexpr
    HsArrForm{} -> do
      -- TODO
      briDocByExactInlineOnly "HsArrForm{}" lexpr
    HsTick{} -> do
      -- TODO
      briDocByExactInlineOnly "HsTick{}" lexpr
    HsBinTick{} -> do
      -- TODO
      briDocByExactInlineOnly "HsBinTick{}" lexpr
    HsTickPragma{} -> do
      -- TODO
      briDocByExactInlineOnly "HsTickPragma{}" lexpr
    EWildPat{} -> do
      docLit $ Text.pack "_"
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    EAsPat _ asName asExpr -> do
#else
    EAsPat asName asExpr -> do
#endif
      docSeq
        [ docLit $ lrdrNameToText asName <> Text.pack "@"
        , layoutExpr asExpr
        ]
    EViewPat{} -> do
      -- TODO
      briDocByExactInlineOnly "EViewPat{}" lexpr
    ELazyPat{} -> do
      -- TODO
      briDocByExactInlineOnly "ELazyPat{}" lexpr
    HsWrap{} -> do
      -- TODO
      briDocByExactInlineOnly "HsWrap{}" lexpr
#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
    HsConLikeOut{} -> do
      -- TODO
      briDocByExactInlineOnly "HsWrap{}" lexpr
    ExplicitSum{} -> do
      -- TODO
      briDocByExactInlineOnly "ExplicitSum{}" lexpr
#endif
#if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
    XExpr{} -> error "brittany internal error: XExpr"
#endif

recordExpression
  :: (Data.Data.Data lExpr, Data.Data.Data name)
  => Bool
  -> IndentPolicy
  -> GenLocated SrcSpan lExpr
  -> ToBriDocM BriDocNumbered
  -> [(GenLocated SrcSpan name, Text, Maybe (ToBriDocM BriDocNumbered))]
  -> ToBriDocM BriDocNumbered
recordExpression False _ lexpr nameDoc [] =
  docSeq
    [ docNodeAnnKW lexpr (Just AnnOpenC) $ docSeq [nameDoc, docLit $ Text.pack "{"]
    , docLit $ Text.pack "}"
    ]
recordExpression True _ lexpr nameDoc [] =
  docSeq -- this case might still be incomplete, and is probably not used
         -- atm anyway.
    [ docNodeAnnKW lexpr (Just AnnOpenC) $ docSeq [nameDoc, docLit $ Text.pack "{"]
    , docLit $ Text.pack " .. }"
    ]
recordExpression dotdot indentPolicy lexpr nameDoc rFs@(rF1:rFr) = do
  let (rF1f, rF1n, rF1e) = rF1
  runFilteredAlternative $ do
    -- container { fieldA = blub, fieldB = blub }
    addAlternative
      $ docSeq
      [ docNodeAnnKW lexpr Nothing $ appSep $ docForceSingleline nameDoc
      , appSep $ docLit $ Text.pack "{"
      , docSeq $ List.intersperse docCommaSep
              $ rFs <&> \case
                (lfield, fieldStr, Just fieldDoc) ->
                  docWrapNode lfield $ docSeq
                        [ appSep $ docLit fieldStr
                        , appSep $ docLit $ Text.pack "="
                        , docForceSingleline fieldDoc
                        ]
                (lfield, fieldStr, Nothing) ->
                  docWrapNode lfield $ docLit fieldStr
      , if dotdot
          then docSeq [ docCommaSep, docLit $ Text.pack "..", docSeparator]
          else docSeparator
      , docLit $ Text.pack "}"
      ]
    -- hanging single-line fields
    -- container { fieldA = blub
    --           , fieldB = blub
    --           }
    addAlternativeCond (indentPolicy == IndentPolicyFree)
      $ docSeq
      [ docNodeAnnKW lexpr Nothing $ appSep nameDoc
      , docSetBaseY $ docLines $ let
          line1 = docCols ColRec
            [ appSep $ docLit $ Text.pack "{"
            , docWrapNodePrior rF1f $ appSep $ docLit rF1n
            , case rF1e of
                Just x -> docWrapNodeRest rF1f $ docSeq
                                [ appSep $ docLit $ Text.pack "="
                                , docForceSingleline x
                                ]
                Nothing -> docEmpty
            ]
          lineR = rFr <&> \(lfield, fText, fDoc) -> docWrapNode lfield $ docCols ColRec
            [ docCommaSep
            , appSep $ docLit fText
            , case fDoc of
                Just x ->  docSeq [ appSep $ docLit $ Text.pack "="
                                  , docForceSingleline x
                                  ]
                Nothing -> docEmpty
            ]
          dotdotLine = if dotdot
            then docCols ColRec
                   [ docNodeAnnKW lexpr (Just AnnOpenC) docCommaSep
                   , docNodeAnnKW lexpr (Just AnnDotdot)
                     $ docLit $ Text.pack ".."
                   ]
            else docNodeAnnKW lexpr (Just AnnOpenC) docEmpty
          lineN = docLit $ Text.pack "}"
          in [line1] ++ lineR ++ [dotdotLine, lineN]
      ]
    -- non-hanging with expressions placed to the right of the names
    -- container
    -- { fieldA = blub
    -- , fieldB = potentially
    --     multiline
    -- }
    addAlternative
      $ docSetParSpacing
      $ docAddBaseY BrIndentRegular
      $ docPar
          (docNodeAnnKW lexpr Nothing nameDoc)
          (docNonBottomSpacing $ docLines $ let
            line1 = docCols ColRec
              [ appSep $ docLit $ Text.pack "{"
              , docWrapNodePrior rF1f $ appSep $ docLit rF1n
              , docWrapNodeRest rF1f $ case rF1e of
                  Just x -> runFilteredAlternative $ do
                    addAlternativeCond (indentPolicy == IndentPolicyFree) $ do
                      docSeq
                        [appSep $ docLit $ Text.pack "=", docSetBaseY x]
                    addAlternative $ do
                      docSeq
                        [appSep $ docLit $ Text.pack "=", docForceParSpacing x]
                    addAlternative $ do
                      docAddBaseY BrIndentRegular
                        $ docPar (docLit $ Text.pack "=") x
                  Nothing -> docEmpty
              ]
            lineR = rFr <&> \(lfield, fText, fDoc) -> docWrapNode lfield
              $ docCols ColRec
              [ docCommaSep
              , appSep $ docLit fText
              , case fDoc of
                  Just x -> runFilteredAlternative $ do
                    addAlternativeCond (indentPolicy == IndentPolicyFree) $ do
                      docSeq
                        [appSep $ docLit $ Text.pack "=", docSetBaseY x]
                    addAlternative $ do
                      docSeq [ appSep $ docLit $ Text.pack "="
                             , docForceParSpacing x
                             ]
                    addAlternative $ do
                      docAddBaseY BrIndentRegular
                        $ docPar (docLit $ Text.pack "=") x
                  Nothing -> docEmpty
              ]
            dotdotLine = if dotdot
              then docCols ColRec
                     [ docNodeAnnKW lexpr (Just AnnOpenC) docCommaSep
                     , docNodeAnnKW lexpr (Just AnnDotdot)
                       $ docLit $ Text.pack ".."
                     ]
              else docNodeAnnKW lexpr (Just AnnOpenC) docEmpty
            lineN = docLit $ Text.pack "}"
            in [line1] ++ lineR ++ [dotdotLine, lineN]
          )

#if MIN_VERSION_ghc(8,4,0) /* ghc-8.4 */
litBriDoc :: HsLit GhcPs -> BriDocFInt
litBriDoc = \case
  HsChar       (SourceText t) _c                         -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ ['\'', c, '\'']
  HsCharPrim   (SourceText t) _c                         -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ ['\'', c, '\'']
  HsString     (SourceText t) _fastString                -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ FastString.unpackFS fastString
  HsStringPrim (SourceText t) _byteString                -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ Data.ByteString.Char8.unpack byteString
  HsInt        _              (IL (SourceText t) _ _)    -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsIntPrim    (SourceText t) _i                         -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsWordPrim   (SourceText t) _i                         -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsInt64Prim  (SourceText t) _i                         -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsWord64Prim (SourceText t) _i                         -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsInteger (SourceText t) _i                      _type -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsRat     _              (FL (SourceText t) _ _) _type -> BDFLit $ Text.pack t
  HsFloatPrim  _ (FL (SourceText t) _ _)                 -> BDFLit $ Text.pack t
  HsDoublePrim _ (FL (SourceText t) _ _)                 -> BDFLit $ Text.pack t
  _ -> error "litBriDoc: literal with no SourceText"

overLitValBriDoc :: OverLitVal -> BriDocFInt
overLitValBriDoc = \case
  HsIntegral   (IL (SourceText t) _ _) -> BDFLit $ Text.pack t
  HsFractional (FL (SourceText t) _ _) -> BDFLit $ Text.pack t
  HsIsString (SourceText t) _ -> BDFLit $ Text.pack t
  _ -> error "overLitValBriDoc: literal with no SourceText"
#elif MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
litBriDoc :: HsLit -> BriDocFInt
litBriDoc = \case
  HsChar       (SourceText t) _c          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ ['\'', c, '\'']
  HsCharPrim   (SourceText t) _c          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ ['\'', c, '\'']
  HsString     (SourceText t) _fastString -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ FastString.unpackFS fastString
  HsStringPrim (SourceText t) _byteString -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ Data.ByteString.Char8.unpack byteString
  HsInt        (SourceText t) _i          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsIntPrim    (SourceText t) _i          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsWordPrim   (SourceText t) _i          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsInt64Prim  (SourceText t) _i          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsWord64Prim (SourceText t) _i          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsInteger    (SourceText t) _i _type    -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsRat        (FL t _) _type             -> BDFLit $ Text.pack t
  HsFloatPrim  (FL t _)                   -> BDFLit $ Text.pack t
  HsDoublePrim (FL t _)                   -> BDFLit $ Text.pack t
  _ -> error "litBriDoc: literal with no SourceText"

overLitValBriDoc :: OverLitVal -> BriDocFInt
overLitValBriDoc = \case
  HsIntegral (SourceText t) _        -> BDFLit $ Text.pack t
  HsFractional (FL t _) -> BDFLit $ Text.pack t
  HsIsString (SourceText t) _        -> BDFLit $ Text.pack t
  _ -> error "overLitValBriDoc: literal with no SourceText"
#else                        /* ghc-8.0 */
litBriDoc :: HsLit -> BriDocFInt
litBriDoc = \case
  HsChar       t _c          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ ['\'', c, '\'']
  HsCharPrim   t _c          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ ['\'', c, '\'']
  HsString     t _fastString -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ FastString.unpackFS fastString
  HsStringPrim t _byteString -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ Data.ByteString.Char8.unpack byteString
  HsInt        t _i          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsIntPrim    t _i          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsWordPrim   t _i          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsInt64Prim  t _i          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsWord64Prim t _i          -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsInteger t _i _type       -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsRat (FL t _) _type       -> BDFLit $ Text.pack t
  HsFloatPrim  (FL t _)      -> BDFLit $ Text.pack t
  HsDoublePrim (FL t _)      -> BDFLit $ Text.pack t

overLitValBriDoc :: OverLitVal -> BriDocFInt
overLitValBriDoc = \case
  HsIntegral t _        -> BDFLit $ Text.pack t
  HsFractional (FL t _) -> BDFLit $ Text.pack t
  HsIsString t _        -> BDFLit $ Text.pack t
#endif
