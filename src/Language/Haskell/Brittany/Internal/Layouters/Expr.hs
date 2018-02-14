{-# LANGUAGE DataKinds #-}

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

import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString, AnnKeywordId(..) )
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
    HsVar vname -> do
      docLit =<< lrdrNameToTextAnn vname
    HsUnboundVar var -> case var of
      OutOfScope oname _ -> docLit $ Text.pack $ occNameString oname
      TrueExprHole oname -> docLit $ Text.pack $ occNameString oname
    HsRecFld{} -> do
      -- TODO
      briDocByExactInlineOnly "HsRecFld" lexpr
    HsOverLabel{} -> do
      -- TODO
      briDocByExactInlineOnly "HsOverLabel{}" lexpr
    HsIPVar{} -> do
      -- TODO
      briDocByExactInlineOnly "HsOverLabel{}" lexpr
    HsOverLit (OverLit olit _ _ _) -> do
      allocateNode $ overLitValBriDoc olit
    HsLit lit -> do
      allocateNode $ litBriDoc lit
    HsLam (MG (L _ [lmatch@(L _ (Match _ pats _ (GRHSs [lgrhs@(L _ (GRHS [] body))] (L _ EmptyLocalBinds))))]) _ _ _) -> do
      patDocs <- pats `forM` \p -> fmap return $ colsWrapPat =<< layoutPat p
      bodyDoc <- docAddBaseY BrIndentRegular <$> docSharedWrapper layoutExpr body
      let funcPatternPartLine =
            docCols ColCasePattern
              $ (patDocs <&> (\p -> docSeq [docForceSingleline p, docSeparator]))
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
#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
    HsLamCase (MG lmatches@(L _ matches) _ _ _) -> do
#else /* ghc-8.0 */
    HsLamCase _ (MG lmatches@(L _ matches) _ _ _) -> do
#endif
      binderDoc <- docLit $ Text.pack "->"
      funcPatDocs <- docWrapNode lmatches $ layoutPatternBind Nothing binderDoc `mapM` matches
      docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
        (docLit $ Text.pack "\\case")
        (docSetBaseAndIndent $ docNonBottomSpacing $ docLines $ return <$> funcPatDocs)
    HsApp exp1@(L _ HsApp{}) exp2 -> do
      let gather :: [LHsExpr RdrName] -> LHsExpr RdrName -> (LHsExpr RdrName, [LHsExpr RdrName])
          gather list = \case
            (L _ (HsApp l r)) -> gather (r:list) l
            x -> (x, list)
      let (headE, paramEs) = gather [exp2] exp1
      headDoc <- docSharedWrapper layoutExpr headE
      paramDocs <- docSharedWrapper layoutExpr `mapM` paramEs
      docAltFilter
        [ -- foo x y
          ( True
          , docCols ColApp
          $ appSep (docForceSingleline headDoc)
          : spacifyDocs (docForceSingleline <$> paramDocs)
          )
        , -- foo x
          --     y
          ( allowFreeIndent
          , docSeq
            [ appSep (docForceSingleline headDoc)
            , docSetBaseY
            $ docAddBaseY BrIndentRegular
            $ docLines
            $ (docForceSingleline <$> paramDocs)
            ]
          )
        , -- foo
          --   x
          --   y
          ( True
          , docSetParSpacing
          $ docAddBaseY BrIndentRegular
          $ docPar
            (docForceSingleline headDoc)
            ( docNonBottomSpacing
            $ docLines paramDocs
            )
          )
        , -- ( multi
          --   line
          --   function
          -- )
          --   x
          --   y
          ( True
          , docAddBaseY BrIndentRegular
          $ docPar
            headDoc
            ( docNonBottomSpacing
            $ docLines paramDocs
            )
          )
        ]
    HsApp exp1 exp2 -> do
      -- TODO: if expDoc1 is some literal, we may want to create a docCols here.
      expDoc1 <- docSharedWrapper layoutExpr exp1
      expDoc2 <- docSharedWrapper layoutExpr exp2
      docAlt
        [ -- func arg
          docSeq [appSep $ docForceSingleline expDoc1, docForceSingleline expDoc2]
        , -- func argline1
          --   arglines
          -- e.g.
          -- func if x
          --   then 1
          --   else 2
          docSetParSpacing
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
#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
    HsAppType exp1 (HsWC _ ty1) -> do
#else /* ghc-8.0 */
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
    HsAppTypeOut{} -> do
      -- TODO
      briDocByExactInlineOnly "HsAppTypeOut{}" lexpr
    OpApp expLeft@(L _ OpApp{}) expOp _ expRight -> do
      let gather :: [(LHsExpr RdrName, LHsExpr RdrName)] -> LHsExpr RdrName -> (LHsExpr RdrName, [(LHsExpr RdrName, LHsExpr RdrName)])
          gather opExprList = \case
            (L _ (OpApp l1 op1 _ r1)) -> gather ((op1, r1): opExprList) l1
            final -> (final, opExprList)
          (leftOperand, appList) = gather [] expLeft
      leftOperandDoc <- docSharedWrapper layoutExpr leftOperand
      appListDocs <- appList `forM` \(x,y) -> [ (xD, yD)
                                              | xD <- docSharedWrapper layoutExpr x
                                              , yD <- docSharedWrapper layoutExpr y
                                              ]
      opLastDoc <- docSharedWrapper layoutExpr expOp
      expLastDoc <- docSharedWrapper layoutExpr expRight
      let allowPar = case (expOp, expRight) of
            (L _ (HsVar (L _ (Unqual occname))), _)
              | occNameString occname == "$" -> True
            (_, L _ (HsApp _ (L _ HsVar{}))) -> False
            _ -> True
      docAlt
        [ docSeq
          [ appSep $ docForceSingleline leftOperandDoc
          , docSeq
          $ (appListDocs <&> \(od, ed) -> docSeq
              [ appSep $ docForceSingleline od
              , appSep $ docForceSingleline ed
              ]
            )
          , appSep $ docForceSingleline opLastDoc
          , (if allowPar then docForceParSpacing else docForceSingleline)
              expLastDoc
          ]
        -- this case rather leads to some unfortunate layouting than to anything
        -- useful; disabling for now. (it interfers with cols stuff.)
        -- , docSetBaseY
        -- - $ docPar
        --     leftOperandDoc
        --     ( docLines
        -- -    $ (appListDocs <&> \(od, ed) -> docCols ColOpPrefix [appSep od, docSetBaseY ed])
        --       ++ [docCols ColOpPrefix [appSep opLastDoc, docSetBaseY expLastDoc]]
        --     )
        , docPar
            leftOperandDoc
            ( docLines
            $ (appListDocs <&> \(od, ed) -> docCols ColOpPrefix [appSep od, docSetBaseY ed])
              ++ [docCols ColOpPrefix [appSep opLastDoc, docSetBaseY expLastDoc]]
            )
        ]
    OpApp expLeft expOp _ expRight -> do
      expDocLeft  <- docSharedWrapper layoutExpr expLeft
      expDocOp    <- docSharedWrapper layoutExpr expOp
      expDocRight <- docSharedWrapper layoutExpr expRight
      let allowPar = case (expOp, expRight) of
            (L _ (HsVar (L _ (Unqual occname))), _)
              | occNameString occname == "$" -> True
            (_, L _ (HsApp _ (L _ HsVar{}))) -> False
            _ -> True
      docAltFilter
        $   [ -- one-line
              (,) True
            $ docSeq
              [ appSep $ docForceSingleline expDocLeft
              , appSep $ docForceSingleline expDocOp
              , docForceSingleline expDocRight
              ]
            -- , -- line + freely indented block for right expression
            --   docSeq
            --   [ appSep $ docForceSingleline expDocLeft
            --   , appSep $ docForceSingleline expDocOp
            --   , docSetBaseY $ docAddBaseY BrIndentRegular expDocRight
            --   ]
            , -- two-line
              (,) True
            $ docAddBaseY BrIndentRegular
            $ docPar
                expDocLeft
                ( docForceSingleline
                $ docCols ColOpPrefix [appSep $ expDocOp, docSetBaseY expDocRight]
                )
            , -- one-line + par
              (,) allowPar
            $ docSeq
              [ appSep $ docForceSingleline expDocLeft
              , appSep $ docForceSingleline expDocOp
              , docForceParSpacing expDocRight
              ]
            , -- more lines
              (,) True
            $ docAddBaseY BrIndentRegular
            $ docPar
                expDocLeft
                (docCols ColOpPrefix [appSep $ expDocOp, docSetBaseY expDocRight])
            ]
    NegApp op _ -> do
      opDoc <- docSharedWrapper layoutExpr op
      docSeq $ [ docLit $ Text.pack "-"
               , opDoc
               ]
    HsPar innerExp -> do
      innerExpDoc <- docSharedWrapper layoutExpr innerExp
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
    SectionL left op -> do -- TODO: add to testsuite
      leftDoc <- docSharedWrapper layoutExpr left
      opDoc   <- docSharedWrapper layoutExpr op
      docSeq [leftDoc, docSeparator, opDoc]
    SectionR op right -> do -- TODO: add to testsuite
      opDoc    <- docSharedWrapper layoutExpr op
      rightDoc <- docSharedWrapper layoutExpr right
      docSeq [opDoc, docSeparator, rightDoc]
    ExplicitTuple args boxity -> do
      let argExprs = fmap (\case (L _ (Present e)) -> Just e; (L _ (Missing PlaceHolder)) -> Nothing) args
      argDocs <- docSharedWrapper (maybe docEmpty layoutExpr) `mapM` argExprs
      hasComments <- hasAnyCommentsBelow lexpr
      let (openLit, closeLit) = case boxity of
            Boxed -> (docLit $ Text.pack "(", docLit $ Text.pack ")")
            Unboxed -> (docLit $ Text.pack "(#", docLit $ Text.pack "#)")
      case splitFirstLast argDocs of
        FirstLastEmpty -> docSeq
          [ openLit
          , docNodeAnnKW lexpr (Just AnnOpenP) $ closeLit
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
        FirstLast e1 ems eN ->
          docAltFilter
            [ (,) (not hasComments)
            $ docCols ColTuple
              (  [docSeq [openLit, docForceSingleline e1]]
              ++ (ems <&> \e -> docSeq [docCommaSep, docForceSingleline e])
              ++ [docSeq [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenP) (docForceSingleline eN), closeLit]]
              )
            , (,) True
            $ let
                start = docCols ColTuples
                          [appSep $ openLit, e1]
                linesM = ems <&> \d ->
                        docCols ColTuples [docCommaSep, d]
                lineN = docCols ColTuples [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenP) eN]
                end   = closeLit
              in docSetBaseY $ docLines $ [start] ++ linesM ++ [lineN] ++ [end]
            ]
    HsCase cExp (MG lmatches@(L _ matches) _ _ _) -> do
      cExpDoc <- docSharedWrapper layoutExpr cExp
      binderDoc <- docLit $ Text.pack "->"
      funcPatDocs <- docWrapNode lmatches $ layoutPatternBind Nothing binderDoc `mapM` matches
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
    HsIf _ ifExpr thenExpr elseExpr -> do
      ifExprDoc   <- docSharedWrapper layoutExpr ifExpr
      thenExprDoc <- docSharedWrapper layoutExpr thenExpr
      elseExprDoc <- docSharedWrapper layoutExpr elseExpr
      hasComments <- hasAnyCommentsBelow lexpr
      let maySpecialIndent =
            case indentPolicy of
              IndentPolicyLeft -> BrIndentRegular
              _ -> BrIndentSpecial 3
      -- TODO: some of the alternatives (especially last and last-but-one)
      -- overlap.
      docAltFilter
        [ -- if _ then _ else _
          (,) (not hasComments)
        $ docSeq
          [ appSep $ docLit $ Text.pack "if"
          , appSep $ docForceSingleline ifExprDoc
          , appSep $ docLit $ Text.pack "then"
          , appSep $ docForceSingleline thenExprDoc
          , appSep $ docLit $ Text.pack "else"
          , docForceSingleline elseExprDoc
          ]
        , -- either
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
          (,) True
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
        , -- either
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
          (,) True
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
        , (,) True
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
        ]
    HsMultiIf _ cases -> do
      clauseDocs <- cases `forM` layoutGrhs
      binderDoc <- docLit $ Text.pack "->"
      hasComments <- hasAnyCommentsBelow lexpr
      docSetParSpacing $ docAddBaseY BrIndentRegular $ docPar
        (docLit $ Text.pack "if")
        (layoutPatternBindFinal Nothing binderDoc Nothing clauseDocs Nothing hasComments)
    HsLet binds exp1 -> do
      expDoc1 <- docSharedWrapper layoutExpr exp1
      mBindDocs <- layoutLocalBinds binds
      let
        ifIndentLeftElse :: a -> a -> a
        ifIndentLeftElse x y =
          if indentPolicy == IndentPolicyLeft then x else y
      -- this `docSetIndentLevel` might seem out of place, but is here due to
      -- ghc-exactprint's DP handling of "let" in particular.
      -- Just pushing another indentation level is a straightforward approach
      -- to making brittany idempotent, even though the result is non-optimal
      -- if "let" is moved horizontally as part of the transformation, as the
      -- comments before the first let item are moved horizontally with it.
      docSetIndentLevel $ case mBindDocs of
        Just [bindDoc] -> docAlt
          [ docSeq
              [ appSep $ docLit $ Text.pack "let"
              , appSep $ docForceSingleline $ return bindDoc
              , appSep $ docLit $ Text.pack "in"
              , docForceSingleline $ expDoc1
              ]
          , docLines
              [ docAlt
                  [ docSeq
                      [ appSep $ docLit $ Text.pack "let"
                      , ifIndentLeftElse docForceSingleline docSetBaseAndIndent
                      $ return bindDoc
                      ]
                  , docAddBaseY BrIndentRegular
                  $ docPar
                    (docLit $ Text.pack "let")
                    (docSetBaseAndIndent $ return bindDoc)
                  ]
              , docAlt
                  [ docSeq
                      [ appSep $ docLit $ Text.pack $ ifIndentLeftElse "in" "in "
                      , ifIndentLeftElse docForceSingleline docSetBaseAndIndent expDoc1
                      ]
                  , docAddBaseY BrIndentRegular
                  $ docPar
                    (docLit $ Text.pack "in")
                    (docSetBaseY $ expDoc1)
                  ]
              ]
          ]
        Just bindDocs@(_:_) -> docAltFilter
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
          [ ( indentPolicy == IndentPolicyLeft
            , docLines
              [ docAddBaseY BrIndentRegular
              $ docPar
                (docLit $ Text.pack "let")
                (docSetBaseAndIndent $ docLines $ return <$> bindDocs)
              , docSeq
                [ docLit $ Text.pack "in "
                , docAddBaseY BrIndentRegular $ expDoc1
                ]
              ]
            )
          , ( indentPolicy /= IndentPolicyLeft
            , docLines
              [ docSeq
                [ appSep $ docLit $ Text.pack "let"
                , docSetBaseAndIndent $ docLines $ return <$> bindDocs
                ]
              , docSeq
                [ appSep $ docLit $ Text.pack "in "
                , docSetBaseY $ expDoc1
                ]
              ]
            )
          , ( True
            , docLines
              [ docAddBaseY BrIndentRegular
              $ docPar
                (docLit $ Text.pack "let")
                (docSetBaseAndIndent $ docLines $ return <$> bindDocs)
              , docAddBaseY BrIndentRegular
              $ docPar
                (docLit $ Text.pack "in")
                (docSetBaseY $ expDoc1)
              ]
            )
          ]
        _ -> docSeq [appSep $ docLit $ Text.pack "let in", expDoc1]
      -- docSeq [appSep $ docLit "let in", expDoc1]
    HsDo DoExpr (L _ stmts) _ -> do
      stmtDocs <- docSharedWrapper layoutStmt `mapM` stmts
      docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ docPar
            (docLit $ Text.pack "do")
            (docSetBaseAndIndent $ docNonBottomSpacing $ docLines stmtDocs)
    HsDo MDoExpr (L _ stmts) _ -> do
      stmtDocs <- docSharedWrapper layoutStmt `mapM` stmts
      docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ docPar
            (docLit $ Text.pack "mdo")
            (docSetBaseAndIndent $ docNonBottomSpacing $ docLines stmtDocs)
    HsDo x  (L _ stmts) _ | case x of { ListComp -> True
                                      ; MonadComp -> True
                                      ; _ -> False } -> do
      stmtDocs <- docSharedWrapper layoutStmt `mapM` stmts
      hasComments <- hasAnyCommentsBelow lexpr
      docAltFilter
        [ (,) (not hasComments)
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
                  $ fmap docForceSingleline $ List.init stmtDocs
          , docLit $ Text.pack " ]"
          ]
        , (,) True
        $ let
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
        ]
    HsDo{} -> do
      -- TODO
      unknownNodeError "HsDo{} no comp" lexpr
    ExplicitList _ _ elems@(_:_) -> do
      elemDocs <- elems `forM` docSharedWrapper layoutExpr
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
              , docSetBaseY $ docNodeAnnKW lexpr (Just AnnOpenS) $ e
              ]
            , docLit $ Text.pack "]"
            ]
          ]
        FirstLast e1 ems eN ->
          docAltFilter
            [  (,) (not hasComments)
            $  docSeq
            $  [docLit $ Text.pack "["]
            ++ List.intersperse docCommaSep (docForceSingleline <$> (e1:ems ++ [docNodeAnnKW lexpr (Just AnnOpenS) eN]))
            ++ [docLit $ Text.pack "]"]
            , (,) True
            $ let
                start = docCols ColList
                          [appSep $ docLit $ Text.pack "[", e1]
                linesM = ems <&> \d ->
                        docCols ColList [docCommaSep, d]
                lineN = docCols ColList [docCommaSep, docNodeAnnKW lexpr (Just AnnOpenS) eN]
                end   = docLit $ Text.pack "]"
              in docSetBaseY $ docLines $ [start] ++ linesM ++ [lineN] ++ [end]
            ]
    ExplicitList _ _ [] ->
      docLit $ Text.pack "[]"
    ExplicitPArr{} -> do
      -- TODO
      briDocByExactInlineOnly "ExplicitPArr{}" lexpr
    RecordCon lname _ _ (HsRecFields [] Nothing) -> do
      let t = lrdrNameToText lname
      docWrapNode lname $ docSeq
        [ docNodeAnnKW lexpr (Just AnnOpenC) $ docLit $ t <> Text.pack "{"
        , docLit $ Text.pack "}"
        ]
    RecordCon lname _ _ (HsRecFields fs@(_:_) Nothing) -> do
      let nameDoc = docWrapNode lname $ docLit $ lrdrNameToText lname
      ((fd1l, fd1n, fd1e):fdr) <- fs `forM` \fieldl@(L _ (HsRecField (L _ (FieldOcc lnameF _)) fExpr pun)) -> do
        fExpDoc <- if pun
          then return Nothing
          else Just <$> docSharedWrapper layoutExpr fExpr
        return $ (fieldl, lrdrNameToText lnameF, fExpDoc)
      let line1 appender wrapper =
            [ appender $ docLit $ Text.pack "{"
            , docWrapNodePrior fd1l $ appSep $ docLit $ fd1n
            , case fd1e of
                Just x -> docSeq
                  [ appSep $ docLit $ Text.pack "="
                  , docWrapNodeRest fd1l $ wrapper $ x
                  ]
                Nothing -> docEmpty
            ]
      let lineR wrapper = fdr <&> \(lfield, fText, fDoc) ->
            [ docCommaSep
            , appSep $ docLit $ fText
            , case fDoc of
                Just x -> docWrapNode lfield $ docSeq
                  [ appSep $ docLit $ Text.pack "="
                  , wrapper x
                  ]
                Nothing -> docEmpty
            ]
      let lineN =
            [ docNodeAnnKW lexpr (Just AnnOpenC) docEmpty
            , docLit $ Text.pack "}"
            ]
      docAlt
        [  docSeq
        $  [docNodeAnnKW lexpr Nothing $ nameDoc, docSeparator]
        ++ line1 id docForceSingleline
        ++ join (lineR docForceSingleline)
        ++ lineN
        , docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ docPar
            (docNodeAnnKW lexpr Nothing $ nameDoc)
            ( docNonBottomSpacing
            $ docLines
            $  [docCols ColRecUpdate $ line1 appSep (docAddBaseY BrIndentRegular)]
            ++ (docCols ColRecUpdate <$> lineR (docAddBaseY BrIndentRegular))
            ++ [docSeq lineN]
            )
        ]
    RecordCon lname _ _ (HsRecFields [] (Just 0)) -> do
      let t = lrdrNameToText lname
      docWrapNode lname $ docLit $ t <> Text.pack " {..}"
    RecordCon lname _ _ (HsRecFields fs@(_:_) (Just dotdoti)) | dotdoti == length fs -> do
      let nameDoc = docWrapNode lname $ docLit $ lrdrNameToText lname
      ((fd1l, fd1n, fd1e):fdr) <- fs `forM` \fieldl@(L _ (HsRecField (L _ (FieldOcc lnameF _)) fExpr pun)) -> do
        fExpDoc <- if pun
          then return Nothing
          else Just <$> docSharedWrapper layoutExpr fExpr
        return $ (fieldl, lrdrNameToText lnameF, fExpDoc)
      let line1 appender wrapper =
            [ appender $ docLit $ Text.pack "{"
            , docWrapNodePrior fd1l $ appSep $ docLit $ fd1n
            , case fd1e of
                Just x -> docSeq
                  [ appSep $ docLit $ Text.pack "="
                  , docWrapNodeRest fd1l $ wrapper $ x
                  ]
                Nothing -> docEmpty
            ]
      let lineR wrapper = fdr <&> \(lfield, fText, fDoc) ->
            [ docCommaSep
            , appSep $ docLit $ fText
            , case fDoc of
                Just x -> docWrapNode lfield $ docSeq
                  [ appSep $ docLit $ Text.pack "="
                  , wrapper x
                  ]
                Nothing -> docEmpty
            ]
      let lineDot =
            [ docCommaSep
            , docLit $ Text.pack ".."
            ]
      let lineN =
            [ docNodeAnnKW lexpr (Just AnnOpenC) docEmpty
            , docLit $ Text.pack "}"
            ]
      docAlt
        [  docSeq
        $  [docNodeAnnKW lexpr Nothing $ nameDoc, docSeparator]
        ++ line1 id docForceSingleline
        ++ join (lineR docForceSingleline)
        ++ lineDot
        ++ lineN
        , docSetParSpacing
        $ docAddBaseY BrIndentRegular
        $ docPar
            (docNodeAnnKW lexpr Nothing $ nameDoc)
            ( docNonBottomSpacing
            $ docLines
            $  [docCols ColRecUpdate $ line1 appSep (docAddBaseY BrIndentRegular)]
            ++ (docCols ColRecUpdate <$> lineR (docAddBaseY BrIndentRegular))
            ++ [docSeq lineDot, docSeq lineN]
            )
        ]
    RecordCon{} ->
      unknownNodeError "RecordCon with puns" lexpr
    RecordUpd rExpr [] _ _ _ _ -> do
      rExprDoc <- docSharedWrapper layoutExpr rExpr
      docSeq [rExprDoc, docLit $ Text.pack "{}"]
    RecordUpd rExpr fields@(_:_) _ _ _ _ -> do
      rExprDoc <- docSharedWrapper layoutExpr rExpr
      rFs@((rF1f, rF1n, rF1e):rFr) <- fields
        `forM` \lfield@(L _ (HsRecField (L _ ambName) rFExpr pun)) -> do
          rFExpDoc <- if pun
            then return Nothing
            else Just <$> docSharedWrapper layoutExpr rFExpr
          return $ case ambName of
            Unambiguous n _ -> (lfield, lrdrNameToText n, rFExpDoc)
            Ambiguous   n _ -> (lfield, lrdrNameToText n, rFExpDoc)
      docAltFilter
        -- singleline
        [ ( True
          , docSeq
            [ docNodeAnnKW lexpr Nothing $ appSep $ docForceSingleline rExprDoc
            , appSep $ docLit $ Text.pack "{"
            , appSep $ docSeq $ List.intersperse docCommaSep
                    $ rFs <&> \case
                      (lfield, fieldStr, Just fieldDoc) ->
                        docWrapNode lfield $ docSeq
                              [ appSep $ docLit fieldStr
                              , appSep $ docLit $ Text.pack "="
                              , docForceSingleline fieldDoc
                              ]
                      (lfield, fieldStr, Nothing) ->
                        docWrapNode lfield $ docLit fieldStr
            , docLit $ Text.pack "}"
            ]
          )
        -- wild-indentation block
        , ( indentPolicy /= IndentPolicyLeft
          , docSeq
            [ docNodeAnnKW lexpr Nothing $ appSep rExprDoc
            , docSetBaseY $ docLines $ let
                line1 = docCols ColRecUpdate
                  [ appSep $ docLit $ Text.pack "{"
                  , docWrapNodePrior rF1f $ appSep $ docLit $ rF1n
                  , case rF1e of
                      Just x -> docWrapNodeRest rF1f $ docSeq
                                      [ appSep $ docLit $ Text.pack "="
                                      , docForceSingleline $ x
                                      ]
                      Nothing -> docEmpty
                  ]
                lineR = rFr <&> \(lfield, fText, fDoc) -> docWrapNode lfield $ docCols ColRecUpdate
                  [ docCommaSep
                  , appSep $ docLit $ fText
                  , case fDoc of
                      Just x ->  docSeq [ appSep $ docLit $ Text.pack "="
                                        , docForceSingleline x
                                        ]
                      Nothing -> docEmpty
                  ]
                lineN = docSeq
                  [ docNodeAnnKW lexpr (Just AnnOpenC) docEmpty
                  , docLit $ Text.pack "}"
                  ]
                in [line1] ++ lineR ++ [lineN]
            ]
          )
        -- strict indentation block
        , ( True
          , docSetParSpacing
          $ docAddBaseY BrIndentRegular
          $ docPar
              (docNodeAnnKW lexpr Nothing $ rExprDoc)
              (docNonBottomSpacing $ docLines $ let
                line1 = docCols ColRecUpdate
                  [ appSep $ docLit $ Text.pack "{"
                  , docWrapNodePrior rF1f $ appSep $ docLit $ rF1n
                  , docWrapNodeRest rF1f $ case rF1e of
                      Just x -> docSeq [ appSep $ docLit $ Text.pack "="
                                      , docAddBaseY BrIndentRegular $ x
                                      ]
                      Nothing -> docEmpty
                  ]
                lineR = rFr <&> \(lfield, fText, fDoc) -> docWrapNode lfield $ docCols ColRecUpdate
                  [ docCommaSep
                  , appSep $ docLit $ fText
                  , case fDoc of
                      Just x ->  docSeq [ appSep $ docLit $ Text.pack "="
                                        , docAddBaseY BrIndentRegular x
                                        ]
                      Nothing -> docEmpty
                  ]
                lineN = docSeq
                  [ docNodeAnnKW lexpr (Just AnnOpenC) docEmpty
                  , docLit $ Text.pack "}"
                  ]
                in [line1] ++ lineR ++ [lineN])
          )
        ]
#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
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
    ExprWithTySigOut{} -> do
      -- TODO
      briDocByExactInlineOnly "ExprWithTySigOut{}" lexpr
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
    PArrSeq{} -> do
      -- TODO
      briDocByExactInlineOnly "PArrSeq{}" lexpr
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
    EAsPat asName asExpr -> do
      docSeq
        [ docLit $ (lrdrNameToText asName) <> Text.pack "@"
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


#if MIN_VERSION_ghc(8,2,0) /* ghc-8.2 */
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
  HsInteger (SourceText t) _i _type       -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsRat (FL t _) _type       -> BDFLit $ Text.pack t
  HsFloatPrim  (FL t _)      -> BDFLit $ Text.pack t
  HsDoublePrim (FL t _)      -> BDFLit $ Text.pack t
  _ -> error "litBriDoc: literal with no SourceText"

overLitValBriDoc :: OverLitVal -> BriDocFInt
overLitValBriDoc = \case
  HsIntegral (SourceText t) _        -> BDFLit $ Text.pack t
  HsFractional (FL t _) -> BDFLit $ Text.pack t
  HsIsString (SourceText t) _        -> BDFLit $ Text.pack t
  _ -> error "overLitValBriDoc: literal with no SourceText"
#else
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
