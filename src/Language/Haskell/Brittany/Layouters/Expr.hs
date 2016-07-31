{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Layouters.Expr
  ( layoutExpr
  , litBriDoc
  , isExpressionTypeHeadPar
  , isExpressionTypeHeadPar'
  , overLitValBriDoc
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
import qualified FastString
import           BasicTypes

import           Language.Haskell.Brittany.Layouters.Pattern
import           Language.Haskell.Brittany.Layouters.Decl
import           Language.Haskell.Brittany.Layouters.Stmt
import           Language.Haskell.Brittany.Layouters.Type



layoutExpr :: ToBriDoc HsExpr
layoutExpr lexpr@(L _ expr) = docWrapNode lexpr $ case expr of
  HsVar vname -> do
    docLit =<< lrdrNameToTextAnn vname
  HsUnboundVar var -> case var of
    OutOfScope oname _ -> docLit $ Text.pack $ occNameString oname
    TrueExprHole oname -> docLit $ Text.pack $ occNameString oname
  HsRecFld{} -> do
    -- TODO
    briDocByExact lexpr
  HsOverLabel{} -> do
    -- TODO
    briDocByExact lexpr
  HsIPVar{} -> do
    -- TODO
    briDocByExact lexpr
  HsOverLit (OverLit olit _ _ _) -> do
    allocateNode $ overLitValBriDoc olit
  HsLit lit -> do
    allocateNode $ litBriDoc lit
  HsLam (MG (L _ [lmatch@(L _ (Match _ pats _ (GRHSs [lgrhs@(L _ (GRHS [] body))] (L _ EmptyLocalBinds))))]) _ _ _) -> do
    patDocs <- pats `forM` docSharedWrapper layoutPat
    bodyDoc <- docAddBaseY BrIndentRegular <$> docSharedWrapper layoutExpr body
    let funcPatternPartLine =
          docCols ColCasePattern
            $ (patDocs <&> (\p -> docSeq [docForceSingleline p, docSeparator]))
    docAlt
      [ docSeq
        [ docLit $ Text.pack "\\"
        , docWrapNode lmatch $ funcPatternPartLine
        , appSep $ docLit $ Text.pack "->"
        , docWrapNode lgrhs $ bodyDoc
        ]
      -- TODO
      ]
  HsLam{} ->
    unknownNodeError "HsLam too complex" lexpr
  HsLamCase _ (MG lmatches@(L _ matches) _ _ _) -> do
    binderDoc <- docLit $ Text.pack "->"
    funcPatDocs <- docWrapNode lmatches $ layoutPatternBind Nothing binderDoc `mapM` matches
    docAddBaseY BrIndentRegular $ docPar
      (docLit $ Text.pack "\\case")
      (docLines $ return <$> funcPatDocs)
  HsApp exp1@(L _ HsApp{}) exp2 -> do
    let gather :: [LHsExpr RdrName] -> LHsExpr RdrName -> (LHsExpr RdrName, [LHsExpr RdrName])
        gather list = \case
          (L _ (HsApp l r)) -> gather (r:list) l
          x -> (x, list)
    let (headE, paramEs) = gather [exp2] exp1
    headDoc <- docSharedWrapper layoutExpr headE
    paramDocs <- docSharedWrapper layoutExpr `mapM` paramEs
    docAlt
      [ docCols ColApp
      $ appSep (docForceSingleline headDoc)
      : spacifyDocs (docForceSingleline <$> paramDocs)
      , docSeq
        [ appSep (docForceSingleline headDoc)
        , docSetBaseY
        $ docAddBaseY BrIndentRegular
        $ docLines
        $ paramDocs
        ]
      , docAddBaseY BrIndentRegular
      $ docPar
        headDoc
        ( docNonBottomSpacing
        $ docLines paramDocs
        )
      ]
  HsApp exp1 exp2 -> do
    -- TODO: if expDoc1 is some literal, we may want to create a docCols here.
    expDoc1 <- docSharedWrapper layoutExpr exp1
    expDoc2 <- docSharedWrapper layoutExpr exp2
    docAlt
      [ docSeq [appSep $ docForceSingleline expDoc1, docForceSingleline expDoc2]
      , docAddBaseY BrIndentRegular
      $ docPar
        expDoc1
        expDoc2
      ]
  HsAppType{} -> do
    -- TODO
    briDocByExact lexpr
  HsAppTypeOut{} -> do
    -- TODO
    briDocByExact lexpr
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
        , docForceSingleline expLastDoc
        ]
      , docAddBaseY BrIndentRegular
      $ docPar
          (docSetBaseY leftOperandDoc)
          ( docLines
          $ (appListDocs <&> \(od, ed) -> docCols ColOpPrefix [appSep od, docSetBaseY ed])
            ++ [docCols ColOpPrefix [appSep opLastDoc, docSetBaseY expLastDoc]]
          )
      -- TODO: singleline
      -- TODO: wrapping on spine nodes
      ]
  OpApp expLeft expOp _ expRight -> do
    expDocLeft  <- docSharedWrapper layoutExpr expLeft
    expDocOp    <- docSharedWrapper layoutExpr expOp
    expDocRight <- docSharedWrapper layoutExpr expRight
    docAlt
      $   [ docSeq
            [ appSep $ docForceSingleline expDocLeft
            , appSep $ docForceSingleline expDocOp
            , docForceSingleline expDocRight
            ]
          ]
      ++  [ docSeq
            [ appSep $ docForceSingleline expDocLeft
            , appSep $ docForceSingleline expDocOp
            , docForceMultiline expDocRight
            ]
          | isExpressionTypeHeadPar expRight
          ]
      ++  [ docSeq
            [ appSep $ docForceSingleline expDocLeft
            , appSep $ docForceSingleline expDocOp
            , docSetBaseY $ docAddBaseY BrIndentRegular expDocRight
            ]
          , docAddBaseY BrIndentRegular
          $ docPar
              expDocLeft
              -- TODO: turn this into docCols?
              (docCols ColOpPrefix [appSep $ expDocOp, expDocRight])
          ]
  NegApp{} -> do
    -- TODO
    briDocByExact lexpr
  HsPar innerExp -> do
    innerExpDoc <- docSharedWrapper layoutExpr innerExp
    docAlt
      [ docSeq
        [ docLit $ Text.pack "("
        , docForceSingleline innerExpDoc
        , docLit $ Text.pack ")"
        ]
      -- TODO
      ]
  SectionL left op -> do -- TODO: add to testsuite
    leftDoc <- docSharedWrapper layoutExpr left
    opDoc   <- docSharedWrapper layoutExpr op
    docSeq [leftDoc, opDoc]
  SectionR op right -> do -- TODO: add to testsuite
    opDoc    <- docSharedWrapper layoutExpr op
    rightDoc <- docSharedWrapper layoutExpr right
    docSeq [opDoc, rightDoc]
  ExplicitTuple args boxity
    | Just argExprs <- args `forM` (\case (L _ (Present e)) -> Just e; _ -> Nothing) -> do
    argDocs <- docSharedWrapper layoutExpr `mapM` argExprs
    case boxity of
      Boxed -> docAlt
        [ docSeq
        $  [ docLit $ Text.pack "(" ]
        ++ List.intersperse (appSep $ docLit $ Text.pack ",") argDocs
        ++ [ docLit $ Text.pack ")"]
        -- TODO
        ]
      Unboxed -> docAlt
        [ docSeq
        $  [ docLit $ Text.pack "(#" ]
        ++ List.intersperse (appSep $ docLit $ Text.pack ",") argDocs
        ++ [ docLit $ Text.pack "#)"]
        -- TODO
        ]
  ExplicitTuple{} ->
    unknownNodeError "ExplicitTuple|.." lexpr 
  HsCase cExp (MG lmatches@(L _ matches) _ _ _) -> do
    cExpDoc <- docSharedWrapper layoutExpr cExp
    binderDoc <- docLit $ Text.pack "->"
    funcPatDocs <- docWrapNode lmatches $ layoutPatternBind Nothing binderDoc `mapM` matches
    docAlt
      [ docAddBaseY BrIndentRegular
      $ docPar
        ( docSeq
          [ appSep $ docLit $ Text.pack "case"
          , appSep $ docForceSingleline cExpDoc
          , docLit $ Text.pack "of"
          ])
        (docSetIndentLevel $ docNonBottomSpacing $ docLines $ return <$> funcPatDocs)
      , docPar
          ( docAddBaseY BrIndentRegular
          $ docPar (docLit $ Text.pack "case") cExpDoc
          )
          ( docAddBaseY BrIndentRegular
          $ docPar (docLit $ Text.pack "of")
            (docSetIndentLevel $ docLines $ return <$> funcPatDocs)
          )
      ]
  HsIf _ ifExpr thenExpr elseExpr -> do
    ifExprDoc   <- docSharedWrapper layoutExpr ifExpr
    thenExprDoc <- docSharedWrapper layoutExpr thenExpr
    elseExprDoc <- docSharedWrapper layoutExpr elseExpr
    let thenMod = if isExpressionTypeHeadPar thenExpr
          then id
          else docForceSingleline
        elseMod = if isExpressionTypeHeadPar elseExpr
          then id
          else docForceSingleline
    docAlt
      [ docSeq
        [ appSep $ docLit $ Text.pack "if"
        , appSep $ docForceSingleline ifExprDoc
        , appSep $ docLit $ Text.pack "then"
        , appSep $ docForceSingleline thenExprDoc
        , appSep $ docLit $ Text.pack "else"
        , docForceSingleline elseExprDoc
        ]
      , docAddBaseY BrIndentRegular
      $ docPar
          ( docAddBaseY (BrIndentSpecial 3)
          $ docSeq [appSep $ docLit $ Text.pack "if", ifExprDoc])
          (docLines
            [ docAddBaseY BrIndentRegular
            $ docAlt
              [ docSeq [appSep $ docLit $ Text.pack "then", thenMod thenExprDoc]
              , docAddBaseY BrIndentRegular
              $ docPar (docLit $ Text.pack "then") thenExprDoc
              ]
            , docAddBaseY BrIndentRegular
            $ docAlt
              [ docSeq [appSep $ docLit $ Text.pack "else", elseMod elseExprDoc]
              , docAddBaseY BrIndentRegular
              $ docPar (docLit $ Text.pack "else") elseExprDoc
              ]
            ])
      , docLines
        [ docAddBaseY (BrIndentSpecial 3)
        $ docSeq [appSep $ docLit $ Text.pack "if", ifExprDoc]
        , docAddBaseY BrIndentRegular
        $ docPar (docLit $ Text.pack "then") thenExprDoc
        , docAddBaseY BrIndentRegular
        $ docPar (docLit $ Text.pack "else") elseExprDoc
        ]
      ]
  HsMultiIf _ cases -> do
    clauseDocs <- cases `forM` layoutGrhs
    binderDoc <- docLit $ Text.pack " ->"
    docAddBaseY BrIndentRegular $ docPar
      (docLit $ Text.pack "if")
      (layoutPatternBindFinal binderDoc Nothing clauseDocs Nothing)
  HsLet binds exp1 -> do
    expDoc1 <- docSharedWrapper layoutExpr exp1
    mBindDocs <- layoutLocalBinds binds
    case mBindDocs of
      Just [bindDoc] -> docAlt
        [ docSeq
          [ appSep $ docLit $ Text.pack "let"
          , appSep $ docForceSingleline $ return bindDoc
          , appSep $ docLit $ Text.pack "in"
          , docForceSingleline $ expDoc1
          ]
        , docLines
          [ docSeq
            [ appSep $ docLit $ Text.pack "let"
            , docSetBaseY $ docSetIndentLevel $ return bindDoc
            ]
          , docSeq
            [ appSep $ docLit $ Text.pack "in "
            , docSetBaseY $ docSetIndentLevel $ expDoc1
            ]
          ]
        , docLines
          [ docAddBaseY BrIndentRegular
          $ docPar
            (appSep $ docLit $ Text.pack "let")
            (docSetIndentLevel $ return bindDoc)
          , docAddBaseY BrIndentRegular
          $ docPar
            (appSep $ docLit $ Text.pack "in")
            (docSetIndentLevel $ expDoc1)
          ]
        ]
      Just bindDocs@(_:_) -> docAlt
        [ docLines
          [ docSeq
            [ appSep $ docLit $ Text.pack "let"
            , docSetBaseY $ docSetIndentLevel $ docLines $ return <$> bindDocs
            ]
          , docSeq
            [ appSep $ docLit $ Text.pack "in "
            , docSetBaseY $ docSetIndentLevel $ expDoc1
            ]
          ]
        , docLines
          [ docAddBaseY BrIndentRegular
          $ docPar
            (appSep $ docLit $ Text.pack "let")
            (docSetIndentLevel $ docLines $ return <$> bindDocs)
          , docAddBaseY BrIndentRegular
          $ docPar
            (appSep $ docLit $ Text.pack "in")
            (docSetIndentLevel $ expDoc1)
          ]
        ]
      _ -> docSeq [appSep $ docLit $ Text.pack "let in", expDoc1]
    -- docSeq [appSep $ docLit "let in", expDoc1]
  HsDo DoExpr (L _ stmts) _ -> do
    stmtDocs <- docSharedWrapper layoutStmt `mapM` stmts
    docAddBaseY BrIndentRegular
           $ docPar
               (docLit $ Text.pack "do")
               (docSetIndentLevel $ docNonBottomSpacing $ docLines stmtDocs)
  HsDo x  (L _ stmts) _ | case x of { ListComp -> True
                                    ; MonadComp -> True
                                    ; _ -> False } -> do
    stmtDocs <- docSharedWrapper layoutStmt `mapM` stmts
    docAlt
      [ docSeq
        [ appSep $ docLit $ Text.pack "["
        , appSep $ docForceSingleline $ List.last stmtDocs
        , appSep $ docLit $ Text.pack "|"
        , docSeq $ List.intersperse docCommaSep
                $ fmap docForceSingleline $ List.init stmtDocs
        , docLit $ Text.pack "]"
        ]
      , let
          start = docCols ColListComp
                    [appSep $ docLit $ Text.pack "[", List.last stmtDocs]
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
    briDocByExact lexpr
  ExplicitList _ _ elems@(_:_) -> do
    elemDocs <- elems `forM` docSharedWrapper layoutExpr
    docAlt
      [ docSeq
      $  [docLit $ Text.pack "["]
      ++ List.intersperse docCommaSep (docForceSingleline <$> elemDocs)
      ++ [docLit $ Text.pack "]"]
      , let
          start = docCols ColList
                    [appSep $ docLit $ Text.pack "[", List.head elemDocs]
          lines = List.tail elemDocs <&> \d ->
                  docCols ColList [docCommaSep, d]
          end   = docLit $ Text.pack "]"
        in docSetBaseY $ docLines $ [start] ++ lines ++ [end]
      ]
  ExplicitList _ _ [] ->
    docLit $ Text.pack "[]"
  ExplicitPArr{} -> do
    -- TODO
    briDocByExact lexpr
  RecordCon lname _ _ (HsRecFields [] Nothing) -> do
    let t = lrdrNameToText lname
    docLit $ t <> Text.pack "{}"
  RecordCon lname _ _ (HsRecFields fs@(_:_) Nothing) -> do
    let t = lrdrNameToText lname
    (fd1:fdr) <- fs `forM` \(L _ (HsRecField (L _ (FieldOcc lnameF _)) fExpr _)) -> do
      fExpDoc <- docSharedWrapper layoutExpr fExpr
      return $ (lrdrNameToText lnameF, fExpDoc)
    docAlt
      [ docAddBaseY BrIndentRegular
      $ docPar
          (docLit t)
          (docLines $ let
            line1 = docCols ColRecUpdate
              [ appSep $ docLit $ Text.pack "{"
              , appSep $ docLit $ fst fd1
              , docSeq [ appSep $ docLit $ Text.pack "="
                      , docAddBaseY BrIndentRegular $ snd fd1
                      ]
              ]
            lineR = fdr <&> \(fText, fDoc) -> docCols ColRecUpdate
              [ appSep $ docLit $ Text.pack ","
              , appSep $ docLit $ fText
              , docSeq [ appSep $ docLit $ Text.pack "="
                      , docAddBaseY BrIndentRegular fDoc
                      ]
              ]
            lineN = docLit $ Text.pack "}"
            in [line1] ++ lineR ++ [lineN])
      -- TODO oneliner (?)
      ]
  RecordCon{} ->
    unknownNodeError "RecordCon with puns" lexpr
  RecordUpd rExpr [] _ _ _ _ -> do
    rExprDoc <- docSharedWrapper layoutExpr rExpr
    docSeq [rExprDoc, docLit $ Text.pack "{}"]
  RecordUpd rExpr fields@(_:_) _ _ _ _ -> do
    rExprDoc <- docSharedWrapper layoutExpr rExpr
    rFs@(rF1:rFr) <- fields `forM` \(L _ (HsRecField (L _ ambName) rFExpr _)) -> do
      rFExpDoc <- docSharedWrapper layoutExpr rFExpr
      return $ case ambName of
        Unambiguous n _ -> (lrdrNameToText n, rFExpDoc)
        Ambiguous   n _ -> (lrdrNameToText n, rFExpDoc)
    docAlt
      -- singleline
      [ docSeq
        [ appSep rExprDoc
        , appSep $ docLit $ Text.pack "{"
        , appSep $ docSeq $ List.intersperse docCommaSep
                $ rFs <&> \(fieldStr, fieldDoc) ->
                    docSeq [ appSep $ docLit fieldStr
                          , appSep $ docLit $ Text.pack "="
                          , docForceSingleline fieldDoc
                          ]
        , docLit $ Text.pack "}"
        ]
      -- wild-indentation block
      , docSeq
        [ appSep rExprDoc
        , docSetBaseY $ docLines $ let
            line1 = docCols ColRecUpdate
              [ appSep $ docLit $ Text.pack "{"
              , appSep $ docLit $ fst rF1
              , docSeq [ appSep $ docLit $ Text.pack "="
                      , docForceSingleline $ snd rF1
                      ]
              ]
            lineR = rFr <&> \(fText, fDoc) -> docCols ColRecUpdate
              [ appSep $ docLit $ Text.pack ","
              , appSep $ docLit $ fText
              , docSeq [ appSep $ docLit $ Text.pack "="
                      , docForceSingleline fDoc
                      ]
              ]
            lineN = docLit $ Text.pack "}"
            in [line1] ++ lineR ++ [lineN]
        ]
      -- strict indentation block
      , docAddBaseY BrIndentRegular
      $ docPar
          rExprDoc
          (docLines $ let
            line1 = docCols ColRecUpdate
              [ appSep $ docLit $ Text.pack "{"
              , appSep $ docLit $ fst rF1
              , docSeq [ appSep $ docLit $ Text.pack "="
                      , docAddBaseY BrIndentRegular $ snd rF1
                      ]
              ]
            lineR = rFr <&> \(fText, fDoc) -> docCols ColRecUpdate
              [ appSep $ docLit $ Text.pack ","
              , appSep $ docLit $ fText
              , docSeq [ appSep $ docLit $ Text.pack "="
                      , docAddBaseY BrIndentRegular fDoc
                      ]
              ]
            lineN = docLit $ Text.pack "}"
            in [line1] ++ lineR ++ [lineN])
      ]
  ExprWithTySig exp1 (HsIB _ (HsWC _ _ typ1)) -> do
    expDoc <- docSharedWrapper layoutExpr exp1
    typDoc <- docSharedWrapper layoutType typ1
    docSeq
      [ appSep expDoc
      , appSep $ docLit $ Text.pack "::"
      , typDoc
      ]
  ExprWithTySigOut{} -> do
    -- TODO
    briDocByExact lexpr
  ArithSeq _ Nothing info ->
    case info of
      From e1 -> do
        e1Doc <- docSharedWrapper layoutExpr e1
        docSeq
          [ docLit $ Text.pack "["
          , docForceSingleline e1Doc
          , docLit $ Text.pack "..]"
          ]
      FromThen e1 e2 -> do
        e1Doc <- docSharedWrapper layoutExpr e1
        e2Doc <- docSharedWrapper layoutExpr e2
        docSeq
          [ docLit $ Text.pack "["
          , docForceSingleline e1Doc
          , docLit $ Text.pack ","
          , docForceSingleline e2Doc
          , docLit $ Text.pack "..]"
          ]
      FromTo e1 eN -> do
        e1Doc <- docSharedWrapper layoutExpr e1
        eNDoc <- docSharedWrapper layoutExpr eN
        docSeq
          [ docLit $ Text.pack "["
          , docForceSingleline e1Doc
          , docLit $ Text.pack ".."
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
          , docLit $ Text.pack ","
          , docForceSingleline e2Doc
          , docLit $ Text.pack ".."
          , docForceSingleline eNDoc
          , docLit $ Text.pack "]"
          ]
  ArithSeq{} ->
    unknownNodeError "ArithSeq" lexpr
  PArrSeq{} -> do
    -- TODO
    briDocByExact lexpr
  HsSCC{} -> do
    -- TODO
    briDocByExact lexpr
  HsCoreAnn{} -> do
    -- TODO
    briDocByExact lexpr
  HsBracket{} -> do
    -- TODO
    briDocByExact lexpr
  HsRnBracketOut{} -> do
    -- TODO
    briDocByExact lexpr
  HsTcBracketOut{} -> do
    -- TODO
    briDocByExact lexpr
  HsSpliceE{} -> do
    -- TODO
    briDocByExact lexpr
  HsProc{} -> do
    -- TODO
    briDocByExact lexpr
  HsStatic{} -> do
    -- TODO
    briDocByExact lexpr
  HsArrApp{} -> do
    -- TODO
    briDocByExact lexpr
  HsArrForm{} -> do
    -- TODO
    briDocByExact lexpr
  HsTick{} -> do
    -- TODO
    briDocByExact lexpr
  HsBinTick{} -> do
    -- TODO
    briDocByExact lexpr
  HsTickPragma{} -> do
    -- TODO
    briDocByExact lexpr
  EWildPat{} -> do
    -- TODO
    briDocByExact lexpr
  EAsPat{} -> do
    -- TODO
    briDocByExact lexpr
  EViewPat{} -> do
    -- TODO
    briDocByExact lexpr
  ELazyPat{} -> do
    -- TODO
    briDocByExact lexpr
  HsWrap{} -> do
    -- TODO
    briDocByExact lexpr

isExpressionTypeHeadPar :: LHsExpr RdrName -> Bool
isExpressionTypeHeadPar (L _ expr) = case expr of
  RecordCon{} -> True
  RecordUpd{} -> True
  HsDo{} -> True
  HsIf{} -> True
  HsCase{} -> True
  HsLamCase{} -> True
  -- TODO: these cases might have unfortunate layouts, if for some reason
  -- the first operand is multiline.
  OpApp _ _ _ (L _ HsDo{}) -> True
  OpApp _ _ _ (L _ HsLamCase{}) -> True
  _ -> False

isExpressionTypeHeadPar' :: LHsExpr RdrName -> Bool
isExpressionTypeHeadPar' (L _ expr) = case expr of
  RecordCon{} -> True
  RecordUpd{} -> True
  HsDo{} -> True
  HsIf{} -> True
  HsCase{} -> True
  HsLamCase{} -> True
  -- TODO: these cases might have unfortunate layouts, if for some reason
  -- the first operand is multiline.
  OpApp _ _ _ (L _ HsDo{}) -> True
  OpApp _ _ _ (L _ HsLamCase{}) -> True
  HsApp (L _ HsVar{}) _ -> True
  HsApp (L _ (HsApp (L _ HsVar{}) _)) _ -> True
  HsApp (L _ (HsApp (L _ (HsApp (L _ HsVar{}) _)) _)) _ -> True -- TODO: the obvious
  _ -> False

litBriDoc :: HsLit -> BriDocFInt
litBriDoc = \case
  HsChar       t _c           -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ ['\'', c, '\'']
  HsCharPrim   t _c           -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ ['\'', c, '\'']
  HsString     t _fastString  -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ FastString.unpackFS fastString
  HsStringPrim t _byteString  -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ Data.ByteString.Char8.unpack byteString
  HsInt        t _i           -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsIntPrim    t _i           -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsWordPrim   t _i           -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsInt64Prim  t _i           -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsWord64Prim t _i           -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsInteger    t _i _type     -> BDFLit $ Text.pack t -- BDFLit $ Text.pack $ show i
  HsRat        (FL t _) _type -> BDFLit $ Text.pack t
  HsFloatPrim  (FL t _)       -> BDFLit $ Text.pack t
  HsDoublePrim (FL t _)       -> BDFLit $ Text.pack t

overLitValBriDoc :: OverLitVal -> BriDocFInt
overLitValBriDoc = \case
  HsIntegral t _        -> BDFLit $ Text.pack t
  HsFractional (FL t _) -> BDFLit $ Text.pack t
  HsIsString t _        -> BDFLit $ Text.pack t
