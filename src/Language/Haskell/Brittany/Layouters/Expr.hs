{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Layouters.Expr
  ( layoutExpr
  , litBriDoc
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



layoutExpr :: ToBriDoc HsExpr
layoutExpr lexpr@(L _ expr) = fmap (docWrapNode lexpr)
                            $ case expr of
  HsVar vname -> do
    BDLit <$> lrdrNameToTextAnn vname
  HsUnboundVar var -> return $ case var of
    OutOfScope oname _ -> BDLit $ Text.pack $ occNameString oname
    TrueExprHole oname -> BDLit $ Text.pack $ occNameString oname
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
    return $ overLitValBriDoc olit
  HsLit lit -> do
    return $ litBriDoc lit
  HsLam (MG (L _ [L _ (Match _ pats _ (GRHSs [L _ (GRHS [] body)] (L _ EmptyLocalBinds)))]) _ _ _) -> do
    patDocs <- pats `forM` layoutPat
    bodyDoc <- BDAddBaseY BrIndentRegular <$> layoutExpr body
    let funcPatternPartLine =
          BDCols ColCasePattern
            $ (patDocs <&> (\p -> BDSeq [BDForceSingleline p, BDSeparator]))
    return $ BDAlt
      [ BDSeq
        [ BDLit $ Text.pack "\\"
        , funcPatternPartLine
        , appSep $ BDLit $ Text.pack "->"
        , bodyDoc
        ]
      -- TODO
      ]
  HsLam{} ->
    unknownNodeError "HsLam too complex" lexpr
  HsLamCase _ (MG (L _ matches) _ _ _) -> do
    funcPatDocs <- matches `forM` \(L _ (Match _
                                        pats
                                        _mType -- not an actual type sig
                                        (GRHSs grhss whereBinds))) -> do
      patDocs <- pats `forM` layoutPat
      let funcPatternPartLine = case patDocs of
            ps -> BDCols ColFuncPatternsPrefix
              $ (ps <&> (\p -> BDSeq [BDForceSingleline p, BDSeparator]))
      grhssDocsNoInd <- do
        case grhss of
          [grhs1] -> layoutGrhsLCase (Just funcPatternPartLine) grhs1
          (grhs1:grhsr) -> do
            grhsDoc1 <- layoutGrhsLCase (Just funcPatternPartLine) grhs1
            grhsDocr <- layoutGrhsLCase Nothing `mapM` grhsr
            return $ BDLines $ grhsDoc1 : grhsDocr
          [] -> error "layoutBind grhssDocsNoInd"
      let grhssDocs = BDAlt [grhssDocsNoInd {-, grhssDocsInd TODO-}]
      layoutLocalBinds whereBinds >>= \case
        Nothing -> return $ grhssDocs
        Just whereDocs -> do
          return $ BDAddBaseY BrIndentRegular
                 $ docPar grhssDocs 
                 $ BDAddBaseY BrIndentRegular
                 $ docPar (BDLit $ Text.pack "where")
                 $ BDSetIndentLevel $ BDLines whereDocs
    return $ BDAddBaseY BrIndentRegular $ docPar
      (BDLit $ Text.pack "\\case")
      (BDLines funcPatDocs)
  HsApp exp1 exp2 -> do
    -- TODO: if expDoc1 is some literal, we may want to create a BDCols here.
    expDoc1 <- layoutExpr exp1
    expDoc2 <- layoutExpr exp2
    return $ BDAlt
      [ BDSeq [appSep $ BDForceSingleline expDoc1, BDForceSingleline expDoc2]
      , BDAddBaseY BrIndentRegular
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
  OpApp expLeft expOp _ expRight -> do
    expDocLeft  <- layoutExpr expLeft
    expDocOp    <- layoutExpr expOp
    expDocRight <- layoutExpr expRight
    return $ BDAlt
      [ BDSeq
        [ appSep $ BDForceSingleline expDocLeft
        , appSep $ BDForceSingleline expDocOp
        , BDForceSingleline expDocRight
        ]
      , BDAddBaseY BrIndentRegular
      $ docPar
          expDocLeft
          -- TODO: turn this into BDCols?
          (BDSeq [appSep $ expDocOp, expDocRight])
      ]
  NegApp{} -> do
    -- TODO
    briDocByExact lexpr
  HsPar innerExp -> do
    innerExpDoc <- layoutExpr innerExp
    return $ BDAlt
      [ BDSeq
        [ BDLit $ Text.pack "("
        , BDForceSingleline innerExpDoc
        , BDLit $ Text.pack ")"
        ]
      -- TODO
      ]
  SectionL{} -> do
    -- TODO
    briDocByExact lexpr
  SectionR{} -> do
    -- TODO
    briDocByExact lexpr
  ExplicitTuple args boxity
    | Just argExprs <- args `forM` (\case (L _ (Present e)) -> Just e; _ -> Nothing) -> do
    argDocs <- layoutExpr `mapM` argExprs
    return $ case boxity of
      Boxed -> BDAlt
        [ BDSeq
        $  [ BDLit $ Text.pack "(" ]
        ++ List.intersperse (appSep $ BDLit $ Text.pack ",") argDocs
        ++ [ BDLit $ Text.pack ")"]
        -- TODO
        ]
      Unboxed -> BDAlt
        [ BDSeq
        $  [ BDLit $ Text.pack "(#" ]
        ++ List.intersperse (appSep $ BDLit $ Text.pack ",") argDocs
        ++ [ BDLit $ Text.pack "#)"]
        -- TODO
        ]
  ExplicitTuple{} ->
    unknownNodeError "ExplicitTuple|.." lexpr 
  HsCase cExp (MG (L _ matches) _ _ _) -> do
    cExpDoc <- layoutExpr cExp
    funcPatDocs <- matches `forM` \(L _ (Match _
                                        pats
                                        _mType -- not an actual type sig
                                        (GRHSs grhss whereBinds))) -> do
      patDocs <- pats `forM` layoutPat
      let funcPatternPartLine =
            BDCols ColCasePattern
              $ (patDocs <&> (\p -> BDSeq [BDForceSingleline p, BDSeparator]))
      grhssDocsNoInd <- do
        case grhss of
          [grhs1] -> layoutGrhsCase (Just funcPatternPartLine) grhs1
          (grhs1:grhsr) -> do
            grhsDoc1 <- layoutGrhsCase (Just funcPatternPartLine) grhs1
            grhsDocr <- layoutGrhsCase Nothing `mapM` grhsr
            return $ BDLines $ grhsDoc1 : grhsDocr
          [] -> error "layoutBind grhssDocsNoInd"
      let grhssDocs = BDAlt [grhssDocsNoInd {-, grhssDocsInd TODO-}]
      layoutLocalBinds whereBinds >>= \case
        Nothing -> return $ grhssDocs
        Just lhsBindsLRDoc -> do
          return $ BDAddBaseY BrIndentRegular
                 $ docPar grhssDocs 
                 $ BDAddBaseY BrIndentRegular
                 $ docPar (BDLit $ Text.pack "where")
                 $ BDSetIndentLevel $ BDLines lhsBindsLRDoc
    return $ BDAlt
      [ BDAddBaseY BrIndentRegular
      $ docPar
        ( BDSeq
          [ appSep $ BDLit $ Text.pack "case"
          , appSep $ BDForceSingleline cExpDoc
          , BDLit $ Text.pack "of"
          ])
        (BDSetIndentLevel $ BDLines funcPatDocs)
      , docPar
          ( BDAddBaseY BrIndentRegular
          $ docPar (BDLit $ Text.pack "case") cExpDoc
          )
          ( BDAddBaseY BrIndentRegular
          $ docPar (BDLit $ Text.pack "of")
            (BDSetIndentLevel $ BDLines funcPatDocs)
          )
      ]
  HsIf _ ifExpr thenExpr elseExpr -> do
    ifExprDoc   <- layoutExpr ifExpr
    thenExprDoc <- layoutExpr thenExpr
    elseExprDoc <- layoutExpr elseExpr
    return $ BDAlt
      [ BDSeq
        [ appSep $ BDLit $ Text.pack "if"
        , appSep $ BDForceSingleline ifExprDoc
        , appSep $ BDLit $ Text.pack "then"
        , appSep $ BDForceSingleline thenExprDoc
        , appSep $ BDLit $ Text.pack "else"
        , BDForceSingleline elseExprDoc
        ]
      , BDAddBaseY BrIndentRegular
      $ docPar
          ( BDAddBaseY (BrIndentSpecial 3)
          $ BDSeq [appSep $ BDLit $ Text.pack "if", ifExprDoc])
          (BDLines
            [ BDAddBaseY BrIndentRegular
            $ BDAlt
              [ BDSeq [appSep $ BDLit $ Text.pack "then", BDForceSingleline thenExprDoc]
              , BDAddBaseY BrIndentRegular
              $ docPar (BDLit $ Text.pack "then") thenExprDoc
              ]
            , BDAddBaseY BrIndentRegular
            $ BDAlt
              [ BDSeq [appSep $ BDLit $ Text.pack "else", BDForceSingleline elseExprDoc]
              , BDAddBaseY BrIndentRegular
              $ docPar (BDLit $ Text.pack "else") elseExprDoc
              ]
            ])
      , BDLines
        [ BDAddBaseY (BrIndentSpecial 3)
        $ BDSeq [appSep $ BDLit $ Text.pack "if", ifExprDoc]
        , BDAddBaseY BrIndentRegular
        $ docPar (BDLit $ Text.pack "then") thenExprDoc
        , BDAddBaseY BrIndentRegular
        $ docPar (BDLit $ Text.pack "else") elseExprDoc
        ]
      ]
  HsMultiIf _ cases -> do
    caseDocs <- cases `forM` layoutGrhsMWIf
    return $ BDAddBaseY BrIndentRegular $ docPar
      (BDLit $ Text.pack "if")
      (BDLines caseDocs)
  HsLet{} -> do
    -- TODO
    briDocByExact lexpr
  HsDo DoExpr (L _ stmts) _ -> do
    stmtDocs <- layoutStmt `mapM` stmts
    return $ BDAddBaseY BrIndentRegular
           $ docPar
               (BDLit $ Text.pack "do")
               (BDSetIndentLevel $ BDLines stmtDocs)
  HsDo x  (L _ stmts) _ | case x of { ListComp -> True
                                    ; MonadComp -> True
                                    ; _ -> False } -> do
    stmtDocs <- layoutStmt `mapM` stmts
    return $ BDAlt
      [ BDSeq
        [ appSep $ BDLit $ Text.pack "["
        , appSep $ BDForceSingleline $ List.last stmtDocs
        , appSep $ BDLit $ Text.pack "|"
        , BDSeq $ List.intersperse docCommaSep
                $ fmap BDForceSingleline $ List.init stmtDocs
        , BDLit $ Text.pack "]"
        ]
      , let
          start = BDCols ColListComp
                    [appSep $ BDLit $ Text.pack "[", List.last stmtDocs]
          (s1:sM) = List.init stmtDocs
          line1 = BDCols ColListComp
                    [appSep $ BDLit $ Text.pack "|", s1]
          lineM = sM <&> \d ->
                  BDCols ColListComp [docCommaSep, d]
          end   = BDLit $ Text.pack "]"
        in BDSetBaseY $ BDLines $ [start, line1] ++ lineM ++ [end]
      ]
  HsDo{} -> do
    -- TODO
    briDocByExact lexpr
  ExplicitList _ _ elems@(_:_) -> do
    elemDocs <- elems `forM` layoutExpr
    return $ BDAlt
      [ BDSeq
      $  [BDLit $ Text.pack "["]
      ++ List.intersperse docCommaSep (BDForceSingleline <$> elemDocs)
      ++ [BDLit $ Text.pack "]"]
      , let
          start = BDCols ColList
                    [appSep $ BDLit $ Text.pack "[", List.head elemDocs]
          lines = List.tail elemDocs <&> \d ->
                  BDCols ColList [docCommaSep, d]
          end   = BDLit $ Text.pack "]"
        in BDSetBaseY $ BDLines $ [start] ++ lines ++ [end]
      ]
  ExplicitList _ _ [] ->
    return $ BDLit $ Text.pack "[]"
  ExplicitPArr{} -> do
    -- TODO
    briDocByExact lexpr
  RecordCon lname _ _ (HsRecFields [] Nothing) -> do
    let t = lrdrNameToText lname
    return $ BDLit $ t <> Text.pack "{}"
  RecordCon lname _ _ (HsRecFields fs@(_:_) Nothing) -> do
    let t = lrdrNameToText lname
    (fd1:fdr) <- fs `forM` \(L _ (HsRecField (L _ (FieldOcc lnameF _)) fExpr _)) -> do
      fExpDoc <- layoutExpr fExpr
      return $ (lrdrNameToText lnameF, fExpDoc)
    return $ BDAlt
      [ BDAddBaseY BrIndentRegular
      $ docPar
          (BDLit t)
          (BDLines $ let
            line1 = BDCols ColRecUpdate
              [ appSep $ BDLit $ Text.pack "{"
              , appSep $ BDLit $ fst fd1
              , BDSeq [ appSep $ BDLit $ Text.pack "="
                      , BDAddBaseY BrIndentRegular $ snd fd1
                      ]
              ]
            lineR = fdr <&> \(fText, fDoc) -> BDCols ColRecUpdate
              [ appSep $ BDLit $ Text.pack ","
              , appSep $ BDLit $ fText
              , BDSeq [ appSep $ BDLit $ Text.pack "="
                      , BDAddBaseY BrIndentRegular fDoc
                      ]
              ]
            lineN = BDLit $ Text.pack "}"
            in [line1] ++ lineR ++ [lineN])
      -- TODO oneliner (?)
      ]
  RecordCon{} ->
    unknownNodeError "RecordCon with puns" lexpr
  RecordUpd rExpr [] _ _ _ _ -> do
    rExprDoc <- layoutExpr rExpr
    return $ BDSeq [rExprDoc, BDLit $ Text.pack "{}"]
  RecordUpd rExpr fields@(_:_) _ _ _ _ -> do
    rExprDoc <- layoutExpr rExpr
    rF1:rFr <- fields `forM` \(L _ (HsRecField (L _ ambName) rFExpr _)) -> do
      rFExpDoc <- layoutExpr rFExpr
      return $ case ambName of
        Unambiguous n _ -> (lrdrNameToText n, rFExpDoc)
        Ambiguous   n _ -> (lrdrNameToText n, rFExpDoc)
    return $ BDAlt
      [ BDAddBaseY BrIndentRegular
      $ docPar
          rExprDoc
          (BDLines $ let
            line1 = BDCols ColRecUpdate
              [ appSep $ BDLit $ Text.pack "{"
              , appSep $ BDLit $ fst rF1
              , BDSeq [ appSep $ BDLit $ Text.pack "="
                      , BDAddBaseY BrIndentRegular $ snd rF1
                      ]
              ]
            lineR = rFr <&> \(fText, fDoc) -> BDCols ColRecUpdate
              [ appSep $ BDLit $ Text.pack ","
              , appSep $ BDLit $ fText
              , BDSeq [ appSep $ BDLit $ Text.pack "="
                      , BDAddBaseY BrIndentRegular fDoc
                      ]
              ]
            lineN = BDLit $ Text.pack "}"
            in [line1] ++ lineR ++ [lineN])
      -- TODO oneliner (?)
      ]
  ExprWithTySig{} -> do
    -- TODO
    briDocByExact lexpr
  ExprWithTySigOut{} -> do
    -- TODO
    briDocByExact lexpr
  ArithSeq _ Nothing info ->
    case info of
      From e1 -> do
        e1Doc <- layoutExpr e1
        return $ BDSeq
          [ BDLit $ Text.pack "["
          , BDForceSingleline e1Doc
          , BDLit $ Text.pack "..]"
          ]
      FromThen e1 e2 -> do
        e1Doc <- layoutExpr e1
        e2Doc <- layoutExpr e2
        return $ BDSeq
          [ BDLit $ Text.pack "["
          , BDForceSingleline e1Doc
          , BDLit $ Text.pack ","
          , BDForceSingleline e2Doc
          , BDLit $ Text.pack "..]"
          ]
      FromTo e1 eN -> do
        e1Doc <- layoutExpr e1
        eNDoc <- layoutExpr eN
        return $ BDSeq
          [ BDLit $ Text.pack "["
          , BDForceSingleline e1Doc
          , BDLit $ Text.pack ".."
          , BDForceSingleline eNDoc
          , BDLit $ Text.pack "]"
          ]
      FromThenTo e1 e2 eN -> do
        e1Doc <- layoutExpr e1
        e2Doc <- layoutExpr e2
        eNDoc <- layoutExpr eN
        return $ BDSeq
          [ BDLit $ Text.pack "["
          , BDForceSingleline e1Doc
          , BDLit $ Text.pack ","
          , BDForceSingleline e2Doc
          , BDLit $ Text.pack ".."
          , BDForceSingleline eNDoc
          , BDLit $ Text.pack "]"
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


layoutGrhsCase :: Maybe BriDoc -> ToBriDoc' (GRHS RdrName (LHsExpr RdrName))
layoutGrhsCase mPatPart lgrhs@(L _ (GRHS guards body)) = do
  bodyDoc <- BDAddBaseY BrIndentRegular
         <$> layoutExpr body
  let patPart = fromMaybe BDEmpty mPatPart
  docWrapNode lgrhs <$> case guards of
    [] ->
      return $ BDCols ColEquation [patPart, BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]]
    [guard1] -> do
      guardDoc1 <- layoutGuardLStmt guard1
      return $ BDAlt
        [ BDCols ColGuardedEquation
          [ patPart
          , BDSeq [BDLit $ Text.pack "| ", appSep $ guardDoc1]
          , BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]
          ]
        , BDAddBaseY BrIndentRegular
        $ docPar patPart
        $ BDSeq
          [ BDLit $ Text.pack "| "
          , guardDoc1
          , appSep $ BDSeq [BDLit $ Text.pack "->"]
          , bodyDoc
          ]
        , BDAddBaseY BrIndentRegular
        $ docPar patPart
        $ BDLines
          [ BDSeq [appSep $ BDLit $ Text.pack "|", appSep guardDoc1]
          , BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]
          ]
        ]
    (guard1:guardr) -> do
      guardDoc1 <- layoutGuardLStmt guard1
      guardDocr <- layoutGuardLStmt `mapM` guardr
      let hat = BDCols ColGuardedEquation
            [patPart, BDSeq [appSep $ BDLit $ Text.pack "|", appSep guardDoc1]]
          middle = guardDocr <&> \gd -> BDCols ColGuardedEquation
            [BDEmpty, BDSeq [appSep $ BDLit $ Text.pack ",", gd]]
          last = BDCols ColGuardedEquation
            [BDEmpty, BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]]
      return $ BDAlt
        [ BDCols ColGuardedEquation
          [ BDForceSingleline patPart
          , BDSeq $ [appSep $ BDLit $ Text.pack "|", appSep $ BDForceSingleline guardDoc1]
                 ++ (guardDocr >>= \gd ->
                      [appSep $ BDLit $ Text.pack ",", BDForceSingleline gd])
          , BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]
          ]
        , BDLines $ [hat] ++ middle ++ [last]
        ]

layoutGrhsMWIf :: ToBriDoc' (GRHS RdrName (LHsExpr RdrName))
layoutGrhsMWIf lgrhs@(L _ (GRHS guards body)) = do
  bodyDoc <- BDAddBaseY BrIndentRegular
         <$> layoutExpr body
  docWrapNode lgrhs <$> case guards of
    [] ->
      unknownNodeError "layoutGrhsMWIf no guards" lgrhs
    [guard1] -> do
      guardDoc1 <- layoutGuardLStmt guard1
      return $ BDAlt
        [ BDCols ColGuardedEquation
          [ BDSeq [appSep $ BDLit $ Text.pack "|", appSep $ BDForceSingleline guardDoc1]
          , BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]
          ]
        , BDLines
          [ BDSeq [appSep $ BDLit $ Text.pack "|", appSep guardDoc1, BDLit $ Text.pack "->"]
          , BDEnsureIndent BrIndentRegular $ bodyDoc
          ]
        ]
    (guard1:guardr) -> do
      guardDoc1 <- layoutGuardLStmt guard1
      guardDocr <- layoutGuardLStmt `mapM` guardr
      let hat = BDCols ColGuardedEquation
            [BDSeq [appSep $ BDLit $ Text.pack "|", guardDoc1]]
          middle = guardDocr <&> \gd -> BDCols ColGuardedEquation
            [BDSeq [appSep $ BDLit $ Text.pack " ,", appSep gd, BDLit $ Text.pack "->"]]
          last = BDCols ColGuardedEquation
            [BDSeq [BDLit $ Text.pack "  ", bodyDoc]]
      return $ BDAlt
        [ BDCols ColGuardedEquation
          [ BDSeq $ [appSep $ BDLit $ Text.pack "|", BDForceSingleline guardDoc1]
                 ++ (guardDocr >>= \gd ->
                      [appSep $ BDLit $ Text.pack ",", BDForceSingleline gd])
          , BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]
          ]
        , BDLines $ [hat] ++ middle ++ [last]
        ]

layoutGrhsLCase :: Maybe BriDoc -> ToBriDoc' (GRHS RdrName (LHsExpr RdrName))
layoutGrhsLCase mPatPart lgrhs@(L _ (GRHS guards body)) = do
  bodyDoc <- BDAddBaseY BrIndentRegular <$> layoutExpr body
  let patPart = fromMaybe BDEmpty mPatPart
  docWrapNode lgrhs <$> case guards of
    [] ->
      return $ BDCols ColEquation [patPart, BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]]
    [guard1] -> do
      guardDoc1 <- layoutGuardLStmt guard1
      return $ BDAlt
        [ BDCols ColGuardedEquation
          [ patPart
          , BDSeq [appSep $ BDLit $ Text.pack "|", appSep guardDoc1]
          , BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]
          ]
        , BDAddBaseY BrIndentRegular
        $ docPar patPart
        $ BDSeq
          [ BDLit $ Text.pack "| "
          , guardDoc1
          , appSep $ BDSeq [BDLit $ Text.pack "->"]
          , bodyDoc
          ]
        , BDAddBaseY BrIndentRegular
        $ docPar patPart
        $ BDLines
          [ BDSeq [appSep $ BDLit $ Text.pack "|", appSep guardDoc1]
          , BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]
          ]
        ]
    (guard1:guardr) -> do
      guardDoc1 <- layoutGuardLStmt guard1
      guardDocr <- layoutGuardLStmt `mapM` guardr
      let hat = BDCols ColGuardedEquation
            [patPart, BDSeq [appSep $ BDLit $ Text.pack "|", guardDoc1]]
          middle = guardDocr <&> \gd -> BDCols ColGuardedEquation
            [BDEmpty, BDSeq [appSep $ BDLit $ Text.pack ",", gd]]
          last = BDCols ColGuardedEquation
            [BDEmpty, BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]]
      return $ BDAlt
        [ BDCols ColGuardedEquation
          [ BDForceSingleline patPart
          , BDSeq $ [appSep $ BDLit $ Text.pack "|", appSep $ BDForceSingleline guardDoc1]
                 ++ (guardDocr >>= \gd ->
                      [appSep $ BDLit $ Text.pack ",", appSep $ BDForceSingleline gd])
          , BDSeq [appSep $ BDLit $ Text.pack "->", bodyDoc]
          ]
        , BDLines $ [hat] ++ middle ++ [last]
        ]

litBriDoc :: HsLit -> BriDoc
litBriDoc = \case
  HsChar       t _c           -> BDLit $ Text.pack t -- BDLit $ Text.pack $ ['\'', c, '\'']
  HsCharPrim   t _c           -> BDLit $ Text.pack t -- BDLit $ Text.pack $ ['\'', c, '\'']
  HsString     t _fastString  -> BDLit $ Text.pack t -- BDLit $ Text.pack $ FastString.unpackFS fastString
  HsStringPrim t _byteString  -> BDLit $ Text.pack t -- BDLit $ Text.pack $ Data.ByteString.Char8.unpack byteString
  HsInt        t _i           -> BDLit $ Text.pack t -- BDLit $ Text.pack $ show i
  HsIntPrim    t _i           -> BDLit $ Text.pack t -- BDLit $ Text.pack $ show i
  HsWordPrim   t _i           -> BDLit $ Text.pack t -- BDLit $ Text.pack $ show i
  HsInt64Prim  t _i           -> BDLit $ Text.pack t -- BDLit $ Text.pack $ show i
  HsWord64Prim t _i           -> BDLit $ Text.pack t -- BDLit $ Text.pack $ show i
  HsInteger    t _i _type     -> BDLit $ Text.pack t -- BDLit $ Text.pack $ show i
  HsRat        (FL t _) _type -> BDLit $ Text.pack t
  HsFloatPrim  (FL t _)       -> BDLit $ Text.pack t
  HsDoublePrim (FL t _)       -> BDLit $ Text.pack t

overLitValBriDoc :: OverLitVal -> BriDoc
overLitValBriDoc = \case
  HsIntegral t _        -> BDLit $ Text.pack t
  HsFractional (FL t _) -> BDLit $ Text.pack t
  HsIsString t _        -> BDLit $ Text.pack t
