{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Layouters.Decl
  ( layoutSig
  , layoutBind
  , layoutLocalBinds
  , layoutGuardLStmt
  , layoutGrhs
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
import           Language.Haskell.Brittany.Layouters.Pattern

import           Bag ( mapBagM )



layoutSig :: ToBriDoc Sig
layoutSig lsig@(L _loc sig) = case sig of
  TypeSig names (HsIB _ (HsWC _ _ typ)) -> do
    nameStrs <- names `forM` lrdrNameToTextAnn
    let nameStr = Text.intercalate (Text.pack ", ") $ nameStrs
    typeDoc <- layoutType typ
    return $ docWrapNode lsig $ docAlt
      [ docSeq
        [ docPostComment lsig $ docLit nameStr
        , docLit $ Text.pack " :: "
        , BDForceSingleline typeDoc
        ]
      , BDAddBaseY BrIndentRegular
      $ docPar
        (docPostComment lsig $ docLit nameStr)
        ( BDCols ColTyOpPrefix
          [ docLit $ Text.pack ":: "
          , BDAddBaseY (BrIndentSpecial 3) $ typeDoc
          ]
        )
      ]
  _ -> briDocByExact lsig -- TODO: should not be necessary

layoutGuardLStmt :: ToBriDoc' (Stmt RdrName (LHsExpr RdrName))
layoutGuardLStmt lgstmt@(L _ stmtLR) = case stmtLR of
  BodyStmt body _ _ _ -> layoutExpr body
  _ -> briDocByExact lgstmt -- TODO

layoutGrhs :: Maybe BriDoc -> ToBriDoc' (GRHS RdrName (LHsExpr RdrName))
layoutGrhs mPatPart lgrhs@(L _ (GRHS guards body)) = do
  bodyDoc <- BDAddBaseY BrIndentRegular <$> layoutExpr body
  let patPart = fromMaybe BDEmpty mPatPart
  docWrapNode lgrhs <$> case guards of
    [] ->
      return $ BDCols ColEquation
        [appSep $ patPart, BDSeq [appSep $ BDLit $ Text.pack "=", bodyDoc]]
    [guard1] -> do
      guardDoc1 <- layoutGuardLStmt guard1
      return $ BDAlt
        [ BDCols ColGuardedEquation
          [ patPart
          , BDSeq [appSep $ BDLit $ Text.pack "|", appSep $ guardDoc1]
          , BDSeq [appSep $ BDLit $ Text.pack "=", bodyDoc]
          ]
        , BDAddBaseY BrIndentRegular
        $ docPar patPart
        $ BDSeq
          [ appSep $ BDLit $ Text.pack "|"
          , appSep $ guardDoc1
          , appSep $ BDSeq [BDLit $ Text.pack "="]
          , bodyDoc
          ]
        , BDAddBaseY BrIndentRegular
        $ docPar patPart
        $ BDLines
          [ BDSeq [appSep $ BDLit $ Text.pack "|", guardDoc1]
          , BDSeq [appSep $ BDLit $ Text.pack "=", bodyDoc]
          ]
        ]
    (guard1:guardr) -> do
      guardDoc1 <- layoutGuardLStmt guard1
      guardDocr <- layoutGuardLStmt `mapM` guardr
      let hat = BDCols ColGuardedEquation
            [appSep $ patPart, BDSeq [appSep $ BDLit $ Text.pack "|", guardDoc1]]
          middle = guardDocr <&> \gd -> BDCols ColGuardedEquation
            [BDEmpty, BDSeq [appSep $ BDLit $ Text.pack ",", gd]]
          last = BDCols ColGuardedEquation
            [BDEmpty, BDSeq [appSep $ BDLit $ Text.pack "=", bodyDoc]]
      return $ BDAlt
        [ BDCols ColGuardedEquation
          [ appSep $ BDForceSingleline patPart
          , BDSeq $ [appSep $ BDLit $ Text.pack "|", appSep $ BDForceSingleline guardDoc1]
                 ++ (guardDocr >>= \gd ->
                      [appSep $ BDLit $ Text.pack ",", appSep $ BDForceSingleline gd])
          , BDSeq [appSep $ BDLit $ Text.pack "=", bodyDoc]
          ]
        , BDLines $ [hat] ++ middle ++ [last]
        ]

layoutBind :: ToBriDocC (HsBindLR RdrName RdrName) (Either [BriDoc] BriDoc)
layoutBind lbind@(L _ bind) = case bind of
  FunBind fId (MG (L _ matches) _ _ _) _ _ [] -> do
    funcPatDocs <- matches `forM` \(L _ match@(Match _
                                              pats
                                              _mType -- not an actual type sig
                                              (GRHSs grhss whereBinds))) -> do
      let isInfix = isInfixMatch match
      let mId     = fId
      idStr <- lrdrNameToTextAnn mId
      patDocs <- pats `forM` layoutPat
      let funcPatternPartLine = case patDocs of
            (p1:pr) | isInfix -> BDCols ColFuncPatternsInfix
              ( [ appSep $ BDForceSingleline p1
                , appSep $ BDLit idStr
                ]
              ++ (pr <&> (\p -> appSep $ BDForceSingleline p))
              )
            ps -> BDCols ColFuncPatternsPrefix
              $ appSep (BDLit $ idStr)
              : (ps <&> (\p -> BDSeq [BDForceSingleline p, BDSeparator]))
      grhssDocsNoInd <- do
        case grhss of
          [grhs1] -> layoutGrhs (Just funcPatternPartLine) grhs1
          (grhs1:grhsr) -> do
            grhsDoc1 <- layoutGrhs (Just funcPatternPartLine) grhs1
            grhsDocr <- layoutGrhs Nothing `mapM` grhsr
            return $ BDLines $ grhsDoc1 : grhsDocr
          [] -> error "layoutBind grhssDocsNoInd"
      let grhssDocs = BDAlt [grhssDocsNoInd {-, grhssDocsInd TODO-}]
      layoutLocalBinds whereBinds >>= \case
        Nothing -> return $ grhssDocs
        Just whereDocs -> do
          return $ docPar grhssDocs 
                 $ BDEnsureIndent BrIndentRegular
                 $ BDAddBaseY BrIndentRegular
                 $ docPar (BDLit $ Text.pack "where")
                 $ BDSetIndentLevel $ BDLines whereDocs
    return $ Left $ case funcPatDocs of
      [] -> []
      [x1] -> [docWrapNode lbind x1]
      (x1:xs) | (xL:xMR) <- reverse xs ->
           [ BDAnnotationPrior (mkAnnKey lbind) $ x1 ]
        ++ reverse xMR
        ++ [ BDAnnotationPost  (mkAnnKey lbind) $ xL ]
      _ -> error "cannot happen (TM)"
  PatBind pat (GRHSs grhss whereBinds) _ _ ([], []) -> do
    patDoc <- layoutPat pat
    mWhereDocs <- layoutLocalBinds whereBinds
    grhssDocsNoInd <- do
      case grhss of
        [grhs1] -> layoutGrhs (Just $ appSep patDoc) grhs1
        (grhs1:grhsr) -> do
          grhsDoc1 <- layoutGrhs (Just $ appSep patDoc) grhs1
          grhsDocr <- layoutGrhs Nothing `mapM` grhsr
          return $ BDLines $ grhsDoc1 : grhsDocr
        [] -> error "layoutBind grhssDocsNoInd"
    let grhssDocs = BDAlt [grhssDocsNoInd {-, grhssDocsInd TODO-}]
    case mWhereDocs of
      Nothing ->
        return $ Right grhssDocs
      Just whereDocs -> do
        return $ Right
               $ BDAddBaseY BrIndentRegular
               $ docPar grhssDocs 
               $ BDAddBaseY BrIndentRegular
               $ docPar (BDLit $ Text.pack "where")
               $ BDSetIndentLevel $ BDLines whereDocs
  _ -> Right <$> briDocByExact lbind

layoutLocalBinds :: ToBriDocC (HsLocalBindsLR RdrName RdrName) (Maybe [BriDoc])
layoutLocalBinds (L _ binds) = case binds of
  HsValBinds (ValBindsIn lhsBindsLR []) ->
    Just . (>>= either id return) . Data.Foldable.toList <$> mapBagM layoutBind lhsBindsLR -- TODO: fix ordering
  x@(HsValBinds (ValBindsIn{})) ->
    Just . (:[]) <$> unknownNodeError "HsValBinds (ValBindsIn _ (_:_))" x
  x@(HsValBinds (ValBindsOut _binds _lsigs)) ->
    -- i _think_ this case never occurs in non-processed ast
    Just . (:[]) <$> unknownNodeError "HsValBinds ValBindsOut{}" x
  x@(HsIPBinds _ipBinds) ->
    Just . (:[]) <$> unknownNodeError "HsIPBinds" x
  EmptyLocalBinds     ->
    return $ Nothing

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
