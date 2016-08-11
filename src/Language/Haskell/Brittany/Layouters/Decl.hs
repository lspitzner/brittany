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
import           Language.Haskell.Brittany.Config.Types

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
        [ appSep $ docWrapNodeRest lsig $ docLit nameStr
        , appSep $ docLit $ Text.pack "::"
        , docForceSingleline typeDoc
        ]
      , docAddBaseY BrIndentRegular
      $ docPar
        (docWrapNodeRest lsig $ docLit nameStr)
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
      [appSep patDoc, docSeq [appSep $ docLit $ Text.pack "<-", expDoc]]
  _ -> briDocByExact lgstmt -- TODO

layoutBind :: ToBriDocC (HsBindLR RdrName RdrName) (Either [BriDocNumbered] BriDocNumbered)
layoutBind lbind@(L _ bind) = case bind of
  FunBind fId (MG lmatches@(L _ matches) _ _ _) _ _ [] -> do
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

-- TODO: we don't need the `LHsExpr RdrName` anymore, now that there is
-- parSpacing stuff.B
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
    clauseDocs <- docWrapNodeRest lmatch $ layoutGrhs `mapM` grhss
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
  whereIndent <- mAsk
                 <&> _conf_layout
                 .>  _lconfig_indentWhereSpecial
                 .>  confUnpack
                 .>  Bool.bool BrIndentRegular (BrIndentSpecial 1)
  -- TODO: apart from this, there probably are more nodes below which could
  --       be shared between alternatives.
  wherePartMultiLine :: [ToBriDocM BriDocNumbered] <- case mWhereDocs of
        Nothing -> return $ []
        Just ws -> fmap (fmap return) $ sequence $ return @[]
          $ docEnsureIndent whereIndent
          $ docLines
            [ docLit $ Text.pack "where"
            , docEnsureIndent whereIndent
            $ docSetIndentLevel
            $ docNonBottomSpacing
            $ docLines
            $ return <$> ws
            ]
  docAlt $
    -- one-line solution
    [ docCols ColBindingLine
      [ docSeq
        (patPartInline ++ [guardPart])
      , docSeq
        [ appSep $ return binderDoc
        , docForceSingleline $ return body
        , wherePart
        ]
      ]
    | [(guards, body, _bodyRaw)] <- [clauseDocs]
    , let guardPart = case guards of
            [] -> docEmpty
            [g] -> docSeq [appSep $ docLit $ Text.pack "|", return g, docSeparator]
            gs -> docSeq
              $ [appSep $ docLit $ Text.pack "|"]
              ++ List.intersperse docCommaSep (return <$> gs)
              ++ [docSeparator]
    , wherePart <- case mWhereDocs of
          Nothing -> return @[] $ docEmpty
          Just [w] -> return @[] $ docSeq
            [ docSeparator
            , appSep $ docLit $ Text.pack "where"
            , docSetIndentLevel $ docForceSingleline $ return w
            ]
          _ -> []
    ] ++
    -- one-line solution + where in next line(s)
    [ docLines
    $ [ docCols ColBindingLine
        [ docSeq
          (patPartInline ++ [guardPart])
        , docSeq
          [ appSep $ return binderDoc
          , docForceParSpacing $ return body
          ]
        ]
      ] ++ wherePartMultiLine
    | [(guards, body, _bodyRaw)] <- [clauseDocs]
    , let guardPart = case guards of
            [] -> docEmpty
            [g] -> docSeq [appSep $ docLit $ Text.pack "|", return g, docSeparator]
            gs -> docSeq
              $ [appSep $ docLit $ Text.pack "|"]
              ++ List.intersperse docCommaSep (return <$> gs)
              ++ [docSeparator]
    , Data.Maybe.isJust mWhereDocs
    ] ++
    -- two-line solution + where in next line(s)
    [ docLines
    $ [ docForceSingleline
      $ docSeq (patPartInline ++ [guardPart, return binderDoc])
      , docEnsureIndent BrIndentRegular
      $ docForceSingleline
      $ return body
      ] ++ wherePartMultiLine
    | [(guards, body, _bodyRaw)] <- [clauseDocs]
    , let guardPart = case guards of
            [] -> docEmpty
            [g] -> docSeq [appSep $ docLit $ Text.pack "|", return g, docSeparator]
            gs -> docSeq
              $ [appSep $ docLit $ Text.pack "|"]
              ++ List.intersperse docCommaSep (return <$> gs)
              ++ [docSeparator]
    ] ++
    -- pattern and exactly one clause in single line, body as par;
    -- where in following lines
    [ docLines
    $ [ docCols ColBindingLine
        [ docSeq
          (patPartInline ++ [appSep guardPart])
        , docSeq
          [ appSep $ return binderDoc
          , docForceParSpacing $ docAddBaseY BrIndentRegular $ return body
          -- , lineMod $ docAlt
          --   [ docSetBaseY $ return body
          --   , docAddBaseY BrIndentRegular $ return body
          --   ]
          ]
        ]
      ] ++ wherePartMultiLine
    | [(guards, body, _bodyRaw)] <- [clauseDocs]
    , let guardPart = case guards of
            [] -> docEmpty
            [g] -> docSeq [appSep $ docLit $ Text.pack "|", return g]
            gs -> docSeq
              $ [appSep $ docLit $ Text.pack "|"]
              ++ List.intersperse docCommaSep (return <$> gs)
    ] ++
    -- pattern and exactly one clause in single line, body in new line.
    [ docLines
    $ [ docSeq (patPartInline ++ [appSep $ guardPart, return binderDoc])
      , docEnsureIndent BrIndentRegular
      $ docNonBottomSpacing
      $ (docAddBaseY BrIndentRegular $ return body)
      ] ++ wherePartMultiLine
    | [(guards, body, _)] <- [clauseDocs]
    , let guardPart = case guards of
            [] -> docEmpty
            [g] -> docSeq [appSep $ docLit $ Text.pack "|", return g]
            gs -> docSeq
              $ [appSep $ docLit $ Text.pack "|"]
              ++ List.intersperse docCommaSep (return <$> gs)
    ] ++
    -- conservative approach: everything starts on the left.
    [ docLines $
        [ patPartParWrap
        $ docLines
        $ fmap (docEnsureIndent BrIndentRegular)
        $ clauseDocs >>= \(guardDocs, bodyDoc, _) ->
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
        ] ++ wherePartMultiLine
    ]
