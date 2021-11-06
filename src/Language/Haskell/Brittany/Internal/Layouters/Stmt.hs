{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt where



import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics
import           Language.Haskell.Brittany.Internal.Config.Types

import           GHC                            ( GenLocated(L)
                                                )
import           GHC.Hs

import           Language.Haskell.Brittany.Internal.Layouters.Pattern
import           Language.Haskell.Brittany.Internal.Layouters.Decl
import {-# SOURCE #-} Language.Haskell.Brittany.Internal.Layouters.Expr



layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))
layoutStmt lstmt@(L _ stmt) = do
  indentPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
  indentAmount :: Int <-
    mAsk <&> _conf_layout .> _lconfig_indentAmount .> confUnpack
  docWrapNode lstmt $ case stmt of
    LastStmt _ body Nothing _ -> do
      layoutExpr body
    BindStmt _ lPat expr -> do
      patDoc <- fmap return $ colsWrapPat =<< layoutPat lPat
      expDoc <- docSharedWrapper layoutExpr expr
      docAlt
        [ docCols
          ColBindStmt
          [ appSep patDoc
          , docSeq
            [ appSep $ docLit $ Text.pack "<-"
            , docAddBaseY BrIndentRegular $ docForceParSpacing expDoc
            ]
          ]
        , docCols
          ColBindStmt
          [ appSep patDoc
          , docAddBaseY BrIndentRegular
            $ docPar (docLit $ Text.pack "<-") (expDoc)
          ]
        ]
    LetStmt _ binds -> do
      let isFree         = indentPolicy == IndentPolicyFree
      let indentFourPlus = indentAmount >= 4
      layoutLocalBinds binds >>= \case
        Nothing        -> docLit $ Text.pack "let"
          -- i just tested the above, and it is indeed allowed. heh.
        Just []        -> docLit $ Text.pack "let" -- this probably never happens
        Just [bindDoc] -> docAlt
          [ -- let bind = expr
            docCols
            ColDoLet
            [ appSep $ docLit $ Text.pack "let"
            , let
                f = case indentPolicy of
                  IndentPolicyFree -> docSetBaseAndIndent
                  IndentPolicyLeft -> docForceSingleline
                  IndentPolicyMultiple | indentFourPlus -> docSetBaseAndIndent
                                       | otherwise      -> docForceSingleline
              in  f $ return bindDoc
            ]
          , -- let
              --   bind = expr
            docAddBaseY BrIndentRegular $ docPar
            (docLit $ Text.pack "let")
            (docSetBaseAndIndent $ return bindDoc)
          ]
        Just bindDocs -> runFilteredAlternative $ do
          -- let aaa = expra
          --     bbb = exprb
          --     ccc = exprc
          addAlternativeCond (isFree || indentFourPlus) $ docSeq
            [ appSep $ docLit $ Text.pack "let"
            , let f = if indentFourPlus
                    then docEnsureIndent BrIndentRegular
                    else docSetBaseAndIndent
              in  f $ docLines $ return <$> bindDocs
            ]
          -- let
          --   aaa = expra
          --   bbb = exprb
          --   ccc = exprc
          addAlternativeCond (not indentFourPlus)
            $ docAddBaseY BrIndentRegular
            $ docPar (docLit $ Text.pack "let")
                     (docSetBaseAndIndent $ docLines $ return <$> bindDocs)
    RecStmt _ stmts _ _ _ _ _ -> runFilteredAlternative $ do
      -- rec stmt1
      --     stmt2
      --     stmt3
      addAlternativeCond (indentPolicy == IndentPolicyFree) $ docSeq
        [ docLit (Text.pack "rec")
        , docSeparator
        , docSetBaseAndIndent $ docLines $ layoutStmt <$> stmts
        ]
      -- rec
      --   stmt1
      --   stmt2
      --   stmt3
      addAlternative $ docAddBaseY BrIndentRegular $ docPar
        (docLit (Text.pack "rec"))
        (docLines $ layoutStmt <$> stmts)
    BodyStmt _ expr _ _ -> do
      expDoc <- docSharedWrapper layoutExpr expr
      docAddBaseY BrIndentRegular $ expDoc
    _ -> briDocByExactInlineOnly "some unknown statement" lstmt
