{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Layouters.Stmt
  ( layoutStmt
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
import {-# SOURCE #-} Language.Haskell.Brittany.Layouters.Expr



layoutStmt :: ToBriDoc' (StmtLR RdrName RdrName (LHsExpr RdrName))
layoutStmt lstmt@(L _ stmt) = docWrapNode lstmt $ case stmt of
  LastStmt body False _ -> do
    layoutExpr body
  BindStmt lPat expr _ _ _ -> do
    patDoc <- docSharedWrapper layoutPat lPat
    expDoc <- docSharedWrapper layoutExpr expr
    docAlt
      [ docCols ColBindStmt
        [ appSep patDoc
        , docSeq [appSep $ docLit $ Text.pack "<-", docForceParSpacing expDoc]
        ]
      , docCols ColBindStmt
        [ appSep patDoc
        , docAddBaseY BrIndentRegular
        $ docPar (docLit $ Text.pack "<-")
                 (expDoc)
        ]
      ]
  LetStmt binds -> layoutLocalBinds binds >>= \case
    Nothing ->
      docLit $ Text.pack "let" -- i just tested
                               -- it, and it is
                               -- indeed allowed.
                               -- heh.
    Just [] ->
      docLit $ Text.pack "let" -- this probably never happens
    Just [bindDoc] -> docAlt
      [ docCols ColDoLet
        [ appSep $ docLit $ Text.pack "let"
        , docSetIndentLevel $ return bindDoc
        ]
      , docAddBaseY BrIndentRegular $ docPar
        (docLit $ Text.pack "let")
        (docSetIndentLevel $ return bindDoc)
      ]
    Just bindDocs@(bindDoc1:bindDocr) -> do
      -- TODO: the indentation here is screwed up. needs docSetIndentLevel and
      -- SetBaseY based layouting, not cols.
      docSetBaseY $ docAlt
        [ docLines
        $ (docCols ColDoLet
            [ appSep $ docLit $ Text.pack "let"
            , docSetIndentLevel $ return bindDoc1
            ])
        : (bindDocr <&> \bindDoc ->
           docCols ColDoLet
            [ docEnsureIndent (BrIndentSpecial 4) docEmpty
            , docSetIndentLevel $ return bindDoc
            ])
        , docAddBaseY BrIndentRegular
        $ docPar
          (docLit $ Text.pack "let")
          (docSetIndentLevel $ docLines $ return <$> bindDocs)
        ]
  BodyStmt expr _ _ _ -> do
    expDoc <- docSharedWrapper layoutExpr expr
    docAddBaseY BrIndentRegular $ expDoc
  _ -> briDocByExact lstmt
