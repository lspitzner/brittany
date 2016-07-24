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
layoutStmt lstmt@(L _ stmt) = case stmt of
  LastStmt body False _ -> do
    layoutExpr body
  BindStmt lPat expr _ _ _ -> do
    patDoc <- layoutPat lPat
    expDoc <- layoutExpr expr
    return $ docWrapNode lstmt
           $ BDCols ColDoBind
      [patDoc, BDSeq [BDLit $ Text.pack " <- ", expDoc]]
  LetStmt binds -> layoutLocalBinds binds >>= \case
    Nothing ->
      return $ docWrapNode lstmt $ BDLit $ Text.pack "let" -- i just tested
                                                           -- it, and it is
                                                           -- indeed allowed.
                                                           -- heh.
    Just [] ->
      return $ docWrapNode lstmt $ BDLit $ Text.pack "let" -- this probably never happens
    Just [bindDoc] -> return $ docWrapNode lstmt $ BDAlt
      [ BDCols ColDoLet
        [ appSep $ BDLit $ Text.pack "let"
        , BDAddBaseY (BrIndentSpecial 4) bindDoc
        ]
      , BDAddBaseY BrIndentRegular $ docPar
        (BDLit $ Text.pack "let")
        bindDoc
      ]
    Just bindDocs@(bindDoc1:bindDocr) -> do
      return $ docWrapNode lstmt
             $ BDAlt
        [ BDLines
        $ (BDCols ColDoLet
            [ appSep $ BDLit $ Text.pack "let"
            , BDAddBaseY (BrIndentSpecial 4) bindDoc1
            ])
        : (bindDocr <&> \bindDoc ->
           BDCols ColDoLet
            [ appSep $ BDEmpty
            , BDAddBaseY (BrIndentSpecial 4) bindDoc
            ])
        , BDAddBaseY BrIndentRegular
        $ docPar
          (BDLit $ Text.pack "let")
          (BDLines bindDocs)
        ]
  BodyStmt expr _ _ _ -> do
    expDoc <- layoutExpr expr
    return $ docWrapNode lstmt $ BDAddBaseY BrIndentRegular $ expDoc
  _ -> briDocByExact lstmt
