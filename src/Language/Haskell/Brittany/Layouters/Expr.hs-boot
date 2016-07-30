{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Layouters.Expr
  ( layoutExpr
  , litBriDoc
  , isExpressionTypeHeadPar
  , isExpressionTypeHeadPar'
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



layoutExpr :: ToBriDoc HsExpr

-- layoutStmt :: ToBriDoc' (StmtLR RdrName RdrName (LHsExpr RdrName))

litBriDoc :: HsLit -> BriDocFInt

isExpressionTypeHeadPar :: LHsExpr RdrName -> Bool
isExpressionTypeHeadPar' :: LHsExpr RdrName -> Bool
