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

import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           GHC.Hs
import           GHC.Types.Name



layoutExpr :: ToBriDoc HsExpr

-- layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))

litBriDoc :: HsLit GhcPs -> BriDocFInt

overLitValBriDoc :: OverLitVal -> BriDocFInt
