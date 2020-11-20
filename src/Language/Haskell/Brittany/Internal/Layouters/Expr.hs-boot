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
#if MIN_VERSION_ghc(8,10,1)   /* ghc-8.10.1 */
import           GHC.Hs
#else
import           HsSyn
#endif
import           Name



layoutExpr :: ToBriDoc HsExpr

-- layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))

litBriDoc :: HsLit GhcPs -> BriDocFInt

overLitValBriDoc :: OverLitVal -> BriDocFInt
