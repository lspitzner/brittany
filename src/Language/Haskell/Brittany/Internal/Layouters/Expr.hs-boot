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

#if   MIN_VERSION_ghc(8,4,0) /* ghc-8.4 */
litBriDoc :: HsLit GhcPs -> BriDocFInt
#else /* ghc-8.2 */
litBriDoc :: HsLit -> BriDocFInt
#endif

overLitValBriDoc :: OverLitVal -> BriDocFInt
