{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt
  ( layoutStmt
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
import qualified FastString
import           BasicTypes



layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))
