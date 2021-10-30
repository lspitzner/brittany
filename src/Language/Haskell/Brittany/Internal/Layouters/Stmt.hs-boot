{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt
  ( layoutStmt
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics

import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           GHC.Hs
import           GHC.Types.Name
import qualified GHC.Data.FastString
import           GHC.Types.Basic



layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))
