{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt where

import GHC.Hs
import Language.Haskell.Brittany.Internal.Types



layoutStmt :: ToBriDoc' an (StmtLR GhcPs GhcPs (LHsExpr GhcPs))
