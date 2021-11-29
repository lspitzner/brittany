{-# LANGUAGE NoImplicitPrelude #-}

module Brittany.Internal.Layouters.Stmt where

import GHC.Hs
import Brittany.Internal.Types



layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))
