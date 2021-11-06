{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt
  ( layoutStmt
  )
where



import Language.Haskell.Brittany.Internal.Prelude

import           Language.Haskell.Brittany.Internal.Types

import           GHC.Hs



layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))
