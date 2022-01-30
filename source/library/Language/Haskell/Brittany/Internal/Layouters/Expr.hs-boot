{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr where

import GHC.Hs
import Language.Haskell.Brittany.Internal.Types



layoutExpr :: ToBriDoc an HsExpr

-- layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))

litBriDoc :: HsLit GhcPs -> BriDocFInt

overLitValBriDoc :: OverLitVal -> BriDocFInt
