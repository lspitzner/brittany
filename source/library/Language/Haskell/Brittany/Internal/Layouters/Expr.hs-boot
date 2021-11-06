{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr where

import GHC.Hs
import Language.Haskell.Brittany.Internal.Types

layoutExpr :: ToBriDoc HsExpr

litBriDoc :: HsLit GhcPs -> BriDocFInt

overLitValBriDoc :: OverLitVal -> BriDocFInt
