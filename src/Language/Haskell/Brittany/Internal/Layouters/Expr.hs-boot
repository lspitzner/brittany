{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Layouters.Expr
  ( layoutExpr
  , litBriDoc
  , overLitValBriDoc
  )
where



import Language.Haskell.Brittany.Internal.Prelude

import           Language.Haskell.Brittany.Internal.Types

import           GHC.Hs



layoutExpr :: ToBriDoc HsExpr

-- layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))

litBriDoc :: HsLit GhcPs -> BriDocFInt

overLitValBriDoc :: OverLitVal -> BriDocFInt
