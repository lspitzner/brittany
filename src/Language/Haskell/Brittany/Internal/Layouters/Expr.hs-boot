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

import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           HsSyn
import           Name



layoutExpr :: ToBriDoc HsExpr

-- layoutStmt :: ToBriDoc' (StmtLR RdrName RdrName (LHsExpr RdrName))

litBriDoc :: HsLit -> BriDocFInt

overLitValBriDoc :: OverLitVal -> BriDocFInt
