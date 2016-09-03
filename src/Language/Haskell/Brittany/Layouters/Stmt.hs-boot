{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Layouters.Stmt
  ( layoutStmt
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Types
import           Language.Haskell.Brittany.LayouterBasics

import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           SrcLoc ( SrcSpan )
import           HsSyn
import           Name
import qualified FastString
import           BasicTypes



layoutStmt :: ToBriDoc' (StmtLR RdrName RdrName (LHsExpr RdrName))
