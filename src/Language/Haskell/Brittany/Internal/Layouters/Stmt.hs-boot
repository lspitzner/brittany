{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt
  ( layoutStmt
  )
where



import Language.Haskell.Brittany.Internal.Prelude

import           Language.Haskell.Brittany.Internal.Types

import           GHC.Hs



layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))
