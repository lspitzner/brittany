{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.Transformations.Par
  ( transformSimplifyPar
  )
where



import Language.Haskell.Brittany.Internal.Prelude

import           Language.Haskell.Brittany.Internal.Utils
import           Language.Haskell.Brittany.Internal.Types



transformSimplifyPar :: BriDoc -> BriDoc
transformSimplifyPar = transformUp $ \case
  -- BDPar BrIndentNone line1 line2 -> Just $ BDLines [line1, line2]
  -- BDPar line indented ->
  --   Just $ BDLines [line, indented]
  -- BDPar ind1 (BDPar ind2 line p1) p2 | ind1==ind2 ->
  --   Just $ BDPar ind1 line (BDLines [p1, p2])
  x@(BDPar _ (BDPar _ BDPar{} _) _) -> x
  BDPar ind1 (BDPar ind2 line p1) (BDLines indenteds) ->
    BDPar ind1 line (BDLines (BDEnsureIndent ind2 p1 : indenteds))
  BDPar ind1 (BDPar ind2 line p1) p2 ->
    BDPar ind1 line (BDLines [BDEnsureIndent ind2 p1, p2])
  BDLines lines | any ( \case
                        BDLines{} -> True
                        BDEmpty{} -> True
                        _         -> False
                      )
                      lines  -> case go lines of
    []  -> BDEmpty
    [x] -> x
    xs  -> BDLines xs
   where
    go = (=<<) $ \case
      BDLines l -> go l
      BDEmpty   -> []
      x         -> [x]
  BDLines []                    -> BDEmpty
  BDLines [x]                   -> x
  -- BDCols sig cols | BDPar ind line indented <- List.last cols ->
  --   Just $ BDPar ind (BDCols sig (List.init cols ++ [line])) indented
  -- BDPar BrIndentNone line indented ->
  --   Just $ BDLines [line, indented]
  BDEnsureIndent BrIndentNone x -> x
  x                             -> x
