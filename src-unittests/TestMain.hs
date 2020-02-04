{-# LANGUAGE QuasiQuotes #-}

module Main where



#include "prelude.inc"

import Test.Hspec

import Language.Haskell.Brittany.Internal

import AsymptoticPerfTests



main :: IO ()
main = hspec $ tests

tests :: Spec
tests = do
  describe "asymptotic perf roundtrips" $ asymptoticPerfTest
