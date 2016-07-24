{-# LANGUAGE QuasiQuotes #-}

module Main where



#include "prelude.inc"

import Test.Hspec

import NeatInterpolation

import Language.Haskell.Brittany

import IdentityTests
import AsymptoticPerfTests



main :: IO ()
main = hspec $ tests

tests :: Spec
tests = do
  describe "identity roundtrips" $ identityTests
  describe "asymptotic perf roundtrips" $ asymptoticPerfTest
