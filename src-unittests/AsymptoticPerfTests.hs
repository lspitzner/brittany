{-# LANGUAGE ScopedTypeVariables #-}

module AsymptoticPerfTests where



import Language.Haskell.Brittany.Internal.PreludeUtils
import qualified Data.Text as Text

import Test.Hspec

import TestUtils



asymptoticPerfTest :: Spec
asymptoticPerfTest = do
  it "10 do statements"
    $  roundTripEqualWithTimeout 1500000
    $  (Text.pack "func = do\n")
    <> Text.replicate 10 (Text.pack "  statement\n")
  it "10 do nestings"
    $  roundTripEqualWithTimeout 4000000
    $  (Text.pack "func = ")
    <> mconcat
         (   [1 .. 10]
         <&> \(i :: Int) ->
               (Text.replicate (2 * i) (Text.pack " ") <> Text.pack "do\n")
         )
    <> Text.replicate 2000 (Text.pack " ")
    <> Text.pack "return\n"
    <> Text.replicate 2002 (Text.pack " ")
    <> Text.pack "()"
  it "10 AppOps"
    $  roundTripEqualWithTimeout 1000000
    $  (Text.pack "func = expr")
    <> Text.replicate 10 (Text.pack "\n     . expr") --TODO
