{-# LANGUAGE QuasiQuotes #-}

module TestUtils where



#include "prelude.inc"

import Test.Hspec

import NeatInterpolation

import Language.Haskell.Brittany

import Language.Haskell.Brittany.Config.Types

import System.Timeout ( timeout )



roundTripEqual :: Text -> Expectation
roundTripEqual t = fmap (fmap PPTextWrapper) (parsePrintModule defaultTestConfig "TestFakeFileName.hs" t)
    `shouldReturn` Right (PPTextWrapper t)

roundTripEqualWithTimeout :: Int -> Text -> Expectation
roundTripEqualWithTimeout time t =
  timeout time action `shouldReturn` Just (Right (PPTextWrapper t))
  where
    action = fmap (fmap PPTextWrapper)
                  (parsePrintModule defaultTestConfig "TestFakeFileName.hs" t)

newtype PPTextWrapper = PPTextWrapper Text
  deriving Eq

instance Show PPTextWrapper where
  show (PPTextWrapper t) = "\n" ++ Text.unpack t

defaultTestConfig :: Config
defaultTestConfig = Config
    { _conf_debug = _conf_debug staticDefaultConfig
    , _conf_layout = LayoutConfig
      { _lconfig_cols               = Identity 80
      , _lconfig_indentPolicy       = Identity IndentPolicyFree
      , _lconfig_indentAmount       = Identity 2
      , _lconfig_indentWhereSpecial = Identity True
      , _lconfig_indentListSpecial  = Identity True
      , _lconfig_importColumn       = Identity 60
      , _lconfig_altChooser         = Identity $ AltChooserBoundedSearch 3
      }
    , _conf_errorHandling = _conf_errorHandling staticDefaultConfig
    }
