{-# LANGUAGE QuasiQuotes #-}

module TestUtils where



#include "prelude.inc"

import Test.Hspec

import NeatInterpolation

import Language.Haskell.Brittany.Internal

import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Config

import System.Timeout ( timeout )

import Data.Coerce ( coerce )



roundTripEqual :: Text -> Expectation
roundTripEqual t =
  fmap (fmap PPTextWrapper)
       (parsePrintModuleTests defaultTestConfig "TestFakeFileName.hs" t)
    `shouldReturn` Right (PPTextWrapper t)

roundTripEqualWithTimeout :: Int -> Text -> Expectation
roundTripEqualWithTimeout time t =
  timeout time (action >>= evaluate) >>= (`shouldSatisfy`Data.Maybe.isJust)
 where
  action = fmap (fmap PPTextWrapper)
                (parsePrintModuleTests defaultTestConfig "TestFakeFileName.hs" t)

newtype PPTextWrapper = PPTextWrapper Text
  deriving Eq

instance Show PPTextWrapper where
  show (PPTextWrapper t) = "\n" ++ Text.unpack t

defaultTestConfig :: Config
defaultTestConfig = Config
  { _conf_version                   = _conf_version staticDefaultConfig
  , _conf_debug                     = _conf_debug staticDefaultConfig
  , _conf_layout                    = LayoutConfig
    { _lconfig_cols                      = coerce (80 :: Int)
    , _lconfig_indentPolicy              = coerce IndentPolicyFree
    , _lconfig_indentAmount              = coerce (2 :: Int)
    , _lconfig_indentWhereSpecial        = coerce True
    , _lconfig_indentListSpecial         = coerce True
    , _lconfig_importColumn              = coerce (60 :: Int)
    , _lconfig_importAsColumn            = coerce (60 :: Int)
    , _lconfig_altChooser                = coerce $ AltChooserBoundedSearch 3
    , _lconfig_columnAlignMode           = coerce (ColumnAlignModeMajority 0.7)
    , _lconfig_alignmentLimit            = coerce (30 :: Int)
    , _lconfig_alignmentBreakOnMultiline = coerce True
    , _lconfig_hangingTypeSignature      = coerce False
    , _lconfig_reformatModulePreamble    = coerce True
    , _lconfig_allowSingleLineExportList = coerce True
    }
  , _conf_errorHandling             = (_conf_errorHandling staticDefaultConfig)
    { _econf_ExactPrintFallback = coerce ExactPrintFallbackModeNever
    }
  , _conf_preprocessor              = (_conf_preprocessor staticDefaultConfig)
  , _conf_forward = ForwardOptions {_options_ghc = Identity []}
  , _conf_roundtrip_exactprint_only = coerce False
  }
