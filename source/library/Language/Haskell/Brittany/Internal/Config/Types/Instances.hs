{-# OPTIONS_GHC -fno-warn-orphans #-}
-- These optimizations are disabled to improve compile times (and compilation
-- memory usage). When we do not disable them, the CI servers take more than
-- 10 minutes to compile this module alone.
-- Having looked into aeson and how the instances are written, I still do
-- not understand what makes GHC choke so much here. The size of the raw
-- expressions below looks fairly negligible, so there must be some expansion
-- due to inlining going on. But even disabling INLINE pragmas in aeson did
-- not seem to change anything.
-- Nonetheless, this solution works and has no downsides because the
-- instances defined here are not in any way performance-critical.
{-# OPTIONS_GHC -fno-pre-inlining #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fignore-interface-pragmas #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Haskell.Brittany.Internal.Config.Types.Instances where

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson
import Data.Yaml
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Prelude



aesonDecodeOptionsBrittany :: Aeson.Options
aesonDecodeOptionsBrittany = Aeson.defaultOptions
  { Aeson.omitNothingFields = True
  , Aeson.fieldLabelModifier = dropWhile (=='_')
  }

instance FromJSON (CDebugConfig Maybe) where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON (CDebugConfig Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

instance FromJSON IndentPolicy where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON IndentPolicy where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany
  toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

instance FromJSON AltChooser where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON AltChooser where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany
  toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

instance FromJSON ColumnAlignMode where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON ColumnAlignMode where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany
  toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

instance FromJSON CPPMode where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON CPPMode where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany
  toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

instance FromJSON ExactPrintFallbackMode where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON ExactPrintFallbackMode where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany
  toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

instance FromJSON (CLayoutConfig Maybe) where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON (CLayoutConfig Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

instance FromJSON (CErrorHandlingConfig Maybe) where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON (CErrorHandlingConfig Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

instance FromJSON (CForwardOptions Maybe) where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON (CForwardOptions Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

instance FromJSON (CPreProcessorConfig Maybe) where
  parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany

instance ToJSON (CPreProcessorConfig Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

instance ToJSON (CConfig Maybe) where
  toJSON = Aeson.genericToJSON aesonDecodeOptionsBrittany

-- This custom instance ensures the "omitNothingFields" behaviour not only for
-- leafs, but for nodes of the config as well. This way e.g. "{}" is valid
-- config file content.
instance FromJSON (CConfig Maybe) where
  parseJSON (Object v) = Config
    <$> v .:?  Key.fromString "conf_version"
    <*> v .:?= Key.fromString "conf_debug"
    <*> v .:?= Key.fromString "conf_layout"
    <*> v .:?= Key.fromString "conf_errorHandling"
    <*> v .:?= Key.fromString "conf_forward"
    <*> v .:?= Key.fromString "conf_preprocessor"
    <*> v .:?  Key.fromString "conf_roundtrip_exactprint_only"
    <*> v .:?  Key.fromString "conf_disable_formatting"
    <*> v .:?  Key.fromString "conf_obfuscate"
  parseJSON invalid    = Aeson.typeMismatch "Config" invalid

-- Pretends that the value is {} when the key is not present.
(.:?=) :: FromJSON a => Object -> Key.Key -> Parser a
o .:?= k = o .:? k >>= maybe (parseJSON (Aeson.object [])) pure
