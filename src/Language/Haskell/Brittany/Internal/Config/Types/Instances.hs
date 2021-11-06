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

module Language.Haskell.Brittany.Internal.Config.Types.Instances
where



#include "prelude.inc"

import Data.Yaml
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson

import Language.Haskell.Brittany.Internal.Config.Types

import GHC.Generics



aesonDecodeOptionsBrittany :: Aeson.Options
aesonDecodeOptionsBrittany = Aeson.defaultOptions
  { Aeson.omitNothingFields = True
  , Aeson.fieldLabelModifier = dropWhile (=='_')
  }

#define makeFromJSON(type)\
  instance FromJSON (type) where\
    parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany;\
    {-# NOINLINE parseJSON #-}
#define makeToJSON(type)\
  instance ToJSON (type) where\
    toJSON     = Aeson.genericToJSON aesonDecodeOptionsBrittany;\
    {-# NOINLINE toJSON #-};\
    toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany;\
    {-# NOINLINE toEncoding #-}

#define makeFromJSONMaybe(type)\
  instance FromJSON (type Maybe) where\
    parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany;\
    {-# NOINLINE parseJSON #-}
#define makeToJSONMaybe(type)\
  instance ToJSON (type Maybe) where\
    toJSON     = Aeson.genericToJSON aesonDecodeOptionsBrittany;\
    {-# NOINLINE toJSON #-};\
    toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany;\
    {-# NOINLINE toEncoding #-}


makeFromJSONMaybe(CDebugConfig)
makeToJSONMaybe(CDebugConfig)

makeFromJSON(IndentPolicy)
makeToJSON(IndentPolicy)
makeFromJSON(AltChooser)
makeToJSON(AltChooser)
makeFromJSON(ColumnAlignMode)
makeToJSON(ColumnAlignMode)
makeFromJSON(CPPMode)
makeToJSON(CPPMode)
makeFromJSON(ExactPrintFallbackMode)
makeToJSON(ExactPrintFallbackMode)

makeFromJSONMaybe(CLayoutConfig)
makeToJSONMaybe(CLayoutConfig)

makeFromJSONMaybe(CErrorHandlingConfig)
makeToJSONMaybe(CErrorHandlingConfig)

makeFromJSONMaybe(CForwardOptions)
makeToJSONMaybe(CForwardOptions)

makeFromJSONMaybe(CPreProcessorConfig)
makeToJSONMaybe(CPreProcessorConfig)

makeToJSONMaybe(CConfig)

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
