{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.Brittany.Internal.Config.Types.Instances
where



#include "prelude.inc"

import Data.Yaml
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
#define makeFromJSONOption(type)\
  instance FromJSON (type Option) where\
    parseJSON = fmap (cMap Option) . parseJSON;\
    {-# NOINLINE parseJSON #-}
#define makeToJSONMaybe(type)\
  instance ToJSON (type Maybe) where\
    toJSON     = Aeson.genericToJSON aesonDecodeOptionsBrittany;\
    {-# NOINLINE toJSON #-};\
    toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany;\
    {-# NOINLINE toEncoding #-}
#define makeToJSONOption(type)\
  instance ToJSON (type Option) where\
    toJSON     = toJSON     . cMap getOption;\
    {-# NOINLINE toJSON #-};\
    toEncoding = toEncoding . cMap getOption;\
    {-# NOINLINE toEncoding #-}


makeFromJSONOption(CDebugConfig)
makeFromJSONMaybe(CDebugConfig)
makeToJSONOption(CDebugConfig)
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

makeFromJSONOption(CLayoutConfig)
makeFromJSONMaybe(CLayoutConfig)
makeToJSONOption(CLayoutConfig)
makeToJSONMaybe(CLayoutConfig)

makeFromJSONOption(CErrorHandlingConfig)
makeFromJSONMaybe(CErrorHandlingConfig)
makeToJSONOption(CErrorHandlingConfig)
makeToJSONMaybe(CErrorHandlingConfig)

makeFromJSONOption(CForwardOptions)
makeFromJSONMaybe(CForwardOptions)
makeToJSONOption(CForwardOptions)
makeToJSONMaybe(CForwardOptions)

makeFromJSONOption(CPreProcessorConfig)
makeFromJSONMaybe(CPreProcessorConfig)
makeToJSONOption(CPreProcessorConfig)
makeToJSONMaybe(CPreProcessorConfig)

makeFromJSONOption(CConfig)
makeToJSONOption(CConfig)
makeToJSONMaybe(CConfig)

-- This custom instance ensures the "omitNothingFields" behaviour not only for
-- leafs, but for nodes of the config as well. This way e.g. "{}" is valid
-- config file content.
instance FromJSON (CConfig Maybe) where
  parseJSON (Object v) = Config
    <$> v .:?  Text.pack "conf_version"
    <*> v .:?= Text.pack "conf_debug"
    <*> v .:?= Text.pack "conf_layout"
    <*> v .:?= Text.pack "conf_errorHandling"
    <*> v .:?= Text.pack "conf_forward"
    <*> v .:?= Text.pack "conf_preprocessor"
  parseJSON invalid    = Aeson.typeMismatch "Config" invalid

-- Pretends that the value is {} when the key is not present.
(.:?=) :: FromJSON a => Object -> Text -> Parser a
o .:?= k = o .:? k >>= maybe (parseJSON (Aeson.object [])) pure

