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



import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import qualified Control.Monad.Reader.Class as Reader.Class
import qualified Control.Monad.RWS.Class as RWS.Class
import qualified Control.Monad.State.Class as State.Class
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.MultiRWS.Lazy as MultiRWSL
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.State.Lazy as StateL
import qualified Control.Monad.Trans.State.Strict as StateS
import qualified Control.Monad.Writer.Class as Writer.Class
import qualified Data.Bool as Bool
import qualified Data.ByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Coerce
import qualified Data.Data
import qualified Data.Either
import qualified Data.Foldable
import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Lazy as IntMapL
import qualified Data.IntMap.Strict as IntMapS
import qualified Data.List.Extra
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL.Encoding
import qualified Data.Text.Lazy.IO as TextL.IO
import qualified GHC.OldList as List
import qualified Safe as Safe
import qualified System.Directory
import qualified System.IO
import qualified Text.PrettyPrint
import qualified Text.PrettyPrint.Annotated
import qualified Text.PrettyPrint.Annotated.HughesPJ
import qualified Text.PrettyPrint.Annotated.HughesPJClass

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
