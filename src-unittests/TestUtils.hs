module TestUtils where



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

import Test.Hspec

-- import NeatInterpolation

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
    , _lconfig_allowHangingQuasiQuotes   = coerce True
    , _lconfig_experimentalSemicolonNewlines = coerce False
    -- , _lconfig_allowSinglelineRecord     = coerce False
    }
  , _conf_errorHandling             = (_conf_errorHandling staticDefaultConfig)
    { _econf_ExactPrintFallback = coerce ExactPrintFallbackModeNever
    }
  , _conf_preprocessor              = (_conf_preprocessor staticDefaultConfig)
  , _conf_forward = ForwardOptions {_options_ghc = Identity []}
  , _conf_roundtrip_exactprint_only = coerce False
  , _conf_disable_formatting = coerce False
  , _conf_obfuscate = coerce False
  }
