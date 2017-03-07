{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Haskell.Brittany.Config.Types
where



#include "prelude.inc"

import Data.Yaml
import qualified Data.Aeson.Types as Aeson
import GHC.Generics
import Control.Lens

import Data.Data ( Data )

import Data.Coerce ( Coercible, coerce )

import Data.Semigroup.Generic

import Data.Semigroup ( Last, Option )



confUnpack :: Coercible a b => Identity a -> b
confUnpack (Identity x) = coerce x

data DebugConfigF f = DebugConfig
  { _dconf_dump_config                :: f (Semigroup.Last Bool)
  , _dconf_dump_annotations           :: f (Semigroup.Last Bool)
  , _dconf_dump_ast_unknown           :: f (Semigroup.Last Bool)
  , _dconf_dump_ast_full              :: f (Semigroup.Last Bool)
  , _dconf_dump_bridoc_raw            :: f (Semigroup.Last Bool)
  , _dconf_dump_bridoc_simpl_alt      :: f (Semigroup.Last Bool)
  , _dconf_dump_bridoc_simpl_floating :: f (Semigroup.Last Bool)
  , _dconf_dump_bridoc_simpl_par      :: f (Semigroup.Last Bool)
  , _dconf_dump_bridoc_simpl_columns  :: f (Semigroup.Last Bool)
  , _dconf_dump_bridoc_simpl_indent   :: f (Semigroup.Last Bool)
  , _dconf_dump_bridoc_final          :: f (Semigroup.Last Bool)
  }
  deriving (Generic)

data LayoutConfigF f = LayoutConfig
  { _lconfig_cols         :: f (Last Int) -- the thing that has default 80.
  , _lconfig_indentPolicy :: f (Last IndentPolicy)
  , _lconfig_indentAmount :: f (Last Int)
  , _lconfig_indentWhereSpecial :: f (Last Bool) -- indent where only 1 sometimes (TODO).
  , _lconfig_indentListSpecial  :: f (Last Bool) -- use some special indentation for ","
                                                 -- when creating zero-indentation
                                                 -- multi-line list literals.
  , _lconfig_importColumn :: f (Last Int)
  , _lconfig_altChooser      :: f (Last AltChooser)
  , _lconfig_columnAlignMode :: f (Last ColumnAlignMode)
  }
  deriving (Generic)

data ForwardOptionsF f = ForwardOptions
  { _options_ghc :: f [String]
  }
  deriving (Generic)

data ErrorHandlingConfigF f = ErrorHandlingConfig
  { _econf_produceOutputOnErrors   :: f (Semigroup.Last Bool)
  , _econf_Werror                  :: f (Semigroup.Last Bool)
  , _econf_CPPMode                 :: f (Semigroup.Last CPPMode)
  , _econf_ExactPrintFallback      :: f (Semigroup.Last ExactPrintFallbackMode)
    -- ^ Determines when to fall back on the exactprint'ed output when
    -- syntactical constructs are encountered which are not yet handled by
    -- brittany.
    -- Note that the "risky" setting is risky because even with the check of
    -- the syntactic validity of the brittany output, at least in theory there
    -- may be cases where the output is syntactically/semantically valid but
    -- has different semantics that the code pre-transformation.
  }
  deriving (Generic)

data ConfigF f = Config
  { _conf_debug :: DebugConfigF f
  , _conf_layout :: LayoutConfigF f
  , _conf_errorHandling :: ErrorHandlingConfigF f
  , _conf_forward :: ForwardOptionsF f
  }
  deriving (Generic)

data ErrorHandlingConfigFMaybe = ErrorHandlingConfigMaybe
  { _econfm_produceOutputOnErrors :: Maybe (Semigroup.Last Bool)
  , _econfm_Werror                :: Maybe (Semigroup.Last Bool)
  , _econfm_CPPMode               :: Maybe (Semigroup.Last CPPMode)
  }
  deriving (Generic)

-- i wonder if any Show1 stuff could be leveraged.
deriving instance Show (DebugConfigF Identity)
deriving instance Show (LayoutConfigF Identity)
deriving instance Show (ErrorHandlingConfigF Identity)
deriving instance Show (ForwardOptionsF Identity)
deriving instance Show (ConfigF Identity)

deriving instance Show (DebugConfigF Option)
deriving instance Show (LayoutConfigF Option)
deriving instance Show (ErrorHandlingConfigF Option)
deriving instance Show (ForwardOptionsF Option)
deriving instance Show (ConfigF Option)

deriving instance Data (DebugConfigF Identity)
deriving instance Data (LayoutConfigF Identity)
deriving instance Data (ErrorHandlingConfigF Identity)
deriving instance Data (ForwardOptionsF Identity)
deriving instance Data (ConfigF Identity)

instance Semigroup.Semigroup (DebugConfigF Option) where
  (<>) = gmappend
instance Semigroup.Semigroup (LayoutConfigF Option) where
  (<>) = gmappend
instance Semigroup.Semigroup (ErrorHandlingConfigF Option) where
  (<>) = gmappend
instance Semigroup.Semigroup (ForwardOptionsF Option) where
  (<>) = gmappend
instance Semigroup.Semigroup (ConfigF Option) where
  (<>) = gmappend

type Config = ConfigF Identity
type DebugConfig = DebugConfigF Identity
type LayoutConfig = LayoutConfigF Identity
type ErrorHandlingConfig = ErrorHandlingConfigF Identity

aesonDecodeOptionsBrittany :: Aeson.Options
aesonDecodeOptionsBrittany = Aeson.defaultOptions
  { Aeson.omitNothingFields = True
  , Aeson.fieldLabelModifier = dropWhile (=='_')
  }

-- instance FromJSON a => FromJSON (Semigroup.Last a) where
--   parseJSON obj = Semigroup.Last <$> parseJSON obj
--   {-# INLINE parseJSON #-}
-- instance ToJSON a => ToJSON (Semigroup.Last a) where
--   toJSON (Semigroup.Last x) = toJSON x
--   {-# INLINE toJSON #-}
--
-- instance FromJSON a => FromJSON (Option a) where
--   parseJSON obj = Option <$> parseJSON obj
--   {-# INLINE parseJSON #-}
-- instance ToJSON a => ToJSON (Option a) where
--   toJSON (Option x) = toJSON x
--   {-# INLINE toJSON #-}

#define makeFromJSON(type)\
  instance FromJSON (type) where\
    parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany
#define makeToJSON(type)\
  instance ToJSON (type) where\
    toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

#define makeFromJSONMaybe(type)\
  instance FromJSON (type Maybe) where\
    parseJSON = Aeson.genericParseJSON aesonDecodeOptionsBrittany
#define makeFromJSONOption(type)\
  instance FromJSON (type Option) where\
    parseJSON = fmap (cMap Option) . parseJSON
#define makeToJSONMaybe(type)\
  instance ToJSON (type Maybe) where\
    toJSON     = Aeson.genericToJSON aesonDecodeOptionsBrittany;\
    toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany
#define makeToJSONOption(type)\
  instance ToJSON (type Option) where\
    toJSON     = toJSON     . cMap getOption;\
    toEncoding = toEncoding . cMap getOption

makeFromJSON(ErrorHandlingConfigFMaybe)
makeToJSON  (ErrorHandlingConfigFMaybe)
deriving instance Show (ErrorHandlingConfigFMaybe)


makeFromJSONOption (DebugConfigF)
makeFromJSONMaybe  (DebugConfigF)
makeToJSONOption   (DebugConfigF)
makeToJSONMaybe    (DebugConfigF)
-- instance FromJSON (DebugConfigF Option) where
--   parseJSON = genericParseJSON aesonDecodeOptionsBrittany
-- instance ToJSON   (DebugConfigF Option) where
--   toEncoding = Aeson.genericToEncoding aesonDecodeOptionsBrittany

makeFromJSON (IndentPolicy)
makeToJSON   (IndentPolicy)
makeFromJSON (AltChooser)
makeToJSON   (AltChooser)
makeFromJSON (ColumnAlignMode)
makeToJSON   (ColumnAlignMode)
makeFromJSON (CPPMode)
makeToJSON   (CPPMode)
makeFromJSON (ExactPrintFallbackMode)
makeToJSON   (ExactPrintFallbackMode)

makeFromJSONOption (LayoutConfigF)
makeFromJSONMaybe  (LayoutConfigF)
makeToJSONOption   (LayoutConfigF)
makeToJSONMaybe    (LayoutConfigF)

makeFromJSONOption (ErrorHandlingConfigF)
makeFromJSONMaybe  (ErrorHandlingConfigF)
makeToJSONOption   (ErrorHandlingConfigF)
makeToJSONMaybe    (ErrorHandlingConfigF)

makeFromJSONOption (ForwardOptionsF)
makeFromJSONMaybe  (ForwardOptionsF)
makeToJSONOption   (ForwardOptionsF)
makeToJSONMaybe    (ForwardOptionsF)

makeFromJSONOption (ConfigF)
makeFromJSONMaybe  (ConfigF)
makeToJSONOption   (ConfigF)
makeToJSONMaybe    (ConfigF)

-- instance Monoid DebugConfig where
--   mempty = DebugConfig Nothing Nothing
--   DebugConfig x1 x2 `mappend` DebugConfig y1 y2
--     = DebugConfig (y1 <|> x1)
--                   (y2 <|> x2)
-- 
-- instance Monoid LayoutConfig where
--   mempty = LayoutConfig Nothing Nothing Nothing Nothing Nothing Nothing
--   LayoutConfig x1 x2 x3 x4 x5 x6 `mappend` LayoutConfig y1 y2 y3 y4 y5 y6
--     = LayoutConfig (y1 <|> x1)
--                    (y2 <|> x2)
--                    (y3 <|> x3)
--                    (y4 <|> x4)
--                    (y5 <|> x5)
--                    (y6 <|> x6)
-- 
-- instance Monoid Config where
--   mempty = Config
--     { _conf_debug = mempty
--     , _conf_layout = mempty
--     }
--   mappend c1 c2 = Config
--     { _conf_debug = _conf_debug c1 <> _conf_debug c2
--     , _conf_layout = _conf_layout c1 <> _conf_layout c2
--     }

data IndentPolicy = IndentPolicyLeft -- never create a new indentation at more
                                     -- than old indentation + amount
                  | IndentPolicyFree -- can create new indentations whereever
                  | IndentPolicyMultiple -- can create indentations only
                                         -- at any n * amount.
  deriving (Show, Generic, Data)

data AltChooser = AltChooserSimpleQuick -- always choose last alternative.
                                        -- leads to tons of sparsely filled
                                        -- lines.
                | AltChooserShallowBest -- choose the first matching alternative
                                        -- using the simplest spacing
                                        -- information for the children.
                | AltChooserBoundedSearch Int
                                        -- choose the first matching alternative
                                        -- using a bounded list of recursive
                                        -- options having sufficient space.
  deriving (Show, Generic, Data)

data ColumnAlignMode
  = ColumnAlignModeDisabled
    -- ^ Make no column alignments whatsoever
  | ColumnAlignModeUnanimously
    -- ^ Make column alignments only if it does not cause overflow for any of
    -- the affected lines.
  | ColumnAlignModeMajority Float
    -- ^ If at least (ratio::Float) of the aligned elements have sufficient
    -- space for the alignment, act like ColumnAlignModeAnimously; otherwise
    -- act like ColumnAlignModeDisabled.
  | ColumnAlignModeAnimouslyScale Int
    -- ^ Scale back columns to some degree if their sum leads to overflow.
    -- This is done in a linear fashion.
    -- The Int specifies additional columns to be added to column maximum for
    -- scaling calculation purposes.
  | ColumnAlignModeAnimously
    -- ^ Decide on a case-by-case basis if alignment would cause overflow.
    -- If it does, cancel all alignments for this (nested) column description.
  -- ColumnAlignModeAnimouslySome -- potentially to implement
  | ColumnAlignModeAlways
    -- ^ Always respect column alignments, even if it makes stuff overflow.
  deriving (Show, Generic, Data)

data CPPMode = CPPModeAbort  -- abort program on seeing -XCPP
             | CPPModeWarn   -- warn about CPP and non-roundtripping in its
                             -- presence.
             | CPPModeNowarn -- silently allow CPP, if possible (i.e. input is
                             -- file.)
  deriving (Show, Generic, Data)

data ExactPrintFallbackMode
  = ExactPrintFallbackModeNever  -- never fall back on exactprinting
  | ExactPrintFallbackModeInline -- fall back only if there are no newlines in
                                 -- the exactprint'ed output.
  | ExactPrintFallbackModeRisky  -- fall back even in the presence of newlines.
                                 -- THIS MAY THEORETICALLY CHANGE SEMANTICS OF
                                 -- A PROGRAM BY TRANSFORMING IT.
  deriving (Show, Generic, Data)

staticDefaultConfig :: Config
staticDefaultConfig = Config
  { _conf_debug         = DebugConfig
    { _dconf_dump_config                = coerce False
    , _dconf_dump_annotations           = coerce False
    , _dconf_dump_ast_unknown           = coerce False
    , _dconf_dump_ast_full              = coerce False
    , _dconf_dump_bridoc_raw            = coerce False
    , _dconf_dump_bridoc_simpl_alt      = coerce False
    , _dconf_dump_bridoc_simpl_floating = coerce False
    , _dconf_dump_bridoc_simpl_par      = coerce False
    , _dconf_dump_bridoc_simpl_columns  = coerce False
    , _dconf_dump_bridoc_simpl_indent   = coerce False
    , _dconf_dump_bridoc_final          = coerce False
    }
  , _conf_layout        = LayoutConfig
    { _lconfig_cols               = coerce (80 :: Int)
    , _lconfig_indentPolicy       = coerce IndentPolicyFree
    , _lconfig_indentAmount       = coerce (2 :: Int)
    , _lconfig_indentWhereSpecial = coerce True
    , _lconfig_indentListSpecial  = coerce True
    , _lconfig_importColumn       = coerce (60 :: Int)
    , _lconfig_altChooser         = coerce (AltChooserBoundedSearch 3)
    , _lconfig_columnAlignMode    = coerce (ColumnAlignModeMajority 0.7)
    }
  , _conf_errorHandling = ErrorHandlingConfig
    { _econf_produceOutputOnErrors   = coerce False
    , _econf_Werror                  = coerce False
    , _econf_CPPMode                 = coerce CPPModeAbort
    , _econf_ExactPrintFallback      = coerce ExactPrintFallbackModeInline
    }
  , _conf_forward       = ForwardOptions
    { _options_ghc = Identity []
    }
  }

-- TODO: automate writing instances for this to get
--       the above Monoid instance for free.
-- potentially look at http://hackage.haskell.org/package/fieldwise-0.1.0.0/docs/src/Data-Fieldwise.html#deriveFieldwise
class CZip k where
  cZip :: (forall a . f a -> g a -> h a) -> k f -> k g -> k h

instance CZip DebugConfigF where
  cZip f (DebugConfig x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)
         (DebugConfig y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) = DebugConfig
    (f x1 y1)
    (f x2 y2)
    (f x3 y3)
    (f x4 y4)
    (f x5 y5)
    (f x6 y6)
    (f x7 y7)
    (f x8 y8)
    (f x9 y9)
    (f x10 y10)
    (f x11 y11)

instance CZip LayoutConfigF where
  cZip f (LayoutConfig x1 x2 x3 x4 x5 x6 x7 x8)
         (LayoutConfig y1 y2 y3 y4 y5 y6 y7 y8) = LayoutConfig
    (f x1 y1)
    (f x2 y2)
    (f x3 y3)
    (f x4 y4)
    (f x5 y5)
    (f x6 y6)
    (f x7 y7)
    (f x8 y8)

instance CZip ErrorHandlingConfigF where
  cZip f (ErrorHandlingConfig x1 x2 x3 x4)
         (ErrorHandlingConfig y1 y2 y3 y4) = ErrorHandlingConfig
    (f x1 y1)
    (f x2 y2)
    (f x3 y3)
    (f x4 y4)

instance CZip ForwardOptionsF where
  cZip f (ForwardOptions x1)
         (ForwardOptions y1) = ForwardOptions
    (f x1 y1)

instance CZip ConfigF where
  cZip f (Config x1 x2 x3 x4) (Config y1 y2 y3 y4) = Config
    (cZip f x1 y1)
    (cZip f x2 y2)
    (cZip f x3 y3)
    (cZip f x4 y4)

cMap :: CZip k => (forall a . f a -> g a) -> k f -> k g
cMap f c = cZip (\_ -> f) c c

makeLenses ''DebugConfigF
makeLenses ''ConfigF
makeLenses ''LayoutConfigF
