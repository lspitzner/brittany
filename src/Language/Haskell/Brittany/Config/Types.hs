{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Haskell.Brittany.Config.Types
where



#include "prelude.inc"

import Data.Yaml
import GHC.Generics
import Control.Lens

import Data.Data ( Data )



data DebugConfigF f = DebugConfig
  { _dconf_dump_config                :: f Bool
  , _dconf_dump_annotations           :: f Bool
  , _dconf_dump_ast_unknown           :: f Bool
  , _dconf_dump_ast_full              :: f Bool
  , _dconf_dump_bridoc_raw            :: f Bool
  , _dconf_dump_bridoc_simpl_alt      :: f Bool
  , _dconf_dump_bridoc_simpl_floating :: f Bool
  , _dconf_dump_bridoc_simpl_par      :: f Bool
  , _dconf_dump_bridoc_simpl_columns  :: f Bool
  , _dconf_dump_bridoc_simpl_indent   :: f Bool
  , _dconf_dump_bridoc_final          :: f Bool
  }
  deriving (Generic)

data LayoutConfigF f = LayoutConfig
  { _lconfig_cols :: f Int -- the thing that has default 80.
  , _lconfig_indentPolicy :: f IndentPolicy
  , _lconfig_indentAmount :: f Int
  , _lconfig_indentWhereSpecial :: f Bool -- indent where only 1 sometimes (TODO).
  , _lconfig_indentListSpecial  :: f Bool -- use some special indentation for ","
                                          -- when creating zero-indentation
                                          -- multi-line list literals.
  , _lconfig_importColumn :: f Int
  , _lconfig_altChooser :: f AltChooser
  }
  deriving (Generic)

data ErrorHandlingConfigF f = ErrorHandlingConfig
  { _econf_produceOutputOnErrors :: f Bool
  , _econf_Werror :: f Bool
  }
  deriving (Generic)

data ConfigF f = Config
  { _conf_debug :: DebugConfigF f
  , _conf_layout :: LayoutConfigF f
  , _conf_errorHandling :: ErrorHandlingConfigF f
  }
  deriving (Generic)

-- i wonder if any Show1 stuff could be leveraged.
deriving instance Show (DebugConfigF Identity)
deriving instance Show (LayoutConfigF Identity)
deriving instance Show (ErrorHandlingConfigF Identity)
deriving instance Show (ConfigF Identity)

deriving instance Show (DebugConfigF Maybe)
deriving instance Show (LayoutConfigF Maybe)
deriving instance Show (ErrorHandlingConfigF Maybe)
deriving instance Show (ConfigF Maybe)

deriving instance Data (DebugConfigF Identity)
deriving instance Data (LayoutConfigF Identity)
deriving instance Data (ErrorHandlingConfigF Identity)
deriving instance Data (ConfigF Identity)

type Config = ConfigF Identity
type DebugConfig = DebugConfigF Identity
type LayoutConfig = LayoutConfigF Identity
type ErrorHandlingConfig = ErrorHandlingConfigF Identity

instance FromJSON (DebugConfigF Maybe)
instance ToJSON   (DebugConfigF Maybe)

instance FromJSON IndentPolicy
instance ToJSON   IndentPolicy
instance FromJSON AltChooser
instance ToJSON   AltChooser

instance FromJSON (LayoutConfigF Maybe)
instance ToJSON   (LayoutConfigF Maybe)

instance FromJSON (ErrorHandlingConfigF Maybe)
instance ToJSON   (ErrorHandlingConfigF Maybe)

instance FromJSON (ConfigF Maybe)
instance ToJSON   (ConfigF Maybe)

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

staticDefaultConfig :: Config
staticDefaultConfig = Config
    { _conf_debug = DebugConfig
      { _dconf_dump_config           = Identity False
      , _dconf_dump_annotations      = Identity False
      , _dconf_dump_ast_unknown      = Identity False
      , _dconf_dump_ast_full         = Identity False
      , _dconf_dump_bridoc_raw            = Identity False
      , _dconf_dump_bridoc_simpl_alt      = Identity False
      , _dconf_dump_bridoc_simpl_floating = Identity False
      , _dconf_dump_bridoc_simpl_par      = Identity False
      , _dconf_dump_bridoc_simpl_columns  = Identity False
      , _dconf_dump_bridoc_simpl_indent   = Identity False
      , _dconf_dump_bridoc_final          = Identity False
      }
    , _conf_layout = LayoutConfig
      { _lconfig_cols               = Identity 80
      , _lconfig_indentPolicy       = Identity IndentPolicyFree
      , _lconfig_indentAmount       = Identity 2
      , _lconfig_indentWhereSpecial = Identity True
      , _lconfig_indentListSpecial  = Identity True
      , _lconfig_importColumn       = Identity 60
      , _lconfig_altChooser         = Identity $ AltChooserBoundedSearch 3
      }
    , _conf_errorHandling = ErrorHandlingConfig
      { _econf_produceOutputOnErrors = Identity False
      , _econf_Werror                = Identity False
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
  cZip f (LayoutConfig x1 x2 x3 x4 x5 x6 x7)
         (LayoutConfig y1 y2 y3 y4 y5 y6 y7) = LayoutConfig
    (f x1 y1)
    (f x2 y2)
    (f x3 y3)
    (f x4 y4)
    (f x5 y5)
    (f x6 y6)
    (f x7 y7)

instance CZip ErrorHandlingConfigF where
  cZip f (ErrorHandlingConfig x1 x2)
         (ErrorHandlingConfig y1 y2) = ErrorHandlingConfig
    (f x1 y1)
    (f x2 y2)

instance CZip ConfigF where
  cZip f (Config x1 x2 x3) (Config y1 y2 y3) = Config
    (cZip f x1 y1)
    (cZip f x2 y2)
    (cZip f x3 y3)

cMap :: CZip k => (forall a . f a -> g a) -> k f -> k g
cMap f c = cZip (\_ -> f) c c

makeLenses ''DebugConfigF
makeLenses ''ConfigF
makeLenses ''LayoutConfigF
