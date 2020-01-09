{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany
  ( parsePrintModule
  , pPrintModule
  , staticDefaultConfig
  , forwardOptionsSyntaxExtsEnabled
  , userConfigPath
  , findLocalConfigPath
  , readConfigs
  , readConfigsWithUserConfig
  , Config
  , CConfig(..)
  , CDebugConfig(..)
  , CLayoutConfig(..)
  , CErrorHandlingConfig(..)
  , CForwardOptions(..)
  , CPreProcessorConfig(..)
  , BrittanyError(..)
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Internal
import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Config

