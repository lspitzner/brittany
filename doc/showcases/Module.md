
Last updated for brittany version `0.10.0.0`.

# Example layouting of the module header (exports/imports)

## On default settings

default settings are:

~~~~
conf_layout:
  lconfig_indentPolicy: IndentPolicyFree
  lconfig_importColumn: 50
  lconfig_importAsColumn: 50
~~~~


~~~~.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  )
where

import qualified Paths_brittany
import           Language.Haskell.Brittany

import           Network.Wai
import           Network.HTTP.Types
import qualified Network.Wai.Handler.Warp      as Warp

import           Data.String

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL

import           Control.Monad.Loops

import qualified Data.Text.Encoding            as Text
import qualified Data.Text                     as Text

import           Data.Version                   ( showVersion )

import qualified System.Mem
import qualified Control.Concurrent
import           Control.Concurrent.Async       ( async
                                                , waitEitherCatch
                                                , waitEitherCatchCancel
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Time.Clock
import           Data.Time.Format
import           Text.Parsec             hiding ( (<|>) )
~~~~

For long module names, things will be moved one line below and aligned as
before. Long identifiers may overflow our 80 column limit:

~~~~.hs
import qualified Example.Very.Long.Module.Name.Internal
                                               as T
import           Example.Very.Long.Module.Name.Internal
                                                ( someFunc
                                                , MyDataType
                                                , globalConstant
                                                )
import           Example.Very.Long.Module.Name.Internal
                                                ( someVeryLongAndDescriptiveFunctionName
                                                )
~~~~

## Alternative setting - long identifiers

If you have many long module names or use large identifiers, you might
be interested in these alternative settings:

~~~~
conf_layout:
  lconfig_importColumn: 21
  lconfig_importAsColumn: 70
~~~~

Now, our previous examples becomes:

~~~~.hs
import qualified Example.Very.Long.Module.Name.Internal            as T
import           Example.Very.Long.Module.Name.Internal
                   ( someFunc
                   , MyDataType
                   , globalConstant
                   )
import           Example.Very.Long.Module.Name.Internal
                   ( someVeryLongAndDescriptiveFunctionName )
~~~~

## Alternative setting - "IndentPolicyLeft"

The global switch "indent policy" that has the rough intention of removing any
cases of "hanging indentation" also affects module layouting:

~~~~
conf_layout:
  lconfig_indentPolicy: IndentPolicyLeft
~~~~

Now, our previous examples becomes:

~~~~.hs
import qualified Example.Very.Long.Module.Name.Internal as T
import Example.Very.Long.Module.Name.Internal
  (someFunc, MyDataType, globalConstant)
import Example.Very.Long.Module.Name.Internal
  (someVeryLongAndDescriptiveFunctionName)
~~~~
