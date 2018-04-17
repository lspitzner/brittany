module Language.Haskell.Brittany.Internal.Config
  ( CConfig(..)
  , CDebugConfig(..)
  , CLayoutConfig(..)
  , DebugConfig
  , LayoutConfig
  , Config
  , cmdlineConfigParser
  , staticDefaultConfig
  , forwardOptionsSyntaxExtsEnabled
  , readConfig
  , userConfigPath
  , findLocalConfigPath
  , readConfigs
  , readConfigsWithUserConfig
  , writeDefaultConfig
  , showConfigYaml
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics

import qualified Data.Yaml
import           Data.CZipWith

import           UI.Butcher.Monadic
import Data.Monoid ((<>))

import qualified System.Console.CmdArgs.Explicit as CmdArgs

import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Config.Types.Instances
import           Language.Haskell.Brittany.Internal.Utils

import           Data.Coerce ( Coercible, coerce )
import qualified Data.List.NonEmpty as NonEmpty

import qualified System.Directory as Directory
import qualified System.FilePath.Posix as FilePath

staticDefaultConfig :: Config
staticDefaultConfig = Config
  { _conf_version = coerce (1 :: Int)
  , _conf_debug   = DebugConfig
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
    , _dconf_roundtrip_exactprint_only  = coerce False
    }
  , _conf_layout = LayoutConfig
    { _lconfig_cols                      = coerce (80 :: Int)
    , _lconfig_indentPolicy              = coerce IndentPolicyFree
    , _lconfig_indentAmount              = coerce (2 :: Int)
    , _lconfig_indentWhereSpecial        = coerce True
    , _lconfig_indentListSpecial         = coerce True
    , _lconfig_importColumn              = coerce (50 :: Int)
    , _lconfig_importAsColumn            = coerce (50 :: Int)
    , _lconfig_altChooser                = coerce (AltChooserBoundedSearch 3)
    , _lconfig_columnAlignMode           = coerce (ColumnAlignModeMajority 0.7)
    , _lconfig_alignmentLimit            = coerce (30 :: Int)
    , _lconfig_alignmentBreakOnMultiline = coerce True
    , _lconfig_hangingTypeSignature      = coerce False
    , _lconfig_reformatModulePreamble    = coerce True
    , _lconfig_allowSingleLineExportList = coerce False
    }
  , _conf_errorHandling = ErrorHandlingConfig
    { _econf_produceOutputOnErrors   = coerce False
    , _econf_Werror                  = coerce False
    , _econf_ExactPrintFallback      = coerce ExactPrintFallbackModeInline
    , _econf_omit_output_valid_check = coerce False
    }
  , _conf_preprocessor = PreProcessorConfig
    { _ppconf_CPPMode            = coerce CPPModeAbort
    , _ppconf_hackAroundIncludes = coerce False
    }
  , _conf_forward = ForwardOptions
    { _options_ghc = Identity []
    }
  }

forwardOptionsSyntaxExtsEnabled :: ForwardOptions
forwardOptionsSyntaxExtsEnabled = ForwardOptions
  { _options_ghc = Identity
    [ "-XLambdaCase"
    , "-XMultiWayIf"
    , "-XGADTs"
    , "-XPatternGuards"
    , "-XViewPatterns"
    , "-XTupleSections"
    , "-XExplicitForAll"
    , "-XImplicitParams"
    , "-XQuasiQuotes"
    , "-XTemplateHaskell"
    , "-XBangPatterns"
    , "-XTypeApplications"
    ]
  }

cmdlineConfigParser :: CmdParser Identity out (CConfig Option)
cmdlineConfigParser = do
  -- TODO: why does the default not trigger; ind never should be []!!
  ind                <- addFlagReadParams "" ["indent"] "AMOUNT" (flagHelpStr "spaces per indentation level")
  cols               <- addFlagReadParams "" ["columns"] "AMOUNT" (flagHelpStr "target max columns (80 is an old default for this)")
  importCol          <- addFlagReadParams "" ["import-col"] "N" (flagHelpStr "column to align import lists at")
  importAsCol        <- addFlagReadParams "" ["import-as-col"] "N" (flagHelpStr "column to qualified-as module names at")

  dumpConfig         <- addSimpleBoolFlag "" ["dump-config"] (flagHelp $ parDoc "dump the programs full config (merged commandline + file + defaults)")
  dumpAnnotations    <- addSimpleBoolFlag "" ["dump-annotations"] (flagHelp $ parDoc "dump the full annotations returned by ghc-exactprint")
  dumpUnknownAST     <- addSimpleBoolFlag "" ["dump-ast-unknown"] (flagHelp $ parDoc "dump the ast for any nodes not transformed, but copied as-is by brittany")
  dumpCompleteAST    <- addSimpleBoolFlag "" ["dump-ast-full"] (flagHelp $ parDoc "dump the full ast")
  dumpBriDocRaw      <- addSimpleBoolFlag "" ["dump-bridoc-raw"] (flagHelp $ parDoc "dump the pre-transformation bridoc")
  dumpBriDocAlt      <- addSimpleBoolFlag "" ["dump-bridoc-alt"] (flagHelp $ parDoc "dump the partially transformed bridoc: after transformation: alt")
  dumpBriDocPar      <- addSimpleBoolFlag "" ["dump-bridoc-par"] (flagHelp $ parDoc "dump the partially transformed bridoc: after transformation: par")
  dumpBriDocFloating <- addSimpleBoolFlag ""
                                          ["dump-bridoc-floating"]
                                          (flagHelp $ parDoc "dump the partially transformed bridoc: after transformation: floating")
  dumpBriDocColumns <- addSimpleBoolFlag "" ["dump-bridoc-columns"] (flagHelp $ parDoc "dump the partially transformed bridoc: after transformation: columns")
  dumpBriDocIndent  <- addSimpleBoolFlag "" ["dump-bridoc-indent"] (flagHelp $ parDoc "dump the partially transformed bridoc: after transformation: indent")
  dumpBriDocFinal   <- addSimpleBoolFlag "" ["dump-bridoc-final"] (flagHelp $ parDoc "dump the post-transformation bridoc")

  outputOnErrors <- addSimpleBoolFlag "" ["output-on-errors"] (flagHelp $ parDoc "even when there are errors, produce output (or try to to the degree possible)")
  wError            <- addSimpleBoolFlag "" ["werror"] (flagHelp $ parDoc "treat warnings as errors")
  omitValidCheck    <- addSimpleBoolFlag "" ["omit-output-check"] (flagHelp $ parDoc "omit checking if the output is syntactically valid (debugging)")

  roundtripOnly    <- addSimpleBoolFlag "" ["exactprint-only"] (flagHelp $ parDoc "do not reformat, but exclusively use exactprint to roundtrip (debugging)")

  optionsGhc        <- addFlagStringParams ""
                                           ["ghc-options"]
                                           "STRING"
                                           (flagHelp $ parDoc "allows to define default language extensions. The parameter is forwarded to ghc.")

  return $ Config
    { _conf_version = mempty
    , _conf_debug   = DebugConfig
      { _dconf_dump_config                = wrapLast $ falseToNothing dumpConfig
      , _dconf_dump_annotations           = wrapLast $ falseToNothing dumpAnnotations
      , _dconf_dump_ast_unknown           = wrapLast $ falseToNothing dumpUnknownAST
      , _dconf_dump_ast_full              = wrapLast $ falseToNothing dumpCompleteAST
      , _dconf_dump_bridoc_raw            = wrapLast $ falseToNothing dumpBriDocRaw
      , _dconf_dump_bridoc_simpl_alt      = wrapLast $ falseToNothing dumpBriDocAlt
      , _dconf_dump_bridoc_simpl_par      = wrapLast $ falseToNothing dumpBriDocPar
      , _dconf_dump_bridoc_simpl_floating = wrapLast $ falseToNothing dumpBriDocFloating
      , _dconf_dump_bridoc_simpl_columns  = wrapLast $ falseToNothing dumpBriDocColumns
      , _dconf_dump_bridoc_simpl_indent   = wrapLast $ falseToNothing dumpBriDocIndent
      , _dconf_dump_bridoc_final          = wrapLast $ falseToNothing dumpBriDocFinal
      , _dconf_roundtrip_exactprint_only  = wrapLast $ falseToNothing roundtripOnly
      }
    , _conf_layout = LayoutConfig
      { _lconfig_cols                      = optionConcat cols
      , _lconfig_indentPolicy              = mempty
      , _lconfig_indentAmount              = optionConcat ind
      , _lconfig_indentWhereSpecial        = mempty -- falseToNothing _
      , _lconfig_indentListSpecial         = mempty -- falseToNothing _
      , _lconfig_importColumn              = optionConcat importCol
      , _lconfig_importAsColumn            = optionConcat importAsCol
      , _lconfig_altChooser                = mempty
      , _lconfig_columnAlignMode           = mempty
      , _lconfig_alignmentLimit            = mempty
      , _lconfig_alignmentBreakOnMultiline = mempty
      , _lconfig_hangingTypeSignature      = mempty
      , _lconfig_reformatModulePreamble    = mempty
      , _lconfig_allowSingleLineExportList = mempty
      }
    , _conf_errorHandling = ErrorHandlingConfig
      { _econf_produceOutputOnErrors   = wrapLast $ falseToNothing outputOnErrors
      , _econf_Werror                  = wrapLast $ falseToNothing wError
      , _econf_ExactPrintFallback      = mempty
      , _econf_omit_output_valid_check = wrapLast $ falseToNothing omitValidCheck
      }
    , _conf_preprocessor = PreProcessorConfig
      { _ppconf_CPPMode            = mempty
      , _ppconf_hackAroundIncludes = mempty
      }
    , _conf_forward = ForwardOptions
      { _options_ghc = [ optionsGhc & List.unwords & CmdArgs.splitArgs | not $ null optionsGhc ]
      }
    }
 where
  falseToNothing = Option . Bool.bool Nothing (Just True)
  wrapLast :: Option a -> Option (Semigroup.Last a)
  wrapLast = fmap Semigroup.Last
  optionConcat :: (Semigroup.Semigroup (f a), Applicative f) => [a] -> Option (f a)
  optionConcat = mconcat . fmap (pure . pure)

-- configParser :: Parser Config
-- configParser = Config
--   <$> option (eitherReader $ maybe (Left "required <int>!") Right . readMaybe)
--         (long "indent" <> value 2 <> metavar "AMOUNT" <> help "spaces per indentation level")
--   <*> (Bar
--     <$> switch (long "bara" <> help "bara help")
--     <*> switch (long "barb")
--     <*> flag 3 5 (long "barc")
--   )
--
-- configParserInfo :: ParserInfo Config
-- configParserInfo = ParserInfo
--   { infoParser      = configParser
--   , infoFullDesc    = True
--   , infoProgDesc    = return $ PP.text "a haskell code formatting utility based on ghc-exactprint"
--   , infoHeader      = return $ PP.text "brittany"
--   , infoFooter      = empty
--   , infoFailureCode = (-55)
--   , infoIntersperse = True
--   }


-- | Reads a config from a file. If the file does not exist, returns
-- Nothing. If the file exists and parsing fails, prints to stderr and
-- aborts the MaybeT. Otherwise succeed via Just.
-- If the second parameter is True and the file does not exist, writes the
-- staticDefaultConfig to the file.
readConfig
  :: MonadIO m => System.IO.FilePath -> MaybeT m (Maybe (CConfig Option))
readConfig path = do
  exists <- liftIO $ System.Directory.doesFileExist path
  if exists
    then do
      contents <- liftIO $ ByteString.readFile path -- no lazy IO, tyvm.
      fileConf <- case Data.Yaml.decodeEither contents of
        Left e -> do
          liftIO
            $  putStrErrLn
            $  "error reading in brittany config from "
            ++ path
            ++ ":"
          liftIO $ putStrErrLn e
          mzero
        Right x -> return x
      return $ Just fileConf
    else return $ Nothing

-- | Looks for a user-global config file and return its path.
-- If there is no global config in a system, one will be created.
userConfigPath :: IO System.IO.FilePath
userConfigPath = do
  userBritPathSimple <- Directory.getAppUserDataDirectory "brittany"
  userBritPathXdg    <- Directory.getXdgDirectory Directory.XdgConfig "brittany"
  let searchDirs = [userBritPathSimple, userBritPathXdg]
  globalConfig <- Directory.findFileWith Directory.doesFileExist searchDirs "config.yaml"
  maybe (writeUserConfig userBritPathXdg) pure globalConfig
  where
    writeUserConfig dir = do
      let createConfPath = dir FilePath.</> "config.yaml"
      liftIO $ Directory.createDirectoryIfMissing True dir
      writeDefaultConfig $ createConfPath
      pure createConfPath

-- | Searches for a local (per-project) brittany config starting from a given directory
findLocalConfigPath :: System.IO.FilePath -> IO (Maybe System.IO.FilePath)
findLocalConfigPath dir = do
  let dirParts = FilePath.splitDirectories dir
  -- when provided dir is "a/b/c", searchDirs is ["a/b/c", "a/b", "a", "/"]
  let searchDirs = FilePath.joinPath <$> reverse (List.inits dirParts)
  Directory.findFileWith Directory.doesFileExist searchDirs "brittany.yaml"

-- | Reads specified configs.
readConfigs
  :: CConfig Option        -- ^ Explicit options, take highest priority
  -> [System.IO.FilePath]  -- ^ List of config files to load and merge, highest priority first
  -> MaybeT IO Config
readConfigs cmdlineConfig configPaths = do
  configs <- readConfig `mapM` configPaths
  let merged = Semigroup.sconcat $ NonEmpty.reverse (cmdlineConfig :| catMaybes configs)
  return $ cZipWith fromOptionIdentity staticDefaultConfig merged

-- | Reads provided configs
-- but also applies the user default configuration (with lowest priority)
readConfigsWithUserConfig
  :: CConfig Option        -- ^ Explicit options, take highest priority
  -> [System.IO.FilePath]  -- ^ List of config files to load and merge, highest priority first
  -> MaybeT IO Config
readConfigsWithUserConfig cmdlineConfig configPaths = do
  defaultPath <- liftIO $ userConfigPath
  readConfigs cmdlineConfig (configPaths ++ [defaultPath])

writeDefaultConfig :: MonadIO m => System.IO.FilePath -> m ()
writeDefaultConfig path =
  liftIO $ ByteString.writeFile path $ Data.Yaml.encode $ cMap
    (Option . Just . runIdentity)
    staticDefaultConfig

showConfigYaml :: Config -> String
showConfigYaml = Data.ByteString.Char8.unpack
               . Data.Yaml.encode
               . cMap (\(Identity x) -> Just x)

