module Language.Haskell.Brittany.Config
  ( CConfig(..)
  , CDebugConfig(..)
  , CLayoutConfig(..)
  , DebugConfig
  , LayoutConfig
  , Config
  , configParser
  , staticDefaultConfig
  , readMergePersConfig
  , showConfigYaml
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Types
import           Language.Haskell.Brittany.LayouterBasics

import qualified Data.Yaml

import           UI.Butcher.Monadic

import qualified System.Console.CmdArgs.Explicit as CmdArgs

import           Language.Haskell.Brittany.Config.Types
import           Language.Haskell.Brittany.Utils

import           Data.Coerce ( Coercible, coerce )



configParser :: CmdParser Identity out (CConfig Option)
configParser = do
  -- TODO: why does the default not trigger; ind never should be []!!
  ind                <- addFlagReadParams "" ["indent"] "AMOUNT" (flagHelpStr "spaces per indentation level")
  cols               <- addFlagReadParams "" ["columns"] "AMOUNT" (flagHelpStr "target max columns (80 is an old default for this)")
  importCol          <- addFlagReadParams "" ["import-col"] "N" (flagHelpStr "column to align import lists at")

  dumpConfig         <- addSimpleBoolFlag "" ["dump-config"] (flagHelp $ parDoc "dump the programs full config (commandline + file + defaults)")
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

  outputOnErrors <- addSimpleBoolFlag "" ["output-on-errors"] (flagHelp $ parDoc "even when there are errors, produce output (or try to to the degree possible")
  wError            <- addSimpleBoolFlag "" ["werror"] (flagHelp $ parDoc "treat warnings as errors")
  omitValidCheck    <- addSimpleBoolFlag "" ["omit-output-check"] (flagHelp $ parDoc "omit checking if the output is syntactically valid; for dev on brittany")

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
      }
    , _conf_layout = LayoutConfig
      { _lconfig_cols                      = optionConcat cols
      , _lconfig_indentPolicy              = mempty
      , _lconfig_indentAmount              = optionConcat ind
      , _lconfig_indentWhereSpecial        = mempty -- falseToNothing _
      , _lconfig_indentListSpecial         = mempty -- falseToNothing _
      , _lconfig_importColumn              = optionConcat importCol
      , _lconfig_altChooser                = mempty
      , _lconfig_columnAlignMode           = mempty
      , _lconfig_alignmentLimit            = mempty
      , _lconfig_alignmentBreakOnMultiline = mempty
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

readMergePersConfig
  :: System.IO.FilePath -> Bool -> CConfig Option -> MaybeT IO (CConfig Option)
readMergePersConfig path shouldCreate conf = do
  exists <- liftIO $ System.Directory.doesFileExist path
  if
    | exists -> do
        contents <- liftIO $ ByteString.readFile path -- no lazy IO, tyvm.
        fileConf <- case Data.Yaml.decodeEither contents of
          Left e -> do
            liftIO
              $ putStrErrLn
              $ "error reading in brittany config from " ++ path ++ ":"
            liftIO $ putStrErrLn e
            mzero
          Right x -> return x
        return $ fileConf Semigroup.<> conf
    | shouldCreate -> do
        liftIO $ ByteString.writeFile path
               $ Data.Yaml.encode
               $ cMap (Option . Just . runIdentity) staticDefaultConfig
        return $ conf
    | otherwise -> do
        return conf

showConfigYaml :: Config -> String
showConfigYaml = Data.ByteString.Char8.unpack
               . Data.Yaml.encode
               . cMap (\(Identity x) -> Just x)
