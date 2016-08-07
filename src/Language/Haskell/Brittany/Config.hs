module Language.Haskell.Brittany.Config
  ( ConfigF(..)
  , DebugConfigF(..)
  , LayoutConfigF(..)
  , DebugConfig
  , LayoutConfig
  , Config
  , configParser
  , staticDefaultConfig
  , readMergePersConfig
  )
where



#include "prelude.inc"

import DynFlags ( getDynFlags )
import GHC ( runGhc, GenLocated(L), moduleNameString )
import qualified Parser        as GHC
import qualified ApiAnnotation as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified HeaderInfo    as GHC
import qualified Lexer         as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC
import qualified Parser        as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC
import RdrName ( RdrName(..) )
import Control.Monad.IO.Class
import GHC.Paths (libdir)
import HsSyn
import SrcLoc ( SrcSpan, Located )
-- import Outputable ( ppr, runSDoc )
-- import DynFlags ( unsafeGlobalDynFlags )

import           ApiAnnotation ( AnnKeywordId(..) )
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint.Parsers
import qualified Language.Haskell.GHC.ExactPrint.Preprocess as ExactPrint.Preprocess
import qualified Data.Map as Map

import qualified Data.Text.Lazy.Builder as Text.Builder

import qualified Debug.Trace as Trace

import Language.Haskell.Brittany.Types
import Language.Haskell.Brittany.LayoutBasics

-- import Data.Aeson
import GHC.Generics
import Control.Lens

import qualified Data.Yaml

import UI.Butcher.Monadic

import Language.Haskell.Brittany.Config.Types
import Language.Haskell.Brittany.Utils



configParser :: CmdParser Identity out (ConfigF Maybe)
configParser = do
  -- TODO: why does the default not trigger; ind never should be []!!
  ind <- addFlagReadParam "" ["indent"] "AMOUNT"
    (flagHelpStr "spaces per indentation level")
  cols <- addFlagReadParam "" ["columns"] "AMOUNT"
    (flagHelpStr "target max columns (80 is an old default for this)")
  importCol <- addFlagReadParam "" ["import-col"] "N"
    (flagHelpStr "column to align import lists at")

  dumpConfig      <- addSimpleBoolFlag "" ["dump-config"] (flagHelp $ parDoc "dump the programs full config (commandline + file + defaults)")
  dumpAnnotations <- addSimpleBoolFlag "" ["dump-annotations"] (flagHelp $ parDoc "dump the full annotations returned by ghc-exactprint")
  dumpUnknownAST  <- addSimpleBoolFlag "" ["dump-ast-unknown"] (flagHelp $ parDoc "dump the ast for any nodes not transformed, but copied as-is by brittany")
  dumpCompleteAST <- addSimpleBoolFlag "" ["dump-ast-full"] (flagHelp $ parDoc "dump the full ast")
  dumpBriDocRaw      <- addSimpleBoolFlag "" ["dump-bridoc-raw"]      (flagHelp $ parDoc "dump the pre-transformation bridoc")
  dumpBriDocAlt      <- addSimpleBoolFlag "" ["dump-bridoc-alt"]      (flagHelp $ parDoc "dump the partially transformed bridoc: after transformation: alt")
  dumpBriDocPar      <- addSimpleBoolFlag "" ["dump-bridoc-par"]      (flagHelp $ parDoc "dump the partially transformed bridoc: after transformation: par")
  dumpBriDocFloating <- addSimpleBoolFlag "" ["dump-bridoc-floating"] (flagHelp $ parDoc "dump the partially transformed bridoc: after transformation: floating")
  dumpBriDocColumns  <- addSimpleBoolFlag "" ["dump-bridoc-columns"]  (flagHelp $ parDoc "dump the partially transformed bridoc: after transformation: columns")
  dumpBriDocIndent   <- addSimpleBoolFlag "" ["dump-bridoc-indent"]   (flagHelp $ parDoc "dump the partially transformed bridoc: after transformation: indent")
  dumpBriDocFinal    <- addSimpleBoolFlag "" ["dump-bridoc-final"]    (flagHelp $ parDoc "dump the post-transformation bridoc")

  outputOnErrors <- addSimpleBoolFlag "" ["output-on-errors"] (flagHelp $ parDoc "even when there are errors, produce output (or try to to the degree possible")
  wError <- addSimpleBoolFlag "" ["werror"] (flagHelp $ parDoc "treat warnings as errors")
  
  return $ Config
    { _conf_debug = DebugConfig
      { _dconf_dump_config       = falseToNothing dumpConfig
      , _dconf_dump_annotations  = falseToNothing dumpAnnotations
      , _dconf_dump_ast_unknown  = falseToNothing dumpUnknownAST
      , _dconf_dump_ast_full     = falseToNothing dumpCompleteAST
      , _dconf_dump_bridoc_raw            = falseToNothing dumpBriDocRaw
      , _dconf_dump_bridoc_simpl_alt      = falseToNothing dumpBriDocAlt
      , _dconf_dump_bridoc_simpl_par      = falseToNothing dumpBriDocPar
      , _dconf_dump_bridoc_simpl_floating = falseToNothing dumpBriDocFloating
      , _dconf_dump_bridoc_simpl_columns  = falseToNothing dumpBriDocColumns
      , _dconf_dump_bridoc_simpl_indent   = falseToNothing dumpBriDocIndent
      , _dconf_dump_bridoc_final          = falseToNothing dumpBriDocFinal
      }
    , _conf_layout = LayoutConfig
      { _lconfig_cols               = listLastMaybe cols
      , _lconfig_indentPolicy       = Nothing
      , _lconfig_indentAmount       = listLastMaybe ind
      , _lconfig_indentWhereSpecial = Nothing -- falseToNothing _
      , _lconfig_indentListSpecial  = Nothing -- falseToNothing _
      , _lconfig_importColumn       = listLastMaybe importCol
      , _lconfig_altChooser         = Nothing
      }
    , _conf_errorHandling = ErrorHandlingConfig
      { _econf_produceOutputOnErrors = falseToNothing outputOnErrors
      , _econf_Werror                = falseToNothing wError
      }
    }
    where falseToNothing = Bool.bool Nothing (Just True)
          listLastMaybe = listToMaybe . reverse

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
  :: System.IO.FilePath -> Bool -> ConfigF Maybe -> MaybeT IO (ConfigF Maybe)
readMergePersConfig path shouldCreate conf = do
  exists <- liftIO $ System.Directory.doesFileExist path
  if
    | exists -> do
        contents <- liftIO $ ByteString.readFile path -- no lazy IO, tyvm.
        fileConf <- case Data.Yaml.decodeEither contents of
          Left e -> do
            liftIO
              $ putStrLn
              $ "error reading in brittany config from " ++ path ++ ":"
            liftIO $ putStrLn e
            mzero
          Right x -> return x
        return $ (cZip (<|>) conf fileConf)
    | shouldCreate -> do
        liftIO $ ByteString.writeFile path
               $ Data.Yaml.encode
               $ cMap (Just . runIdentity) staticDefaultConfig
        return $ conf
    | otherwise -> do
        return conf
