{-# LANGUAGE DataKinds #-}

module Main where



#include "prelude.inc"

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint.Parsers
import qualified Data.Map as Map

import           Text.Read (Read(..))
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Data.Text.Lazy.Builder as Text.Builder

import           Control.Monad (zipWithM)
import           Data.CZipWith

import qualified Debug.Trace as Trace

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal
import           Language.Haskell.Brittany.Internal.Config
import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Utils

import qualified Text.PrettyPrint as PP

import           DataTreePrint
import           UI.Butcher.Monadic

import qualified System.Exit
import qualified System.Directory as Directory
import qualified System.FilePath.Posix as FilePath

import qualified DynFlags as GHC
import qualified GHC.LanguageExtensions.Type as GHC

import Paths_brittany


data WriteMode = Display | Inplace

instance Read WriteMode where
  readPrec = val "display" Display <|> val "inplace" Inplace
   where
    val iden v = ReadPrec.lift $ ReadP.string iden >> return v

instance Show WriteMode where
  show Display = "display"
  show Inplace = "inplace"


main :: IO ()
main = mainFromCmdParserWithHelpDesc mainCmdParser

helpDoc :: PP.Doc
helpDoc = PP.vcat $ List.intersperse
  (PP.text "")
  [ parDocW
    [ "Reformats one or more haskell modules."
    , "Currently affects only type signatures and function bindings;"
    , "everything else is left unmodified."
    , "Based on ghc-exactprint, thus (theoretically) supporting all"
    , "that ghc does."
    ]
  , parDoc $ "Example invocations:"
  , PP.hang (PP.text "") 2 $ PP.vcat
      [ PP.text "brittany"
      , PP.hang (PP.text "  ") 2 $ PP.text "read from stdin, output to stdout"
      ]
  , PP.hang (PP.text "") 2 $ PP.vcat
      [ PP.text "brittany --indent=4 --write-mode=inplace *.hs"
      , PP.nest 2 $ PP.vcat
        [ PP.text "run on all modules in current directory (no backup!)"
        , PP.text "4 spaces indentation"
        ]
      ]
  , parDocW
    [ "This program is written carefully and contains safeguards to ensure"
    , "the transformation does not change semantics (or the syntax tree at all)"
    , "and that no comments are removed."
    , "Nonetheless, this is a young project, and there will always be bugs."
    , "Please do check the output and do not let brittany override your large"
    , "codebase without having backups."
    ]
  , parDoc $ "There is NO WARRANTY, to the extent permitted by law."
  , parDocW ["This program is free software released under the AGPLv3.", "For details use the --license flag."]
  , parDoc $ "See https://github.com/lspitzner/brittany"
  , parDoc $ "Please report bugs at" ++ " https://github.com/lspitzner/brittany/issues"
  ]

licenseDoc :: PP.Doc
licenseDoc = PP.vcat $ List.intersperse
  (PP.text "")
  [ parDoc $ "Copyright (C) 2016-2017 Lennart Spitzner"
  , parDocW
    [ "This program is free software: you can redistribute it and/or modify"
    , "it under the terms of the GNU Affero General Public License,"
    , "version 3, as published by the Free Software Foundation."
    ]
  , parDocW
    [ "This program is distributed in the hope that it will be useful,"
    , "but WITHOUT ANY WARRANTY; without even the implied warranty of"
    , "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    , "GNU Affero General Public License for more details."
    ]
  , parDocW
    [ "You should have received a copy of the GNU Affero General Public"
    , "License along with this program.  If not, see"
    , "<http://www.gnu.org/licenses/>."
    ]
  ]


mainCmdParser :: CommandDesc () -> CmdParser Identity (IO ()) ()
mainCmdParser helpDesc = do
  addCmdSynopsis "haskell source pretty printer"
  addCmdHelp $ helpDoc
  -- addCmd "debugArgs" $ do
  addHelpCommand helpDesc
  addCmd "license" $ addCmdImpl $ print $ licenseDoc
  -- addButcherDebugCommand
  reorderStart
  printHelp      <- addSimpleBoolFlag "h" ["help"] mempty
  printVersion   <- addSimpleBoolFlag "" ["version"] mempty
  printLicense   <- addSimpleBoolFlag "" ["license"] mempty
  configPaths    <- addFlagStringParams "" ["config-file"] "PATH" (flagHelpStr "path to config file") -- TODO: allow default on addFlagStringParam ?
  cmdlineConfig  <- configParser
  suppressOutput <- addSimpleBoolFlag
    ""
    ["suppress-output"]
    (flagHelp $ parDoc "suppress the regular output, i.e. the transformed haskell source")
  _verbosity <- addSimpleCountFlag "v" ["verbose"] (flagHelp $ parDoc "[currently without effect; TODO]")
  writeMode  <- addFlagReadParam
    ""
    ["write-mode"]
    "(display|inplace)"
    Flag
      { _flag_help    = Just $ PP.vcat
          [ PP.text "display: output for any input(s) goes to stdout"
          , PP.text "inplace: override respective input file (without backup!)"
          ]
      , _flag_default = Just Display
      }
  inputParams <- addParamNoFlagStrings "PATH" (paramHelpStr "paths to input haskell source files")
  reorderStop
  desc        <- peekCmdDesc
  addCmdImpl $ void $ do
    when printLicense $ do
      print licenseDoc
      System.Exit.exitSuccess
    when printVersion $ do
      do
        putStrLn $ "brittany version " ++ showVersion version
        putStrLn $ "Copyright (C) 2016-2017 Lennart Spitzner"
        putStrLn $ "There is NO WARRANTY, to the extent permitted by law."
      System.Exit.exitSuccess
    when printHelp $ do
      liftIO $ print $ ppHelpShallow desc
      System.Exit.exitSuccess

    let inputPaths  = if null inputParams then [Nothing] else map Just inputParams
    let outputPaths = case writeMode of
                      Display -> repeat Nothing
                      Inplace -> inputPaths

    config <- runMaybeT (readConfigs cmdlineConfig configPaths) >>= \case
      Nothing -> System.Exit.exitWith (System.Exit.ExitFailure 53)
      Just x  -> return x
    when (confUnpack $ _dconf_dump_config $ _conf_debug $ config) $ do
      trace (showConfigYaml config) $ return ()

    results <- zipWithM (coreIO putStrErrLn config suppressOutput) inputPaths outputPaths
    case sequence_ results of
      Left  _ -> System.Exit.exitWith (System.Exit.ExitFailure 1)
      Right _ -> pure ()


-- | The main IO parts for the default mode of operation, and after commandline
-- and config stuff is processed.
coreIO
  :: (String -> IO ()) -- ^ error output function. In parallel operation, you
                       -- may want serialize the different outputs and
                       -- consequently not directly print to stderr.
  -> Config -- ^ global program config.
  -> Bool   -- ^ whether to supress output (to stdout). Purely IO flag, so
            -- currently not part of program config.
  -> Maybe FilePath.FilePath -- ^ input filepath; stdin if Nothing.
  -> Maybe FilePath.FilePath -- ^ output filepath; stdout if Nothing.
  -> IO (Either Int ())      -- ^ Either an errorNo, or success.
coreIO putErrorLnIO config suppressOutput inputPathM outputPathM = ExceptT.runExceptT $ do
  let putErrorLn         = liftIO . putErrorLnIO :: String -> ExceptT.ExceptT e IO ()
  let ghcOptions         = config & _conf_forward & _options_ghc & runIdentity
  -- there is a good of code duplication between the following code and the
  -- `pureModuleTransform` function. Unfortunately, there are also a good
  -- amount of slight differences: This module is a bit more verbose, and
  -- it tries to use the full-blown `parseModule` function which supports
  -- CPP (but requires the input to be a file..).
  let cppMode            = config & _conf_preprocessor & _ppconf_CPPMode & confUnpack
  -- the flag will do the following: insert a marker string
  -- ("-- BRITTANY_INCLUDE_HACK ") right before any lines starting with
  -- "#include" before processing (parsing) input; and remove that marker
  -- string from the transformation output.
  let hackAroundIncludes = config & _conf_preprocessor & _ppconf_hackAroundIncludes & confUnpack
  let exactprintOnly     = config & _conf_debug & _dconf_roundtrip_exactprint_only & confUnpack
  let cppCheckFunc dynFlags = if GHC.xopt GHC.Cpp dynFlags
        then case cppMode of
          CPPModeAbort -> do
            return $ Left "Encountered -XCPP. Aborting."
          CPPModeWarn -> do
            putErrorLnIO
              $  "Warning: Encountered -XCPP."
              ++ " Be warned that -XCPP is not supported and that"
              ++ " brittany cannot check that its output is syntactically"
              ++ " valid in its presence."
            return $ Right True
          CPPModeNowarn -> return $ Right True
        else return $ Right False
  parseResult <- case inputPathM of
    Nothing -> do
      -- TODO: refactor this hack to not be mixed into parsing logic
      let hackF s = if "#include" `isPrefixOf` s then "-- BRITTANY_INCLUDE_HACK " ++ s else s
      let hackTransform =
            if hackAroundIncludes && not exactprintOnly then List.intercalate "\n" . fmap hackF . lines' else id
      inputString <- liftIO $ System.IO.hGetContents System.IO.stdin
      liftIO $ parseModuleFromString ghcOptions "stdin" cppCheckFunc (hackTransform inputString)
    Just p -> liftIO $ parseModule ghcOptions p cppCheckFunc
  case parseResult of
    Left left -> do
      putErrorLn "parse error:"
      putErrorLn $ show left
      ExceptT.throwE 60
    Right (anns, parsedSource, hasCPP) -> do
      when (config & _conf_debug .> _dconf_dump_ast_full .> confUnpack) $ do
        let val = printTreeWithCustom 100 (customLayouterF anns) parsedSource
        trace ("---- ast ----\n" ++ show val) $ return ()
      (errsWarns, outLText) <- do
        if exactprintOnly
          then do
            pure ([], TextL.pack $ ExactPrint.exactPrint parsedSource anns)
          else do
            let omitCheck = config & _conf_errorHandling .> _econf_omit_output_valid_check .> confUnpack
            (ews, outRaw) <- if hasCPP || omitCheck
              then return $ pPrintModule config anns parsedSource
              else liftIO $ pPrintModuleAndCheck config anns parsedSource
            let hackF s = fromMaybe s $ TextL.stripPrefix (TextL.pack "-- BRITTANY_INCLUDE_HACK ") s
            pure $ if hackAroundIncludes
              then (ews, TextL.intercalate (TextL.pack "\n") $ fmap hackF $ TextL.splitOn (TextL.pack "\n") outRaw)
              else (ews, outRaw)
      let customErrOrder ErrorInput{}         = 4
          customErrOrder LayoutWarning{}      = 0 :: Int
          customErrOrder ErrorOutputCheck{}   = 1
          customErrOrder ErrorUnusedComment{} = 2
          customErrOrder ErrorUnknownNode{}   = 3
      when (not $ null errsWarns) $ do
        let groupedErrsWarns = Data.List.Extra.groupOn customErrOrder $ List.sortOn customErrOrder $ errsWarns
        groupedErrsWarns `forM_` \case
          (ErrorOutputCheck{}:_) -> do
            putErrorLn $ "ERROR: brittany pretty printer" ++ " returned syntactically invalid result."
          (ErrorInput str:_) -> do
            putErrorLn $ "ERROR: parse error: " ++ str
          uns@(ErrorUnknownNode{}:_) -> do
            putErrorLn $ "ERROR: encountered unknown syntactical constructs:"
            uns `forM_` \case
              ErrorUnknownNode str ast -> do
                putErrorLn str
                when (config & _conf_debug & _dconf_dump_ast_unknown & confUnpack) $ do
                  putErrorLn $ "  " ++ show (astToDoc ast)
              _ -> error "cannot happen (TM)"
          warns@(LayoutWarning{}:_) -> do
            putErrorLn $ "WARNINGS:"
            warns `forM_` \case
              LayoutWarning str -> putErrorLn str
              _                 -> error "cannot happen (TM)"
          unused@(ErrorUnusedComment{}:_) -> do
            putErrorLn
              $  "Error: detected unprocessed comments."
              ++ " The transformation output will most likely"
              ++ " not contain some of the comments"
              ++ " present in the input haskell source file."
            putErrorLn $ "Affected are the following comments:"
            unused `forM_` \case
              ErrorUnusedComment str -> putErrorLn str
              _                      -> error "cannot happen (TM)"
          [] -> error "cannot happen"
      -- TODO: don't output anything when there are errors unless user
      -- adds some override?
      let hasErrors = case config & _conf_errorHandling & _econf_Werror & confUnpack of
            False -> 0 < maximum (-1 : fmap customErrOrder errsWarns)
            True  -> not $ null errsWarns
          outputOnErrs = config & _conf_errorHandling & _econf_produceOutputOnErrors & confUnpack
          shouldOutput = not suppressOutput && (not hasErrors || outputOnErrs)

      when shouldOutput $ addTraceSep (_conf_debug config) $ case outputPathM of
        Nothing -> liftIO $ TextL.IO.putStr $ outLText
        Just p  -> liftIO $ TextL.IO.writeFile p $ outLText

      when hasErrors $ ExceptT.throwE 70
 where
  addTraceSep conf =
    if or
         [ confUnpack $ _dconf_dump_annotations conf
         , confUnpack $ _dconf_dump_ast_unknown conf
         , confUnpack $ _dconf_dump_ast_full conf
         , confUnpack $ _dconf_dump_bridoc_raw conf
         , confUnpack $ _dconf_dump_bridoc_simpl_alt conf
         , confUnpack $ _dconf_dump_bridoc_simpl_floating conf
         , confUnpack $ _dconf_dump_bridoc_simpl_columns conf
         , confUnpack $ _dconf_dump_bridoc_simpl_indent conf
         , confUnpack $ _dconf_dump_bridoc_final conf
         ]
      then trace "----"
      else id


readConfigs :: CConfig Option -> [System.IO.FilePath] -> MaybeT IO Config
readConfigs cmdlineConfig configPaths = do
  userBritPathSimple <- liftIO $ Directory.getAppUserDataDirectory "brittany"
  userBritPathXdg    <- liftIO
    $ Directory.getXdgDirectory Directory.XdgConfig "brittany"
  let userConfigPathSimple = userBritPathSimple FilePath.</> "config.yaml"
  let userConfigPathXdg    = userBritPathXdg FilePath.</> "config.yaml"
  let
    findLocalConfig :: MaybeT IO (Maybe (CConfig Option))
    findLocalConfig = do
      cwd <- liftIO $ Directory.getCurrentDirectory
      let dirParts = FilePath.splitDirectories cwd
      let searchDirs =
            [ FilePath.joinPath x | x <- reverse $ List.inits dirParts ]
      -- when cwd is "a/b/c", searchDirs is ["a/b/c", "a/b", "a", "/"]
      mFilePath <- liftIO $ Directory.findFileWith Directory.doesFileExist
                                                   searchDirs
                                                   "brittany.yaml"
      case mFilePath of
        Nothing -> pure Nothing
        Just fp -> readConfig fp
  configsRead <- case configPaths of
    [] -> do
      localConfig      <- findLocalConfig
      userConfigSimple <- readConfig userConfigPathSimple
      userConfigXdg    <- readConfig userConfigPathXdg
      let userConfig = userConfigSimple <|> userConfigXdg
      when (Data.Maybe.isNothing userConfig) $ do
        liftIO $ Directory.createDirectoryIfMissing False userBritPathXdg
        writeDefaultConfig userConfigPathXdg
      -- rightmost has highest priority
      pure $ [userConfig, localConfig]
    paths -> readConfig `mapM` reverse paths
                   -- reverse to give highest priority to the first
  merged <-
    pure $ Semigroup.mconcat $ catMaybes $ configsRead ++ [Just cmdlineConfig]
  return $ cZipWith fromOptionIdentity staticDefaultConfig merged
