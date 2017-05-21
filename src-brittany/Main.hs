{-# LANGUAGE DataKinds #-}

module Main where



#include "prelude.inc"

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint.Parsers
import qualified Data.Map as Map

import qualified Data.Text.Lazy.Builder as Text.Builder

import           Data.CZipWith

import qualified Debug.Trace as Trace

import           Language.Haskell.Brittany.Types
import           Language.Haskell.Brittany
import           Language.Haskell.Brittany.Config
import           Language.Haskell.Brittany.Config.Types
import           Language.Haskell.Brittany.Utils

import qualified Text.PrettyPrint as PP

import           DataTreePrint
import           UI.Butcher.Monadic

import qualified System.Exit
import qualified System.Directory as Directory
import qualified System.FilePath.Posix as FilePath

import qualified DynFlags as GHC
import qualified GHC.LanguageExtensions.Type as GHC

import Paths_brittany



main :: IO ()
main = mainFromCmdParserWithHelpDesc mainCmdParser

mainCmdParser :: CommandDesc () -> CmdParser Identity (IO ()) ()
mainCmdParser helpDesc = do
  addCmdSynopsis "haskell source pretty printer"
  addCmdHelp $ PP.vcat $ List.intersperse (PP.text "")
    [ parDoc $ "Transforms one haskell module by reformatting"
            ++ " (parts of) the source code (while preserving the"
            ++ " parts not transformed)."
    , parDoc $ "Based on ghc-exactprint, thus (theoretically) supporting all"
            ++ " that ghc does."
    , parDoc $ "This is an early, experimental release."
            ++ " Only type-signatures and function-bindings are transformed."
            ++ " There is a check in place, but no warranties that the output"
            ++ " is valid haskell."
    , parDoc $ "There is NO WARRANTY, to the extent permitted by law."
    , parDoc $ "See https://github.com/lspitzner/brittany"
    , parDoc $ "Please report bugs at"
            ++ " https://github.com/lspitzner/brittany/issues"
    ]
  -- addCmd "debugArgs" $ do
  addHelpCommand helpDesc
  -- addButcherDebugCommand
  reorderStart
  printHelp <- addSimpleBoolFlag "" ["help"] mempty
  printVersion <- addSimpleBoolFlag "" ["version"] mempty
  inputPaths <- addFlagStringParams "i" ["input"] "PATH" (flagHelpStr "path to input haskell source file")
  outputPaths <- addFlagStringParams "o" ["output"] "PATH" (flagHelpStr "output file path")
  configPaths <- addFlagStringParams "" ["config-file"] "PATH" (flagHelpStr "path to config file") -- TODO: allow default on addFlagStringParam ?
  cmdlineConfig <- configParser
  suppressOutput <- addSimpleBoolFlag "" ["suppress-output"] (flagHelp $ parDoc "suppress the regular output, i.e. the transformed haskell source")
  _verbosity <- addSimpleCountFlag "v" ["verbose"] (flagHelp $ parDoc "[currently without effect; TODO]")
  reorderStop
  inputParam <- addStringParamOpt "PATH" (paramHelpStr "path to input haskell source file")
  desc <- peekCmdDesc
  addCmdImpl $ void $ do
    when printVersion $ do
      liftIO $ do
        putStrLn $ "brittany version " ++ showVersion version
        putStrLn $ "Copyright (C) 2016-2017 Lennart Spitzner"
        putStrLn $ "There is NO WARRANTY, to the extent permitted by law."
      System.Exit.exitSuccess
    when printHelp $ do
      liftIO $ print $ ppHelpShallow desc
      System.Exit.exitSuccess
    inputPathM <- case maybeToList inputParam ++ inputPaths of
      [] -> do
        return Nothing
      [x] -> return $ Just x
      _ -> do
        liftIO $ putStrErrLn $ "more than one input, aborting"
        System.Exit.exitWith (System.Exit.ExitFailure 50)
    outputPath <- case outputPaths of
      [] -> do
        return Nothing
      [x] -> return $ Just x
      _ -> do
        liftIO $ putStrErrLn $ "more than one output, aborting"
        System.Exit.exitWith (System.Exit.ExitFailure 50)
    config <- runMaybeT (readConfigs cmdlineConfig configPaths) >>= \case
      Nothing -> System.Exit.exitWith (System.Exit.ExitFailure 50)
      Just x -> return x
    when (confUnpack $ _dconf_dump_config $ _conf_debug $ config) $ do
      trace (showConfigYaml config) $ return ()
    let ghcOptions = config
                   & _conf_forward
                   & _options_ghc
                   & runIdentity
    liftIO $ do
      let cppMode = config
                  & _conf_preprocessor
                  & _ppconf_CPPMode
                  & runIdentity
                  & Semigroup.getLast
      let cppCheckFunc dynFlags = if GHC.xopt GHC.Cpp dynFlags
            then case cppMode of
              CPPModeAbort -> do
                return $ Left "Encountered -XCPP. Aborting."
              CPPModeWarn -> do
                putStrErrLn
                  $  "Warning: Encountered -XCPP."
                  ++ " Be warned that -XCPP is not supported and that"
                  ++ " brittany cannot check that its output is syntactically"
                  ++ " valid in its presence."
                return $ Right True
              CPPModeNowarn ->
                return $ Right True
            else return $ Right False
      parseResult <- case inputPathM of
        Nothing -> parseModuleFromString ghcOptions "stdin" cppCheckFunc
                   =<< System.IO.hGetContents System.IO.stdin
        Just p -> parseModule ghcOptions p cppCheckFunc
      case parseResult of
        Left left -> do
          putStrErrLn "parse error:"
          printErr left
          System.Exit.exitWith (System.Exit.ExitFailure 60)
        Right (anns, parsedSource, hasCPP) -> do
          when (config & _conf_debug .> _dconf_dump_ast_full .> confUnpack) $ do
            let val = printTreeWithCustom 100 (customLayouterF anns) parsedSource
            trace ("---- ast ----\n" ++ show val) $ return ()
          -- mapM_ printErr (Map.toList anns)
          -- let L _ (HsModule name exports imports decls _ _) = parsedSource
          -- let someDecls = take 3 decls
          -- -- let out = ExactPrint.exactPrint parsedSource anns
          -- let out = do
          --       decl <- someDecls
          --       ExactPrint.exactPrint decl anns
          let omitCheck = config & _conf_errorHandling .> _econf_omit_output_valid_check .> confUnpack
          (errsWarns, outLText) <- if hasCPP || omitCheck
            then return $ pPrintModule config anns parsedSource
            else pPrintModuleAndCheck config anns parsedSource
          let customErrOrder LayoutWarning{}            = 0 :: Int
              customErrOrder LayoutErrorOutputCheck{}   = 1
              customErrOrder LayoutErrorUnusedComment{} = 2
              customErrOrder LayoutErrorUnknownNode{}   = 3
          when (not $ null errsWarns) $ do
            let groupedErrsWarns = Data.List.Extra.groupOn customErrOrder
                                 $ List.sortOn customErrOrder
                                 $ errsWarns
            groupedErrsWarns `forM_` \case
              (LayoutErrorOutputCheck{}:_) -> do
                putStrErrLn $ "ERROR: brittany pretty printer returned syntactically invalid result."
              uns@(LayoutErrorUnknownNode{}:_) -> do
                putStrErrLn $ "ERROR: encountered unknown syntactical constructs:"
                uns `forM_` \case
                  LayoutErrorUnknownNode str ast -> do
                    putStrErrLn str
                    when (config & _conf_debug & _dconf_dump_ast_unknown & confUnpack) $ do
                      putStrErrLn $ "  " ++ show (astToDoc ast)
                  _ -> error "cannot happen (TM)"
              warns@(LayoutWarning{}:_) -> do
                putStrErrLn $ "WARNINGS:"
                warns `forM_` \case
                  LayoutWarning str -> putStrErrLn str
                  _ -> error "cannot happen (TM)"
              unused@(LayoutErrorUnusedComment{}:_) -> do
                putStrErrLn $ "Error: detected unprocessed comments. the transformation "
                        ++ "output will most likely not contain certain of the comments "
                        ++ "present in the input haskell source file."
                putStrErrLn $ "Affected are the following comments:"
                unused `forM_` \case
                  LayoutErrorUnusedComment str -> putStrErrLn str
                  _ -> error "cannot happen (TM)"
              [] -> error "cannot happen"
          -- TODO: don't output anything when there are errors unless user
          -- adds some override?
          let hasErrors = case config
                               & _conf_errorHandling
                               & _econf_Werror
                               & confUnpack of
                False -> 0 < maximum (-1 : fmap customErrOrder errsWarns)
                True  -> not $ null errsWarns
              outputOnErrs = config
                           & _conf_errorHandling
                           & _econf_produceOutputOnErrors
                           & confUnpack
          let shouldOutput = not suppressOutput
                          && (not hasErrors || outputOnErrs)

          when shouldOutput $ addTraceSep (_conf_debug config) $ case outputPath of
            Nothing -> TextL.IO.putStr     $ outLText
            Just p -> TextL.IO.writeFile p $ outLText

          when hasErrors $
            System.Exit.exitWith (System.Exit.ExitFailure 70)
  where
    addTraceSep conf = if or
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
  let defLocalConfigPath = "brittany.yaml"
  userBritPath <- liftIO $ Directory.getAppUserDataDirectory "brittany"
  let defUserConfigPath = userBritPath FilePath.</> "config.yaml"
  merged <- case configPaths of
    []    -> do
      liftIO $ Directory.createDirectoryIfMissing False userBritPath
      return cmdlineConfig
        >>= readMergePersConfig defLocalConfigPath False
        >>= readMergePersConfig defUserConfigPath  True
    -- TODO: ensure that paths exist ?
    paths -> foldl (\prev p -> prev >>= readMergePersConfig p False)
                   (return cmdlineConfig)
                   paths
  return $ cZipWith fromOptionIdentity staticDefaultConfig merged
