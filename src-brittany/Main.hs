{-# LANGUAGE DataKinds #-}

module Main where



#include "prelude.inc"

import DynFlags ( getDynFlags )
import GHC ( runGhc, GenLocated(L), moduleNameString )
import qualified Parser as GHC.Parser
import RdrName ( RdrName(..) )
import Control.Monad.IO.Class
import GHC.Paths (libdir)
import HsSyn
import SrcLoc ( SrcSpan, Located )
-- import Outputable ( ppr, runSDoc )
-- import DynFlags ( unsafeGlobalDynFlags )

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint.Parsers
import qualified Data.Map as Map

import qualified Data.Text.Lazy.Builder as Text.Builder

import qualified Debug.Trace as Trace

import Language.Haskell.Brittany.Types
import Language.Haskell.Brittany.LayoutBasics
import Language.Haskell.Brittany
import Language.Haskell.Brittany.Config
import Language.Haskell.Brittany.Config.Types
import Language.Haskell.Brittany.Utils

import qualified Text.PrettyPrint as PP

import           DataTreePrint
import           UI.Butcher.Monadic

import qualified System.Exit

import Paths_brittany



main :: IO ()
main = mainFromCmdParser mainCmdParser

mainCmdParser :: CmdParser Identity (IO ()) ()
mainCmdParser = do
  addCmdSynopsis "haskell source pretty printer"
  addCmdHelp $ PP.vcat $ List.intersperse (PP.text "")
    [ parDoc $ "Transforms one haskell module by reformatting"
            ++ " (parts of) the source code, while preserving the"
            ++ " parts not transformed."
            ++ " Especially, comments are preserved completely"
            ++ " and newlines are in many cases."
    , parDoc $ "Based on ghc-exactprint, thus supporting all that"
            ++ " ghc does."
    ]
  -- addCmd "debugArgs" $ do
  addHelpCommand
  -- addButcherDebugCommand
  reorderStart
  printHelp <- addSimpleBoolFlag "" ["help"] mempty
  printVersion <- addSimpleBoolFlag "" ["version"] mempty
  inputPaths <- addFlagStringParam "i" ["input"] "PATH" (flagHelpStr "path to input haskell source file")
  outputPaths <- addFlagStringParam "o" ["output"] "PATH" (flagHelpStr "output file path")
  configPaths <- addFlagStringParam "" ["config-file"] "PATH" (flagHelpStr "path to config file") -- TODO: allow default on addFlagStringParam ?
  cmdlineConfig <- configParser
  suppressOutput <- addSimpleBoolFlag "" ["suppress-output"] (flagHelp $ parDoc "suppress the regular output, i.e. the transformed haskell source")
  _verbosity <- addSimpleCountFlag "v" ["verbose"] (flagHelp $ parDoc "[currently without effect; TODO]")
  reorderStop
  desc <- peekCmdDesc
  addCmdImpl $ void $ do
    when printVersion $ do
      liftIO $ putStrLn $ "brittany version " ++ showVersion version
      System.Exit.exitSuccess
    when printHelp $ do
      liftIO $ print $ ppHelpShallow desc
      System.Exit.exitSuccess
    -- runGhc (Just libdir) $ do
    --   dynflags <- getDynFlags
    --   input <- liftIO $ readFile "local/Sample.hs"
    --   let parseOutput = runParser dynflags parserModule input
    --   liftIO $ case parseOutput of
    --     Failure msg strloc -> do
    --       putStrLn "some failed parse"
    --       putStrLn msg
    --       print strloc
    --     Parsed a -> putStrLn "some successful parse."
    --     Partial a (x,y) -> do
    --       putStrLn "some partial parse"
    --       print x
    --       print y
    inputPathM <- case inputPaths of
      [] -> do
        return Nothing
      [x] -> return $ Just x
      _ -> do
        liftIO $ putStrLn $ "more than one input, aborting"
        System.Exit.exitWith (System.Exit.ExitFailure 50)
    outputPath <- case outputPaths of
      [] -> do
        return Nothing
      [x] -> return $ Just x
      _ -> do
        liftIO $ putStrLn $ "more than one output, aborting"
        System.Exit.exitWith (System.Exit.ExitFailure 50)
    let configPath = maybe "brittany.yaml" id $ listToMaybe $ reverse configPaths
    config <- do
      may <- runMaybeT $ readMergePersConfig cmdlineConfig configPath
      case may of
        Nothing -> System.Exit.exitWith (System.Exit.ExitFailure 50)
        Just x -> return x
    when (runIdentity $ _dconf_dump_config $ _conf_debug $ config) $ do
      trace (showTree config) $ return ()
    liftIO $ do
      parseResult <- case inputPathM of
        Nothing -> ExactPrint.Parsers.parseModuleFromString "stdin"
                   =<< System.IO.hGetContents System.IO.stdin
        Just p -> ExactPrint.parseModule p
      case parseResult of
        Left left -> do
          putStrLn "parse error:"
          print left
          System.Exit.exitWith (System.Exit.ExitFailure 60)
        Right (anns, parsedSource) -> do
          when (config & _conf_debug .> _dconf_dump_ast_full .> runIdentity) $ do
            let val = printTreeWithCustom 100 (customLayouterF anns) parsedSource
            trace ("---- ast ----\n" ++ show val) $ return ()
          -- mapM_ print (Map.toList anns)
          -- let L _ (HsModule name exports imports decls _ _) = parsedSource
          -- let someDecls = take 3 decls
          -- -- let out = ExactPrint.exactPrint parsedSource anns
          -- let out = do
          --       decl <- someDecls
          --       ExactPrint.exactPrint decl anns
          let (errsWarns, outLText) = pPrintModule config anns parsedSource
          let customErrOrder LayoutWarning{}            = 0 :: Int
              customErrOrder LayoutErrorUnusedComment{} = 1
              customErrOrder LayoutErrorUnknownNode{}   = 2
          when (not $ null errsWarns) $ do
            let groupedErrsWarns = Data.List.Extra.groupOn customErrOrder
                                 $ List.sortOn customErrOrder
                                 $ errsWarns
            groupedErrsWarns `forM_` \case
              uns@(LayoutErrorUnknownNode{}:_) -> do
                putStrLn $ "ERROR: encountered unknown syntactical constructs:"
                uns `forM_` \case
                  LayoutErrorUnknownNode str ast -> do
                    putStrLn str
                    putStrLn $ "  " ++ show (astToDoc ast)
                  _ -> error "cannot happen (TM)"
              warns@(LayoutWarning{}:_) -> do
                putStrLn $ "WARNINGS:"
                warns `forM_` \case
                  LayoutWarning str -> putStrLn str
                  _ -> error "cannot happen (TM)"
              unused@(LayoutErrorUnusedComment{}:_) -> do
                putStrLn $ "Error: detected unprocessed comments. the transformation "
                        ++ "output will most likely not contain certain of the comments "
                        ++ "present in the input haskell source file."
                putStrLn $ "Affected are the following comments:"
                unused `forM_` \case
                  LayoutErrorUnusedComment str -> putStrLn str
                  _ -> error "cannot happen (TM)"
              [] -> error "cannot happen"
          -- TODO: don't output anything when there are errors unless user
          -- adds some override?
          let hasErrors = case config
                               & _conf_errorHandling
                               & _econf_Werror
                               & runIdentity of
                False -> 0 < maximum (-1 : fmap customErrOrder errsWarns)
                True  -> not $ null errsWarns
              outputOnErrs = config
                           & _conf_errorHandling
                           & _econf_produceOutputOnErrors
                           & runIdentity
          let shouldOutput = not suppressOutput
                          && (not hasErrors || outputOnErrs)

          when shouldOutput $ addTraceSep (_conf_debug config) $ case outputPath of
            Nothing -> TextL.IO.putStr     $ outLText
            Just p -> TextL.IO.writeFile p $ outLText

          when hasErrors $
            System.Exit.exitWith (System.Exit.ExitFailure 70)
  where
    addTraceSep conf = if foldr1 (||)
        [ runIdentity $ _dconf_dump_annotations conf
        , runIdentity $ _dconf_dump_ast_unknown conf
        , runIdentity $ _dconf_dump_ast_full conf
        , runIdentity $ _dconf_dump_bridoc_raw conf
        , runIdentity $ _dconf_dump_bridoc_simpl_alt conf
        , runIdentity $ _dconf_dump_bridoc_simpl_floating conf
        , runIdentity $ _dconf_dump_bridoc_simpl_columns conf
        , runIdentity $ _dconf_dump_bridoc_simpl_indent conf
        , runIdentity $ _dconf_dump_bridoc_final conf
        ]
      then trace "----"
      else id
