{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Main (main) where



import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import qualified Control.Monad.Reader.Class as Reader.Class
import qualified Control.Monad.RWS.Class as RWS.Class
import qualified Control.Monad.State.Class as State.Class
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.MultiRWS.Lazy as MultiRWSL
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.State.Lazy as StateL
import qualified Control.Monad.Trans.State.Strict as StateS
import qualified Control.Monad.Writer.Class as Writer.Class
import qualified Data.Bool as Bool
import qualified Data.ByteString
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Coerce
import qualified Data.Data
import qualified Data.Either
import qualified Data.Foldable
import qualified Data.Foldable as Foldable
import qualified Data.IntMap.Lazy as IntMapL
import qualified Data.IntMap.Strict as IntMapS
import qualified Data.List.Extra
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL.Encoding
import qualified Data.Text.Lazy.IO as TextL.IO
import qualified GHC.OldList as List
import qualified Safe as Safe
import qualified System.Directory
import qualified System.IO
import qualified Text.PrettyPrint
import qualified Text.PrettyPrint.Annotated
import qualified Text.PrettyPrint.Annotated.HughesPJ
import qualified Text.PrettyPrint.Annotated.HughesPJClass

-- brittany { lconfig_importAsColumn: 60, lconfig_importColumn: 60 }
import qualified Language.Haskell.GHC.ExactPrint         as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate
                                                         as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types   as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint.Parsers
import qualified Data.Map                                as Map
import qualified Data.Monoid

import           GHC                                      ( GenLocated(L) )
import           GHC.Utils.Outputable                               ( Outputable(..)
                                                          , showSDocUnsafe
                                                          )

import           Text.Read                                ( Read(..) )
import qualified Text.ParserCombinators.ReadP            as ReadP
import qualified Text.ParserCombinators.ReadPrec         as ReadPrec
import qualified Data.Text.Lazy.Builder                  as Text.Builder

import           Control.Monad                            ( zipWithM )
import           Data.CZipWith

import qualified Debug.Trace                             as Trace

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal
import           Language.Haskell.Brittany.Internal.Config
import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Utils
import           Language.Haskell.Brittany.Internal.Obfuscation

import qualified Text.PrettyPrint                        as PP

import           DataTreePrint
import           UI.Butcher.Monadic

import qualified System.Exit
import qualified System.Directory                        as Directory
import qualified System.FilePath.Posix                   as FilePath

import qualified GHC.Driver.Session                                as GHC
import qualified GHC.LanguageExtensions.Type             as GHC

import           Paths_brittany



data WriteMode = Display | Inplace

instance Read WriteMode where
  readPrec = val "display" Display <|> val "inplace" Inplace
    where val iden v = ReadPrec.lift $ ReadP.string iden >> return v

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
    , "Currently affects only the module head (imports/exports), type"
    , "signatures and function bindings;"
    , "everything else is left unmodified."
    , "Based on ghc-exactprint, thus (theoretically) supporting all"
    , "that ghc does."
    ]
  , parDoc $ "Example invocations:"
  , PP.hang (PP.text "") 2 $ PP.vcat
    [ PP.text "brittany"
    , PP.nest 2 $ PP.text "read from stdin, output to stdout"
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
    , "the output is syntactically valid and that no comments are removed."
    , "Nonetheless, this is a young project, and there will always be bugs,"
    , "and ensuring that the transformation never changes semantics of the"
    , "transformed source is currently not possible."
    , "Please do check the output and do not let brittany override your large"
    , "codebase without having backups."
    ]
  , parDoc $ "There is NO WARRANTY, to the extent permitted by law."
  , parDocW
    [ "This program is free software released under the AGPLv3."
    , "For details use the --license flag."
    ]
  , parDoc $ "See https://github.com/lspitzner/brittany"
  , parDoc
  $  "Please report bugs at"
  ++ " https://github.com/lspitzner/brittany/issues"
  ]

licenseDoc :: PP.Doc
licenseDoc = PP.vcat $ List.intersperse
  (PP.text "")
  [ parDoc $ "Copyright (C) 2016-2019 Lennart Spitzner"
  , parDoc $ "Copyright (C) 2019 PRODA LTD"
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
  printHelp    <- addSimpleBoolFlag "h" ["help"] mempty
  printVersion <- addSimpleBoolFlag "" ["version"] mempty
  printLicense <- addSimpleBoolFlag "" ["license"] mempty
  noUserConfig <- addSimpleBoolFlag "" ["no-user-config"] mempty
  configPaths  <- addFlagStringParams ""
                                      ["config-file"]
                                      "PATH"
                                      (flagHelpStr "path to config file") -- TODO: allow default on addFlagStringParam ?
  cmdlineConfig  <- cmdlineConfigParser
  suppressOutput <- addSimpleBoolFlag
    ""
    ["suppress-output"]
    (flagHelp $ parDoc
      "suppress the regular output, i.e. the transformed haskell source"
    )
  _verbosity <- addSimpleCountFlag
    "v"
    ["verbose"]
    (flagHelp $ parDoc "[currently without effect; TODO]")
  checkMode <- addSimpleBoolFlag
    "c"
    ["check-mode"]
    (flagHelp
      (PP.vcat
        [ PP.text "check for changes but do not write them out"
        , PP.text "exits with code 0 if no changes necessary, 1 otherwise"
        , PP.text "and print file path(s) of files that have changes to stdout"
        ]
      )
    )
  writeMode <- addFlagReadParam
    ""
    ["write-mode"]
    "(display|inplace)"
    (              flagHelp
        (PP.vcat
          [ PP.text "display: output for any input(s) goes to stdout"
          , PP.text "inplace: override respective input file (without backup!)"
          ]
        )
    Data.Monoid.<> flagDefault Display
    )
  inputParams <- addParamNoFlagStrings
    "PATH"
    (paramHelpStr "paths to input/inout haskell source files")
  reorderStop
  addCmdImpl $ void $ do
    when printLicense $ do
      print licenseDoc
      System.Exit.exitSuccess
    when printVersion $ do
      do
        putStrLn $ "brittany version " ++ showVersion version
        putStrLn $ "Copyright (C) 2016-2019 Lennart Spitzner"
        putStrLn $ "Copyright (C) 2019 PRODA LTD"
        putStrLn $ "There is NO WARRANTY, to the extent permitted by law."
      System.Exit.exitSuccess
    when printHelp $ do
      liftIO
        $ putStrLn
        $ PP.renderStyle PP.style { PP.ribbonsPerLine = 1.0 }
        $ ppHelpShallow helpDesc
      System.Exit.exitSuccess

    let inputPaths =
          if null inputParams then [Nothing] else map Just inputParams
    let outputPaths = case writeMode of
          Display -> repeat Nothing
          Inplace -> inputPaths

    configsToLoad <- liftIO $ if null configPaths
      then
        maybeToList <$> (Directory.getCurrentDirectory >>= findLocalConfigPath)
      else pure configPaths

    config <-
      runMaybeT
          (if noUserConfig
            then readConfigs cmdlineConfig configsToLoad
            else readConfigsWithUserConfig cmdlineConfig configsToLoad
          )
        >>= \case
              Nothing -> System.Exit.exitWith (System.Exit.ExitFailure 53)
              Just x  -> return x
    when (config & _conf_debug & _dconf_dump_config & confUnpack)
      $ trace (showConfigYaml config)
      $ return ()

    results <- zipWithM (coreIO putStrErrLn config suppressOutput checkMode)
                        inputPaths
                        outputPaths

    if checkMode
      then when (any (== Changes) (Data.Either.rights results))
        $ System.Exit.exitWith (System.Exit.ExitFailure 1)
      else case results of
        xs | all Data.Either.isRight xs -> pure ()
        [Left x] -> System.Exit.exitWith (System.Exit.ExitFailure x)
        _ -> System.Exit.exitWith (System.Exit.ExitFailure 1)


data ChangeStatus = Changes | NoChanges
  deriving (Eq)

-- | The main IO parts for the default mode of operation, and after commandline
-- and config stuff is processed.
coreIO
  :: (String -> IO ()) -- ^ error output function. In parallel operation, you
                       -- may want serialize the different outputs and
                       -- consequently not directly print to stderr.
  -> Config -- ^ global program config.
  -> Bool   -- ^ whether to supress output (to stdout). Purely IO flag, so
            -- currently not part of program config.
  -> Bool   -- ^ whether we are (just) in check mode.
  -> Maybe FilePath.FilePath -- ^ input filepath; stdin if Nothing.
  -> Maybe FilePath.FilePath -- ^ output filepath; stdout if Nothing.
  -> IO (Either Int ChangeStatus)      -- ^ Either an errorNo, or the change status.
coreIO putErrorLnIO config suppressOutput checkMode inputPathM outputPathM =
  ExceptT.runExceptT $ do
    let putErrorLn = liftIO . putErrorLnIO :: String -> ExceptT.ExceptT e IO ()
    let ghcOptions = config & _conf_forward & _options_ghc & runIdentity
    -- there is a good of code duplication between the following code and the
    -- `pureModuleTransform` function. Unfortunately, there are also a good
    -- amount of slight differences: This module is a bit more verbose, and
    -- it tries to use the full-blown `parseModule` function which supports
    -- CPP (but requires the input to be a file..).
    let cppMode    = config & _conf_preprocessor & _ppconf_CPPMode & confUnpack
    -- the flag will do the following: insert a marker string
    -- ("-- BRITANY_INCLUDE_HACK ") right before any lines starting with
    -- "#include" before processing (parsing) input; and remove that marker
    -- string from the transformation output.
    -- The flag is intentionally misspelled to prevent clashing with
    -- inline-config stuff.
    let hackAroundIncludes =
          config & _conf_preprocessor & _ppconf_hackAroundIncludes & confUnpack
    let exactprintOnly = viaGlobal || viaDebug
         where
          viaGlobal = config & _conf_roundtrip_exactprint_only & confUnpack
          viaDebug =
            config & _conf_debug & _dconf_roundtrip_exactprint_only & confUnpack

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
    (parseResult, originalContents) <- case inputPathM of
      Nothing -> do
        -- TODO: refactor this hack to not be mixed into parsing logic
        let hackF s = if "#include" `isPrefixOf` s
              then "-- BRITANY_INCLUDE_HACK " ++ s
              else s
        let hackTransform = if hackAroundIncludes && not exactprintOnly
              then List.intercalate "\n" . fmap hackF . lines'
              else id
        inputString <- liftIO $ System.IO.hGetContents System.IO.stdin
        parseRes <- liftIO $ parseModuleFromString ghcOptions
                                                   "stdin"
                                                   cppCheckFunc
                                                   (hackTransform inputString)
        return (parseRes, Text.pack inputString)
      Just p -> liftIO $ do
        parseRes  <- parseModule ghcOptions p cppCheckFunc
        inputText <- Text.IO.readFile p
        -- The above means we read the file twice, but the
        -- GHC API does not really expose the source it
        -- read. Should be in cache still anyways.
        --
        -- We do not use TextL.IO.readFile because lazy IO is evil.
        -- (not identical -> read is not finished ->
        -- handle still open -> write below crashes - evil.)
        return (parseRes, inputText)
    case parseResult of
      Left left -> do
        putErrorLn "parse error:"
        putErrorLn left
        ExceptT.throwE 60
      Right (anns, parsedSource, hasCPP) -> do
        (inlineConf, perItemConf) <-
          case
            extractCommentConfigs anns (getTopLevelDeclNameMap parsedSource)
          of
            Left (err, input) -> do
              putErrorLn $ "Error: parse error in inline configuration:"
              putErrorLn err
              putErrorLn $ "  in the string \"" ++ input ++ "\"."
              ExceptT.throwE 61
            Right c -> -- trace (showTree c) $
              pure c
        let moduleConf = cZipWith fromOptionIdentity config inlineConf
        when (config & _conf_debug & _dconf_dump_ast_full & confUnpack) $ do
          let val = printTreeWithCustom 100 (customLayouterF anns) parsedSource
          trace ("---- ast ----\n" ++ show val) $ return ()
        let disableFormatting =
              moduleConf & _conf_disable_formatting & confUnpack
        (errsWarns, outSText, hasChanges) <- do
          if
            | disableFormatting -> do
              pure ([], originalContents, False)
            | exactprintOnly -> do
              let r = Text.pack $ ExactPrint.exactPrint parsedSource anns
              pure ([], r, r /= originalContents)
            | otherwise -> do
              let omitCheck =
                    moduleConf
                      &  _conf_errorHandling
                      .> _econf_omit_output_valid_check
                      .> confUnpack
              (ews, outRaw) <- if hasCPP || omitCheck
                then return
                  $ pPrintModule moduleConf perItemConf anns parsedSource
                else liftIO $ pPrintModuleAndCheck moduleConf
                                                   perItemConf
                                                   anns
                                                   parsedSource
              let hackF s = fromMaybe s $ TextL.stripPrefix
                    (TextL.pack "-- BRITANY_INCLUDE_HACK ")
                    s
              let out = TextL.toStrict $ if hackAroundIncludes
                    then
                      TextL.intercalate (TextL.pack "\n")
                      $ fmap hackF
                      $ TextL.splitOn (TextL.pack "\n") outRaw
                    else outRaw
              out' <- if moduleConf & _conf_obfuscate & confUnpack
                then lift $ obfuscate out
                else pure out
              pure $ (ews, out', out' /= originalContents)
        let customErrOrder ErrorInput{}         = 4
            customErrOrder LayoutWarning{}      = -1 :: Int
            customErrOrder ErrorOutputCheck{}   = 1
            customErrOrder ErrorUnusedComment{} = 2
            customErrOrder ErrorUnknownNode{}   = -2 :: Int
            customErrOrder ErrorMacroConfig{}   = 5
        when (not $ null errsWarns) $ do
          let groupedErrsWarns =
                Data.List.Extra.groupOn customErrOrder
                  $ List.sortOn customErrOrder
                  $ errsWarns
          groupedErrsWarns `forM_` \case
            (ErrorOutputCheck{} : _) -> do
              putErrorLn
                $  "ERROR: brittany pretty printer"
                ++ " returned syntactically invalid result."
            (ErrorInput str : _) -> do
              putErrorLn $ "ERROR: parse error: " ++ str
            uns@(ErrorUnknownNode{} : _) -> do
              putErrorLn
                $ "WARNING: encountered unknown syntactical constructs:"
              uns `forM_` \case
                ErrorUnknownNode str ast@(L loc _) -> do
                  putErrorLn $ "  " <> str <> " at " <> showSDocUnsafe (ppr loc)
                  when
                      ( config
                      & _conf_debug
                      & _dconf_dump_ast_unknown
                      & confUnpack
                      )
                    $ do
                        putErrorLn $ "  " ++ show (astToDoc ast)
                _ -> error "cannot happen (TM)"
              putErrorLn
                "  -> falling back on exactprint for this element of the module"
            warns@(LayoutWarning{} : _) -> do
              putErrorLn $ "WARNINGS:"
              warns `forM_` \case
                LayoutWarning str -> putErrorLn str
                _                 -> error "cannot happen (TM)"
            unused@(ErrorUnusedComment{} : _) -> do
              putErrorLn
                $  "Error: detected unprocessed comments."
                ++ " The transformation output will most likely"
                ++ " not contain some of the comments"
                ++ " present in the input haskell source file."
              putErrorLn $ "Affected are the following comments:"
              unused `forM_` \case
                ErrorUnusedComment str -> putErrorLn str
                _                      -> error "cannot happen (TM)"
            (ErrorMacroConfig err input : _) -> do
              putErrorLn $ "Error: parse error in inline configuration:"
              putErrorLn err
              putErrorLn $ "  in the string \"" ++ input ++ "\"."
            [] -> error "cannot happen"
        -- TODO: don't output anything when there are errors unless user
        -- adds some override?
        let
          hasErrors =
            case config & _conf_errorHandling & _econf_Werror & confUnpack of
              False -> 0 < maximum (-1 : fmap customErrOrder errsWarns)
              True  -> not $ null errsWarns
          outputOnErrs =
            config
              & _conf_errorHandling
              & _econf_produceOutputOnErrors
              & confUnpack
          shouldOutput =
            not suppressOutput
              && not checkMode
              && (not hasErrors || outputOnErrs)

        when shouldOutput
          $ addTraceSep (_conf_debug config)
          $ case outputPathM of
              Nothing -> liftIO $ Text.IO.putStr $ outSText
              Just p  -> liftIO $ do
                let isIdentical = case inputPathM of
                      Nothing -> False
                      Just _  -> not hasChanges
                unless isIdentical $ Text.IO.writeFile p $ outSText

        when (checkMode && hasChanges) $ case inputPathM of
          Nothing -> pure ()
          Just p -> liftIO $ putStrLn $ "formatting would modify: " ++ p

        when hasErrors $ ExceptT.throwE 70
        return (if hasChanges then Changes else NoChanges)
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
