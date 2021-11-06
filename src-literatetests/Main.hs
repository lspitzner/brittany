module Main
  ( main
  )
where



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

import           Test.Hspec
import           Test.Hspec.Runner              ( hspecWith
                                                , defaultConfig
                                                , configConcurrentJobs
                                                )

import qualified Text.Parsec                   as Parsec
import           Text.Parsec.Text               ( Parser )

import           Data.Char                      ( isSpace )
import           Data.List                      ( groupBy )

import           Language.Haskell.Brittany.Internal

import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Config

import           Data.Coerce                    ( coerce )

import qualified Data.Text.IO                  as Text.IO
import           System.FilePath                ( (</>) )



data InputLine
  = GroupLine Text
  | HeaderLine Text
  | GhcVersionGuardLine Text
  | PendingLine
  | NormalLine Text
  | CommentLine
  deriving Show

data TestCase = TestCase
  { testName :: Text
  , isPending :: Bool
  , minGHCVersion :: Maybe Text
  , content :: Text
  }

main :: IO ()
main = do
  files <- System.Directory.listDirectory "src-literatetests/"
  let blts =
        List.sort
          $ filter (\x -> not ("tests-context-free.blt" `isSuffixOf` x))
          $ filter (".blt" `isSuffixOf`) files
  inputs <- blts `forM` \blt -> Text.IO.readFile ("src-literatetests" </> blt)
  let groups = createChunks =<< inputs
  inputCtxFree <- Text.IO.readFile "src-literatetests/30-tests-context-free.blt"
  let groupsCtxFree = createChunks inputCtxFree
  let parseVersion :: Text -> Maybe [Int]
      parseVersion =
        mapM (readMaybe . Text.unpack) . Text.splitOn (Text.pack ".")
  let ghcVersion = Data.Maybe.fromJust $ parseVersion $ Text.pack VERSION_ghc
  let checkVersion = \case
        Nothing -> True -- no version constraint
        Just s  -> case parseVersion s of
          Nothing -> error $ "could not parse version " ++ Text.unpack s
          Just v  -> v <= ghcVersion
  hspec $ do
    groups `forM_` \(groupname, tests) -> do
      describe (Text.unpack groupname) $ do
        tests `forM_` \test -> when (checkVersion $ minGHCVersion test) $ do
          (if isPending test then before_ pending else id)
            $ it (Text.unpack $ testName test)
            $ roundTripEqual defaultTestConfig
            $ content test
    groupsCtxFree `forM_` \(groupname, tests) -> do
      describe ("context free: " ++ Text.unpack groupname) $ do
        tests `forM_` \test -> when (checkVersion $ minGHCVersion test) $ do
          (if isPending test then before_ pending else id)
            $ it (Text.unpack $ testName test)
            $ roundTripEqual contextFreeTestConfig
            $ content test
 where
  -- this function might be implemented in a weirdly complex fashion; the
  -- reason being that it was copied from a somewhat more complex variant.
  createChunks :: Text -> [(Text, [TestCase])]
  createChunks input =
--    fmap (\case
--        HeaderLine n:PendingLine:rest | Just rlines <- mapM extractNormal rest -> (n, True, Text.unlines rlines)
--        HeaderLine n:rest | Just rlines <- mapM extractNormal rest -> (n, False, Text.unlines rlines)
--        l -> error $ "first non-empty line must start with #test footest\n" ++ show l
--      )
--      $   fmap (groupBy grouperT)
    fmap groupProcessor
      $ groupBy grouperG
      $ filter (not . lineIsSpace)
      $ fmap lineMapper
      $ Text.lines input
   where
    groupProcessor :: [InputLine] -> (Text, [TestCase])
    groupProcessor = \case
      GroupLine g : grouprest ->
        (,) g
          $ fmap testProcessor
          $ groupBy grouperT
          $ filter (not . lineIsSpace)
          $ grouprest
      l -> error $ "first non-empty line must be a #group\n" ++ show l
    testProcessor :: [InputLine] -> TestCase
    testProcessor = \case
      HeaderLine n : rest ->
        let normalLines = Data.Maybe.mapMaybe extractNormal rest
        in  TestCase
              { testName      = n
              , isPending     = any isPendingLine rest
              , minGHCVersion = Data.List.Extra.firstJust extractMinGhc rest
              , content       = Text.unlines normalLines
              }
      l ->
        error $ "first non-empty line must start with #test footest\n" ++ show l
    extractNormal (NormalLine l) = Just l
    extractNormal _              = Nothing
    extractMinGhc (GhcVersionGuardLine v) = Just v
    extractMinGhc _                       = Nothing
    isPendingLine PendingLine{} = True
    isPendingLine _             = False
    specialLineParser :: Parser InputLine
    specialLineParser = Parsec.choice
      [ [ GroupLine $ Text.pack name
        | _    <- Parsec.try $ Parsec.string "#group"
        , _    <- Parsec.many1 $ Parsec.oneOf " \t"
        , name <- Parsec.many1 $ Parsec.noneOf "\r\n:"
        , _    <- Parsec.eof
        ]
      , [ HeaderLine $ Text.pack name
        | _    <- Parsec.try $ Parsec.string "#test"
        , _    <- Parsec.many1 $ Parsec.oneOf " \t"
        , name <- Parsec.many1 $ Parsec.noneOf "\r\n:"
        , _    <- Parsec.eof
        ]
      , [ GhcVersionGuardLine $ Text.pack version
        | _       <- Parsec.try $ Parsec.string "#min-ghc"
        , _       <- Parsec.many1 $ Parsec.oneOf " \t"
        , version <- Parsec.many1 $ Parsec.noneOf "\r\n:"
        ]
      , [ PendingLine
        | _ <- Parsec.try $ Parsec.string "#pending"
        , _ <- Parsec.optional $ many (Parsec.noneOf "\r\n")
        , _ <- Parsec.eof
        ]
      , [ CommentLine
        | _ <- Parsec.many $ Parsec.oneOf " \t"
        , _ <- Parsec.optional $ Parsec.string "##" <* many
          (Parsec.noneOf "\r\n")
        , _ <- Parsec.eof
        ]
      , [ NormalLine mempty
        | _ <- Parsec.try $ Parsec.string "<BLANKLINE>"
        , _ <- Parsec.eof
        ]
      ]
    lineMapper :: Text -> InputLine
    lineMapper line = case Parsec.runParser specialLineParser () "" line of
      Left  _e -> NormalLine line
      Right l  -> l
    lineIsSpace :: InputLine -> Bool
    lineIsSpace CommentLine = True
    lineIsSpace _           = False
    grouperG :: InputLine -> InputLine -> Bool
    grouperG _ GroupLine{} = False
    grouperG _ _           = True
    grouperT :: InputLine -> InputLine -> Bool
    grouperT _ HeaderLine{} = False
    grouperT _ _            = True


--------------------
-- past this line:  copy-pasta from other test (meh..)
--------------------
roundTripEqual :: Config -> Text -> Expectation
roundTripEqual c t =
  fmap (fmap PPTextWrapper) (parsePrintModuleTests c "TestFakeFileName.hs" t)
    `shouldReturn` Right (PPTextWrapper t)

newtype PPTextWrapper = PPTextWrapper Text
  deriving Eq

instance Show PPTextWrapper where
  show (PPTextWrapper t) = "\n" ++ Text.unpack t

-- brittany-next-binding --columns 160
-- brittany-next-binding { lconfig_indentPolicy: IndentPolicyLeft }
defaultTestConfig :: Config
defaultTestConfig = Config
  { _conf_version                   = _conf_version staticDefaultConfig
  , _conf_debug                     = _conf_debug staticDefaultConfig
  , _conf_layout                    = LayoutConfig
    { _lconfig_cols                      = coerce (80 :: Int)
    , _lconfig_indentPolicy              = coerce IndentPolicyFree
    , _lconfig_indentAmount              = coerce (2 :: Int)
    , _lconfig_indentWhereSpecial        = coerce True
    , _lconfig_indentListSpecial         = coerce True
    , _lconfig_importColumn              = coerce (60 :: Int)
    , _lconfig_importAsColumn            = coerce (60 :: Int)
    , _lconfig_altChooser                = coerce $ AltChooserBoundedSearch 3
    , _lconfig_columnAlignMode           = coerce (ColumnAlignModeMajority 0.7)
    , _lconfig_alignmentLimit            = coerce (30 :: Int)
    , _lconfig_alignmentBreakOnMultiline = coerce True
    , _lconfig_hangingTypeSignature      = coerce False
    , _lconfig_reformatModulePreamble    = coerce True
    , _lconfig_allowSingleLineExportList = coerce True
    , _lconfig_allowHangingQuasiQuotes   = coerce True
    , _lconfig_experimentalSemicolonNewlines = coerce False
    -- , _lconfig_allowSinglelineRecord     = coerce False
    }
  , _conf_errorHandling             = (_conf_errorHandling staticDefaultConfig) { _econf_omit_output_valid_check = coerce True }
  , _conf_preprocessor              = _conf_preprocessor staticDefaultConfig
  , _conf_forward                   = ForwardOptions { _options_ghc = Identity [] }
  , _conf_roundtrip_exactprint_only = coerce False
  , _conf_disable_formatting        = coerce False
  , _conf_obfuscate                 = coerce False
  }

contextFreeTestConfig :: Config
contextFreeTestConfig = defaultTestConfig
  { _conf_layout = (_conf_layout defaultTestConfig)
                     { _lconfig_indentPolicy    = coerce IndentPolicyLeft
                     , _lconfig_alignmentLimit  = coerce (1 :: Int)
                     , _lconfig_columnAlignMode = coerce ColumnAlignModeDisabled
                     }
  }
