{-# LANGUAGE QuasiQuotes #-}

module Main where



#include "prelude.inc"

import Test.Hspec

import NeatInterpolation

import qualified Text.Parsec as Parsec
import Text.Parsec.Text ( Parser )

import Data.Char ( isSpace )
import Data.List ( groupBy )

import Language.Haskell.Brittany.Internal

import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Config

import Data.Coerce ( coerce )

import qualified Data.Text.IO as Text.IO
import           System.FilePath ( (</>) )



data InputLine
  = GroupLine Text
  | HeaderLine Text
  | PendingLine
  | NormalLine Text
  | CommentLine
  deriving Show


main :: IO ()
main = do
  files <- System.Directory.listDirectory "src-literatetests/"
  let blts =
        List.sort
          $ filter (\x -> not ("tests-context-free.blt" `isSuffixOf` x))
          $ filter (".blt"`isSuffixOf`) files
  inputs <- blts `forM` \blt -> Text.IO.readFile ("src-literatetests" </> blt)
  let groups = createChunks =<< inputs
  inputCtxFree <- Text.IO.readFile "src-literatetests/tests-context-free.blt"
  let groupsCtxFree = createChunks inputCtxFree
  hspec $ do
    groups `forM_` \(groupname, tests) -> do
      describe (Text.unpack groupname) $ tests `forM_` \(name, pend, inp) -> do
        (if pend then before_ pending else id)
          $ it (Text.unpack name)
          $ roundTripEqual defaultTestConfig inp
    groupsCtxFree `forM_` \(groupname, tests) -> do
      describe ("context free: " ++ Text.unpack groupname)
        $       tests
        `forM_` \(name, pend, inp) -> do
                  (if pend then before_ pending else id)
                    $ it (Text.unpack name)
                    $ roundTripEqual contextFreeTestConfig inp
 where
  -- this function might be implemented in a weirdly complex fashion; the
  -- reason being that it was copied from a somewhat more complex variant.
  createChunks :: Text -> [(Text, [(Text, Bool, Text)])]
  createChunks input =
--    fmap (\case
--        HeaderLine n:PendingLine:rest | Just rlines <- mapM extractNormal rest -> (n, True, Text.unlines rlines)
--        HeaderLine n:rest | Just rlines <- mapM extractNormal rest -> (n, False, Text.unlines rlines)
--        l -> error $ "first non-empty line must start with #test footest\n" ++ show l
--      )
--      $   fmap (groupBy grouperT)
    fmap
        ( \case
          GroupLine g:grouprest ->
            (,) g
              $ fmap
                  ( \case
                    HeaderLine n:PendingLine:rest | Just rlines <- mapM
                      extractNormal
                      rest ->
                      (n, True, Text.unlines rlines)
                    HeaderLine n:rest | Just rlines <- mapM extractNormal rest ->
                      (n, False, Text.unlines rlines)
                    l ->
                      error
                        $ "first non-empty line must start with #test footest\n"
                        ++ show l
                  )
              $ groupBy grouperT
              $ filter (not . lineIsSpace)
              $ grouprest
          l -> error $ "first non-empty line must be a #group\n" ++ show l
        )
      $   groupBy grouperG
      $   filter (not . lineIsSpace)
      $   lineMapper
      <$> Text.lines input
   where
    extractNormal (NormalLine l) = Just l
    extractNormal _              = Nothing
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
      , [ PendingLine
        | _ <- Parsec.try $ Parsec.string "#pending"
        , _ <- Parsec.optional $ many (Parsec.noneOf "\r\n")
        , _ <- Parsec.eof
        ]
      , [ CommentLine
        | _ <- Parsec.many $ Parsec.oneOf " \t"
        , _ <-
          Parsec.optional $ Parsec.string "##" <* many (Parsec.noneOf "\r\n")
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
  fmap (fmap PPTextWrapper)
       (parsePrintModuleTests c "TestFakeFileName.hs" t)
    `shouldReturn` Right (PPTextWrapper t)

newtype PPTextWrapper = PPTextWrapper Text
  deriving Eq

instance Show PPTextWrapper where
  show (PPTextWrapper t) = "\n" ++ Text.unpack t


defaultTestConfig :: Config
defaultTestConfig = Config
  { _conf_version = _conf_version staticDefaultConfig
  , _conf_debug   = _conf_debug staticDefaultConfig
  , _conf_layout  = LayoutConfig
    { _lconfig_cols                      = coerce (80 :: Int)
    , _lconfig_indentPolicy              = coerce IndentPolicyFree
    , _lconfig_indentAmount              = coerce (2 :: Int)
    , _lconfig_indentWhereSpecial        = coerce True
    , _lconfig_indentListSpecial         = coerce True
    , _lconfig_importColumn              = coerce (60 :: Int)
    , _lconfig_altChooser                = coerce $ AltChooserBoundedSearch 3
    , _lconfig_columnAlignMode           = coerce (ColumnAlignModeMajority 0.7)
    , _lconfig_alignmentLimit            = coerce (30 :: Int)
    , _lconfig_alignmentBreakOnMultiline = coerce True
    , _lconfig_hangingTypeSignature      = coerce False
    }
  , _conf_errorHandling = (_conf_errorHandling staticDefaultConfig)
    { _econf_omit_output_valid_check = coerce True
    }
  , _conf_preprocessor = _conf_preprocessor staticDefaultConfig
  , _conf_forward      = ForwardOptions
    { _options_ghc = Identity []
    }
  }

contextFreeTestConfig :: Config
contextFreeTestConfig =
  defaultTestConfig
  { _conf_layout = (_conf_layout defaultTestConfig)
    {_lconfig_indentPolicy = coerce IndentPolicyLeft
    ,_lconfig_alignmentLimit = coerce (1 :: Int)
    ,_lconfig_columnAlignMode = coerce ColumnAlignModeDisabled
    }
  }
