{-# LANGUAGE QuasiQuotes #-}

module Main where



#include "prelude.inc"

import Test.Hspec

import NeatInterpolation

import qualified Text.Parsec as Parsec
import Text.Parsec.Text ( Parser )

import Data.Char ( isSpace )
import Data.List ( groupBy )

import Language.Haskell.Brittany

import Language.Haskell.Brittany.Config.Types

import Data.Coerce ( coerce )

import qualified Data.Text.IO as Text.IO



data InputLine
  = GroupLine Text
  | HeaderLine Text
  | PendingLine
  | NormalLine Text
  | CommentLine
  deriving Show


main :: IO ()
main = do
  input <- Text.IO.readFile "src-literatetests/tests.blt"
  let groups = createChunks input
  hspec $ groups `forM_` \(groupname, tests) -> do
    describe (Text.unpack groupname) $ tests `forM_` \(name, pend, inp) -> do
      (if pend then before_ pending else id)
        $ it (Text.unpack name) $ roundTripEqual inp

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
    fmap (\case
          GroupLine g:grouprest ->
            (,) g
            $ fmap (\case
                HeaderLine n:PendingLine:rest | Just rlines <- mapM extractNormal rest -> (n, True, Text.unlines rlines)
                HeaderLine n:rest | Just rlines <- mapM extractNormal rest -> (n, False, Text.unlines rlines)
                l -> error $ "first non-empty line must start with #test footest\n" ++ show l
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
    extractNormal _ = Nothing
    specialLineParser :: Parser InputLine
    specialLineParser = Parsec.choice
      [ [ GroupLine $ Text.pack name
        | _      <- Parsec.try $ Parsec.string "#group"
        , _      <- Parsec.many1 $ Parsec.oneOf " \t"
        , name   <- Parsec.many1 $ Parsec.noneOf "\r\n:"
        , _      <- Parsec.eof
        ]
      , [ HeaderLine $ Text.pack name
        | _      <- Parsec.try $ Parsec.string "#test"
        , _      <- Parsec.many1 $ Parsec.oneOf " \t"
        , name   <- Parsec.many1 $ Parsec.noneOf "\r\n:"
        , _      <- Parsec.eof
        ]
      , [ PendingLine
        | _      <- Parsec.try $ Parsec.string "#pending"
        , _      <- Parsec.optional $ many (Parsec.noneOf "\r\n")
        , _      <- Parsec.eof
        ]
      , [ CommentLine
        | _ <- Parsec.many $ Parsec.oneOf " \t"
        , _ <- Parsec.optional $ Parsec.string "##" <* many (Parsec.noneOf "\r\n")
        , _ <- Parsec.eof
        ]
      ]
    lineMapper :: Text -> InputLine
    lineMapper line = case Parsec.runParser specialLineParser () "" line of
      Left  _e -> NormalLine line
      Right l -> l
    lineIsSpace :: InputLine -> Bool
    lineIsSpace CommentLine = True
    lineIsSpace _ = False
    grouperG :: InputLine -> InputLine -> Bool
    grouperG _ GroupLine{} = False
    grouperG _ _           = True
    grouperT :: InputLine -> InputLine -> Bool
    grouperT _ HeaderLine{} = False
    grouperT _ _            = True


--------------------
-- past this line:  copy-pasta from other test (meh..)
--------------------
roundTripEqual :: Text -> Expectation
roundTripEqual t =
  fmap (fmap PPTextWrapper)
       (parsePrintModule defaultTestConfig "TestFakeFileName.hs" t)
    `shouldReturn` Right (PPTextWrapper t)

newtype PPTextWrapper = PPTextWrapper Text
  deriving Eq

instance Show PPTextWrapper where
  show (PPTextWrapper t) = "\n" ++ Text.unpack t


defaultTestConfig :: Config
defaultTestConfig = Config
  { _conf_debug         = _conf_debug staticDefaultConfig
  , _conf_layout        = LayoutConfig
    { _lconfig_cols               = coerce (80 :: Int)
    , _lconfig_indentPolicy       = coerce IndentPolicyFree
    , _lconfig_indentAmount       = coerce (2 :: Int)
    , _lconfig_indentWhereSpecial = coerce True
    , _lconfig_indentListSpecial  = coerce True
    , _lconfig_importColumn       = coerce (60 :: Int)
    , _lconfig_altChooser         = coerce $ AltChooserBoundedSearch 3
    , _lconfig_columnAlignMode    = coerce (ColumnAlignModeMajority 0.7)
    }
  , _conf_errorHandling = _conf_errorHandling staticDefaultConfig
  , _conf_forward       = ForwardOptions
    { _options_ghc = Identity []
    }
  }

