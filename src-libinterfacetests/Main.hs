module Main where



import           Test.Hspec
import           Language.Haskell.Brittany
import qualified Data.Text as Text
import qualified System.Exit as Exit
import           Control.Monad.IO.Class



main :: IO ()
main = hspec $ do
  describe "library interface basic functionality" $ do
    it "gives properly formatted result for valid input" $ do
      let
        input = Text.pack $ unlines
          ["func = [00000000000000000000000, 00000000000000000000000, 00000000000000000000000, 00000000000000000000000]"]
      let expected = Text.pack $ unlines
            [ "func ="
            , "  [ 00000000000000000000000"
            , "  , 00000000000000000000000"
            , "  , 00000000000000000000000"
            , "  , 00000000000000000000000"
            , "  ]"
            ]
      output <- liftIO $ parsePrintModule staticDefaultConfig input
      input `shouldSatisfy` \_ -> case output of
        Right x | x == expected -> True
        _                       -> False

