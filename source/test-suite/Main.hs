import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Language.Haskell.Brittany.Main as Brittany
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.parallel $ do
  let directory = "data"
  entries <- Hspec.runIO $ Directory.listDirectory directory
  Monad.forM_ (List.sort entries) $ \entry ->
    case FilePath.stripExtension "hs" entry of
      Nothing -> pure ()
      Just slug -> Hspec.it slug $ do
        let input = FilePath.combine directory entry
        expected <- readFile input
        let output = FilePath.combine "output" entry
        Directory.copyFile input output
        Brittany.mainWith
          "brittany"
          [ "--config-file"
          , FilePath.combine directory "brittany.yaml"
          , "--no-user-config"
          , "--write-mode"
          , "inplace"
          , output
          ]
        actual <- readFile output
        Literal actual `Hspec.shouldBe` Literal expected

newtype Literal
  = Literal String
  deriving Eq

instance Show Literal where
  show (Literal x) = x
