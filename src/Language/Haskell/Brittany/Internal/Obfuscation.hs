module Language.Haskell.Brittany.Internal.Obfuscation
  ( obfuscate
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

import           Data.Char
import           System.Random



obfuscate :: Text -> IO Text
obfuscate input = do
  let predi x = isAlphaNum x || x `elem` "_'"
  let groups = List.groupBy (\a b -> predi a && predi b) (Text.unpack input)
  let idents = Set.toList $ Set.fromList $ filter (all predi) groups
  let exceptionFilter x | x `elem` keywords = False
      exceptionFilter x | x `elem` extraKWs = False
      exceptionFilter x                     = not $ null $ drop 1 x
  let filtered = filter exceptionFilter idents
  mappings <- fmap Map.fromList $ filtered `forM` \x -> do
    r <- createAlias x
    pure (x, r)
  let groups' = groups <&> \w -> fromMaybe w (Map.lookup w mappings)
  pure $ Text.concat $ fmap Text.pack groups'

keywords :: [String]
keywords =
  [ "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "mdo"
  , "else"
  , "forall"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "qualified"
  , "then"
  , "type"
  , "where"
  , "_"
  , "foreign"
  , "ccall"
  , "as"
  , "safe"
  , "unsafe"
  , "hiding"
  , "proc"
  , "rec"
  , "family"
  ]

extraKWs :: [String]
extraKWs = ["return", "pure", "Int", "True", "False", "otherwise"]

createAlias :: String -> IO String
createAlias xs = go NoHint xs
 where
  go _hint ""       = pure ""
  go hint (c : cr)  = do
    c' <- case hint of
      VocalHint | isUpper c -> randomFrom $ "AAAEEEOOOIIIUUU" ++ ['A' .. 'Z']
      _ | isUpper c         -> randomFrom ['A' .. 'Z']
      VocalHint | isLower c -> randomFrom $ "aaaeeeoooiiiuuu" ++ ['a' .. 'z']
      _ | isLower c         -> randomFrom ['a' .. 'z']
      _                     -> pure c
    cr' <- go (if c' `elem` "aeuioAEUIO" then NoVocalHint else VocalHint) cr
    pure (c' : cr')

data Hint = NoHint | VocalHint | NoVocalHint

_randomRange :: Random a => a -> a -> IO a
_randomRange lo hi = do
  gen <- getStdGen
  let (x, gen') = randomR (lo, hi) gen
  setStdGen gen'
  pure x

randomFrom :: Random a => [a] -> IO a
randomFrom l = do
  let hi = length l - 1
  gen <- getStdGen
  let (x, gen') = randomR (0, hi) gen
  setStdGen gen'
  pure $ l List.!! x
