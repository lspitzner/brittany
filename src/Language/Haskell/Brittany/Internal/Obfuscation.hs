module Language.Haskell.Brittany.Internal.Obfuscation
  ( obfuscate
  )
where



#include "prelude.inc"

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
