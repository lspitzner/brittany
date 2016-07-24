module Language.Haskell.Brittany.Prelude
where



import Prelude
import qualified Data.Strict.Maybe as Strict
import Debug.Trace



instance Applicative Strict.Maybe where
  pure = Strict.Just
  Strict.Just f <*> Strict.Just x = Strict.Just (f x)
  _ <*> _ = Strict.Nothing

instance Monad Strict.Maybe where
  return = Strict.Just
  Strict.Nothing >>= _ = Strict.Nothing
  Strict.Just x >>= f = f x

traceFunctionWith
  :: String -> (a -> String) -> (b -> String) -> (a -> b) -> (a -> b)
traceFunctionWith name s1 s2 f x =
  trace traceStr y
  where
    y = f x
    traceStr = name ++ "\nBEFORE:\n" ++ s1 x ++ "\nAFTER:\n" ++ s2 y
