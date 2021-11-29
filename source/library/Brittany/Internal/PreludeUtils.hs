{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brittany.Internal.PreludeUtils where

import Control.Applicative
import Control.DeepSeq (NFData, force)
import Control.Exception.Base (evaluate)
import Control.Monad
import qualified Data.Strict.Maybe as Strict
import Debug.Trace
import Prelude
import System.IO



instance Applicative Strict.Maybe where
  pure = Strict.Just
  Strict.Just f <*> Strict.Just x = Strict.Just (f x)
  _ <*> _ = Strict.Nothing

instance Monad Strict.Maybe where
  Strict.Nothing >>= _ = Strict.Nothing
  Strict.Just x >>= f = f x

instance Alternative Strict.Maybe where
  empty = Strict.Nothing
  x <|> Strict.Nothing = x
  _ <|> x = x

traceFunctionWith
  :: String -> (a -> String) -> (b -> String) -> (a -> b) -> (a -> b)
traceFunctionWith name s1 s2 f x = trace traceStr y
 where
  y = f x
  traceStr = name ++ "\nBEFORE:\n" ++ s1 x ++ "\nAFTER:\n" ++ s2 y

(<&!>) :: Monad m => m a -> (a -> b) -> m b
(<&!>) = flip (<$!>)

putStrErrLn :: String -> IO ()
putStrErrLn s = hPutStrLn stderr s

putStrErr :: String -> IO ()
putStrErr s = hPutStr stderr s

printErr :: Show a => a -> IO ()
printErr = putStrErrLn . show

errorIf :: Bool -> a -> a
errorIf False = id
errorIf True = error "errorIf"

errorIfNote :: Maybe String -> a -> a
errorIfNote Nothing = id
errorIfNote (Just x) = error x

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 4 <&>

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = g . f
infixl 9 .>

evaluateDeep :: NFData a => a -> IO a
evaluateDeep = evaluate . force
