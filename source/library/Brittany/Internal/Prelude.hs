module Brittany.Internal.Prelude
  ( module E
  ) where

import Control.Applicative as E (Alternative(..), Applicative(..))
import Control.Arrow as E ((&&&), (***), (<<<), (>>>), first, second)
import Control.Concurrent as E (forkIO, forkOS, threadDelay)
import Control.Concurrent.Chan as E (Chan)
import Control.Concurrent.MVar as E
  (MVar, newEmptyMVar, newMVar, putMVar, readMVar, swapMVar, takeMVar)
import Control.Exception as E (assert, bracket, evaluate)
import Control.Monad as E
  ( (<$!>)
  , (<=<)
  , (=<<)
  , (>=>)
  , Functor(..)
  , Monad(..)
  , MonadPlus(..)
  , filterM
  , forM
  , forM_
  , forever
  , guard
  , join
  , liftM
  , liftM2
  , liftM3
  , liftM4
  , liftM5
  , mapM
  , mapM_
  , replicateM
  , replicateM_
  , sequence
  , sequence_
  , unless
  , void
  , when
  )
import Control.Monad.Extra as E
  (allM, andM, anyM, ifM, notM, orM, unlessM, whenM)
import Control.Monad.IO.Class as E (MonadIO(..))
import Control.Monad.ST as E (ST)
import Control.Monad.Trans.Class as E (lift)
import Control.Monad.Trans.Maybe as E (MaybeT(..))
import Control.Monad.Trans.MultiRWS as E
  (MonadMultiReader(..), MonadMultiState(..), MonadMultiWriter(..), mGet)
import Data.Bifunctor as E (bimap)
import Data.Bool as E (Bool(..))
import Data.Char as E (Char, chr, ord)
import Data.Data as E (toConstr)
import Data.Either as E (Either(..), either)
import Data.Foldable as E (asum, fold, foldl', foldr')
import Data.Function as E ((&), fix)
import Data.Functor as E (($>))
import Data.Functor.Identity as E (Identity(..))
import Data.IORef as E (IORef)
import Data.Int as E (Int)
import Data.List as E
  ( all
  , break
  , drop
  , dropWhile
  , elem
  , filter
  , find
  , intercalate
  , intersperse
  , isPrefixOf
  , isSuffixOf
  , iterate
  , length
  , mapAccumL
  , mapAccumR
  , maximum
  , minimum
  , notElem
  , nub
  , null
  , partition
  , repeat
  , replicate
  , sortBy
  , sum
  , take
  , takeWhile
  , transpose
  , uncons
  , unzip
  , zip
  , zip3
  , zipWith
  )
import Data.List.Extra as E (nubOrd, stripSuffix)
import Data.List.NonEmpty as E (NonEmpty(..), nonEmpty)
import Data.Map as E (Map)
import Data.Maybe as E
  (Maybe(..), catMaybes, fromMaybe, listToMaybe, maybe, maybeToList)
import Data.Monoid as E
  ( All(..)
  , Alt(..)
  , Any(..)
  , Endo(..)
  , Monoid(..)
  , Product(..)
  , Sum(..)
  , mconcat
  )
import Data.Ord as E (Down(..), Ordering(..), comparing)
import Data.Proxy as E (Proxy(..))
import Data.Ratio as E ((%), Ratio, Rational, denominator, numerator)
import Data.Semigroup as E ((<>), Semigroup(..))
import Data.Sequence as E (Seq)
import Data.Set as E (Set)
import Data.String as E (String)
import Data.Text as E (Text)
import Data.Tree as E (Tree(..))
import Data.Tuple as E (swap)
import Data.Typeable as E (Typeable)
import Data.Version as E (showVersion)
import Data.Void as E (Void)
import Data.Word as E (Word, Word32)
import Debug.Trace as E
  ( trace
  , traceIO
  , traceId
  , traceM
  , traceShow
  , traceShowId
  , traceShowM
  , traceStack
  )
import Foreign.ForeignPtr as E (ForeignPtr)
import Foreign.Storable as E (Storable)
import GHC.Exts as E (Constraint)
import GHC.Hs.Extension as E (GhcPs)
import GHC.Types.Name.Reader as E (RdrName)
import Prelude as E
  ( ($)
  , ($!)
  , (&&)
  , (++)
  , (.)
  , (<$>)
  , Bounded(..)
  , Double
  , Enum(..)
  , Eq(..)
  , Float
  , Floating(..)
  , Foldable
  , Fractional(..)
  , Integer
  , Integral(..)
  , Num(..)
  , Ord(..)
  , RealFloat(..)
  , RealFrac(..)
  , Show(..)
  , Traversable
  , (^)
  , and
  , any
  , const
  , curry
  , error
  , flip
  , foldl
  , foldr
  , foldr1
  , fromIntegral
  , fst
  , head
  , id
  , map
  , not
  , or
  , otherwise
  , print
  , putStr
  , putStrLn
  , realToFrac
  , reverse
  , seq
  , snd
  , subtract
  , traverse
  , uncurry
  , undefined
  , (||)
  )
import System.IO as E (IO, hFlush, stdout)
import Text.Read as E (readMaybe)
