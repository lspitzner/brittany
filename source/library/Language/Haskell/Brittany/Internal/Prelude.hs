module Language.Haskell.Brittany.Internal.Prelude ( module E ) where



-- rather project-specific stuff:
---------------------------------
import GHC.Hs.Extension               as E ( GhcPs )

import GHC.Types.Name.Reader          as E ( RdrName )


-- more general:
----------------

import Data.Functor.Identity         as E ( Identity(..) )
import Control.Concurrent.Chan       as E ( Chan )
import Control.Concurrent.MVar       as E ( MVar
                                          , newEmptyMVar
                                          , newMVar
                                          , putMVar
                                          , readMVar
                                          , takeMVar
                                          , swapMVar
                                          )
import Data.Int                      as E ( Int )
import Data.Word                     as E ( Word
                                          , Word32
                                          )
import Prelude                       as E ( Integer
                                          , Float
                                          , Double
                                          , undefined
                                          , Eq (..)
                                          , Ord (..)
                                          , Enum (..)
                                          , Bounded (..)
                                          , (<$>)
                                          , (.)
                                          , ($)
                                          , ($!)
                                          , Num (..)
                                          , Integral (..)
                                          , Fractional (..)
                                          , Floating (..)
                                          , RealFrac (..)
                                          , RealFloat (..)
                                          , fromIntegral
                                          , error
                                          , foldr
                                          , foldl
                                          , foldr1
                                          , id
                                          , map
                                          , subtract
                                          , putStrLn
                                          , putStr
                                          , Show (..)
                                          , print
                                          , fst
                                          , snd
                                          , (++)
                                          , not
                                          , (&&)
                                          , (||)
                                          , curry
                                          , uncurry
                                          , flip
                                          , const
                                          , seq
                                          , reverse
                                          , otherwise
                                          , traverse
                                          , realToFrac
                                          , or
                                          , and
                                          , head
                                          , any
                                          , (^)
                                          , Foldable
                                          , Traversable
                                          )
import Control.Monad.ST              as E ( ST )
import Data.Bool                     as E ( Bool(..) )
import Data.Char                     as E ( Char
                                          , ord
                                          , chr
                                          )
import Data.Either                   as E ( Either(..)
                                          , either
                                          )
import Data.IORef                    as E ( IORef )
import Data.Maybe                    as E ( Maybe(..)
                                          , fromMaybe
                                          , maybe
                                          , listToMaybe
                                          , maybeToList
                                          , catMaybes
                                          )
import Data.Monoid                   as E ( Endo(..)
                                          , All(..)
                                          , Any(..)
                                          , Sum(..)
                                          , Product(..)
                                          , Alt(..)
                                          , mconcat
                                          , Monoid (..)
                                          )
import Data.Ord                      as E ( Ordering(..)
                                          , Down(..)
                                          , comparing
                                          )
import Data.Ratio                    as E ( Ratio
                                          , Rational
                                          , (%)
                                          , numerator
                                          , denominator
                                          )
import Data.String                   as E ( String )
import Data.Void                     as E ( Void )
import System.IO                     as E ( IO
                                          , hFlush
                                          , stdout
                                          )
import Data.Proxy                    as E ( Proxy(..) )
import Data.Sequence                 as E ( Seq )

import Data.Map                      as E ( Map )
import Data.Set                      as E ( Set )

import Data.Text                     as E ( Text )

import Data.Function                 as E ( fix
                                          , (&)
                                          )

import Data.Foldable                 as E ( foldl'
                                          , foldr'
                                          , fold
                                          , asum
                                          )

import Data.List                     as E ( partition
                                          , null
                                          , elem
                                          , notElem
                                          , minimum
                                          , maximum
                                          , length
                                          , all
                                          , take
                                          , drop
                                          , find
                                          , sum
                                          , zip
                                          , zip3
                                          , zipWith
                                          , repeat
                                          , replicate
                                          , iterate
                                          , nub
                                          , filter
                                          , intersperse
                                          , intercalate
                                          , isSuffixOf
                                          , isPrefixOf
                                          , dropWhile
                                          , takeWhile
                                          , unzip
                                          , break
                                          , transpose
                                          , sortBy
                                          , mapAccumL
                                          , mapAccumR
                                          , uncons
                                          )

import Data.List.NonEmpty            as E ( NonEmpty(..)
                                          , nonEmpty
                                          )

import Data.Tuple                    as E ( swap
                                          )

import Text.Read                     as E ( readMaybe
                                          )

import Control.Monad                 as E ( Functor (..)
                                          , Monad (..)
                                          , MonadPlus (..)
                                          , mapM
                                          , mapM_
                                          , forM
                                          , forM_
                                          , sequence
                                          , sequence_
                                          , (=<<)
                                          , (>=>)
                                          , (<=<)
                                          , forever
                                          , void
                                          , join
                                          , replicateM
                                          , replicateM_
                                          , guard
                                          , when
                                          , unless
                                          , liftM
                                          , liftM2
                                          , liftM3
                                          , liftM4
                                          , liftM5
                                          , filterM
                                          , (<$!>)
                                          )

import Control.Applicative           as E ( Applicative (..)
                                          , Alternative (..)
                                          )

import Foreign.Storable              as E ( Storable )
import GHC.Exts                      as E ( Constraint )

import Control.Concurrent            as E ( threadDelay
                                          , forkIO
                                          , forkOS
                                          )

import Control.Exception             as E ( evaluate
                                          , bracket
                                          , assert
                                          )

import Debug.Trace                   as E ( trace
                                          , traceId
                                          , traceShowId
                                          , traceShow
                                          , traceStack
                                          , traceShowId
                                          , traceIO
                                          , traceM
                                          , traceShowM
                                          )

import Foreign.ForeignPtr            as E ( ForeignPtr
                                          )

import Data.Bifunctor                as E ( bimap )
import Data.Functor                  as E ( ($>) )
import Data.Semigroup                as E ( (<>)
                                          , Semigroup(..)
                                          )

import Data.Typeable                 as E ( Typeable
                                          )

import Control.Arrow                 as E ( first
                                          , second
                                          , (***)
                                          , (&&&)
                                          , (>>>)
                                          , (<<<)
                                          )

import Data.Version                  as E ( showVersion
                                          )

import Data.List.Extra               as E ( nubOrd
                                          , stripSuffix
                                          )
import Control.Monad.Extra           as E ( whenM
                                          , unlessM
                                          , ifM
                                          , notM
                                          , orM
                                          , andM
                                          , anyM
                                          , allM
                                          )

import Data.Tree                     as E ( Tree(..)
                                          )

import Control.Monad.Trans.MultiRWS  as E ( -- MultiRWST (..)
                                          -- , MultiRWSTNull
                                          -- , MultiRWS
                                          -- ,
                                            MonadMultiReader(..)
                                          , MonadMultiWriter(..)
                                          , MonadMultiState(..)
                                          , mGet
                                          -- , runMultiRWST
                                          -- , runMultiRWSTASW
                                          -- , runMultiRWSTW
                                          -- , runMultiRWSTAW
                                          -- , runMultiRWSTSW
                                          -- , runMultiRWSTNil
                                          -- , runMultiRWSTNil_
                                          -- , withMultiReader
                                          -- , withMultiReader_
                                          -- , withMultiReaders
                                          -- , withMultiReaders_
                                          -- , withMultiWriter
                                          -- , withMultiWriterAW
                                          -- , withMultiWriterWA
                                          -- , withMultiWriterW
                                          -- , withMultiWriters
                                          -- , withMultiWritersAW
                                          -- , withMultiWritersWA
                                          -- , withMultiWritersW
                                          -- , withMultiState
                                          -- , withMultiStateAS
                                          -- , withMultiStateSA
                                          -- , withMultiStateA
                                          -- , withMultiStateS
                                          -- , withMultiState_
                                          -- , withMultiStates
                                          -- , withMultiStatesAS
                                          -- , withMultiStatesSA
                                          -- , withMultiStatesA
                                          -- , withMultiStatesS
                                          -- , withMultiStates_
                                          -- , inflateReader
                                          -- , inflateMultiReader
                                          -- , inflateWriter
                                          -- , inflateMultiWriter
                                          -- , inflateState
                                          -- , inflateMultiState
                                          -- , mapMultiRWST
                                          -- , mGetRawR
                                          -- , mGetRawW
                                          -- , mGetRawS
                                          -- , mPutRawR
                                          -- , mPutRawW
                                          -- , mPutRawS
                                          )

import Control.Monad.IO.Class        as E ( MonadIO (..)
                                          )

import Control.Monad.Trans.Class     as E ( lift
                                          )
import Control.Monad.Trans.Maybe     as E ( MaybeT (..)
                                          )

import Data.Data                     as E ( toConstr
                                          )
