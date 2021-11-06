{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Internal.Layouters.Stmt
  ( layoutStmt
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

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.LayouterBasics

import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           GHC.Hs
import           GHC.Types.Name
import qualified GHC.Data.FastString
import           GHC.Types.Basic



layoutStmt :: ToBriDoc' (StmtLR GhcPs GhcPs (LHsExpr GhcPs))
