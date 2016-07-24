{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Haskell.Brittany.Types
where



#include "prelude.inc"

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint

import qualified Data.Text.Lazy.Builder as Text.Builder

import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           SrcLoc ( SrcSpan )

import           Language.Haskell.GHC.ExactPrint ( AnnKey, Comment )
import           Language.Haskell.GHC.ExactPrint.Types ( Anns, DeltaPos, mkAnnKey )

import Language.Haskell.Brittany.Config.Types



type PPM a = MultiRWSS.MultiRWS '[Config, ExactPrint.Anns] '[Text.Builder.Builder, [LayoutError], Seq String] '[] a

type PriorMap = Map AnnKey [(Comment, DeltaPos)]
type PostMap  = Map AnnKey [(Comment, DeltaPos)]

data LayoutState = LayoutState
  { _lstate_baseY         :: Int -- ^ number of current indentation columns
                                 -- (not number of indentations).
  , _lstate_curY          :: Int -- ^ number of chars in the current line.
  , _lstate_indLevel      :: Int -- ^ current indentation level. set for
                                 -- any layout-affected elements such as
                                 -- let/do/case/where elements.
                                 -- The main purpose of this member is to
                                 -- properly align comments, as their
                                 -- annotation positions are relative to the
                                 -- current layout indentation level.
  , _lstate_indLevelLinger :: Int -- like a "last" of indLevel. Used for
                                  -- properly treating cases where comments
                                  -- on the first indented element have an
                                  -- annotation offset relative to the last
                                  -- non-indented element, which is confusing.
  , _lstate_commentsPrior :: PriorMap -- map of "true" pre-node comments that
                                      -- really _should_ be included in the
                                      -- output.
  , _lstate_commentsPost  :: PostMap  -- similarly, for post-node comments.
  , _lstate_commentCol    :: Maybe Int
  , _lstate_addSepSpace   :: Maybe Int -- number of spaces to insert if anyone
                                       -- writes (any non-spaces) in the
                                       -- current line.
  , _lstate_inhibitMTEL   :: Bool
      -- ^ inhibit move-to-exact-location.
      -- normally, processing a node's annotation involves moving to the exact
      -- (vertical) location of the node. this ensures that newlines in the
      -- input are retained in the output.
      -- While this flag is on, this behaviour will be disabled.
      -- The flag is automatically turned off when inserting any kind of
      -- newline.
  , _lstate_isNewline     :: NewLineState
      -- captures if the layouter currently is in a new line, i.e. if the
      -- current line only contains (indentation) spaces.
  }

data NewLineState = NewLineStateInit -- initial state. we do not know if in a
                                     -- newline, really. by special-casing
                                     -- this we can appropriately handle it
                                     -- differently at use-site.
                  | NewLineStateYes
                  | NewLineStateNo
  deriving Eq

-- data LayoutSettings = LayoutSettings
--   { _lsettings_cols :: Int -- the thing that has default 80.
--   , _lsettings_indentPolicy :: IndentPolicy
--   , _lsettings_indentAmount :: Int
--   , _lsettings_indentWhereSpecial :: Bool -- indent where only 1 sometimes (TODO).
--   , _lsettings_indentListSpecial  :: Bool -- use some special indentation for ","
--                                           -- when creating zero-indentation
--                                           -- multi-line list literals.
--   , _lsettings_importColumn :: Int
--   , _lsettings_initialAnns :: ExactPrint.Anns
--   }

data LayoutError = LayoutErrorUnusedComment String
                 | LayoutWarning String
                 | forall ast . Data.Data.Data ast => LayoutErrorUnknownNode String ast

data BriSpacing = BriSpacing
  { _bs_spacePastLineIndent :: Int -- space in the current,
                                   -- potentially somewhat filled
                                   -- line.
  , _bs_spacePastIndent :: Int     -- space required in properly
                                   -- indented blocks below the
                                   -- current line.
  }

data ColSig
  = ColTyOpPrefix
    -- any prefixed operator/paren/"::"/..
    -- expected to have exactly two colums.
    -- e.g. ":: foo"
    --       111222
    --      "-> bar asd asd"
    --       11122222222222
  | ColFuncPatternsPrefix
    -- pattern-part of the lhs, e.g. "func (foo a b) c _".
    -- Has variable number of columns depending on the number of patterns.
  | ColFuncPatternsInfix
    -- pattern-part of the lhs, e.g. "Foo a <> Foo b".
    -- Has variable number of columns depending on the number of patterns.
  | ColCasePattern
  | ColEquation
    -- e.g. "func pat pat = expr"
    --       1111111111111222222
    -- expected to have exactly two columns.
  | ColGuardedEquation
    -- e.g. "func pat pat | cond = expr"
    --       11111111111112222222222222
    -- or   "func pat pat | cond"
    --       1111111111111222222
    -- expected to have exactly two or three columns.
  | ColDoBind
  | ColDoLet -- the non-indented variant
  | ColRecUpdate
  | ColListComp
  | ColList
  | ColOpPrefix -- merge with ColList ? other stuff?

  -- TODO
  deriving (Eq, Data.Data.Data, Show)

data BrIndent = BrIndentNone
              | BrIndentRegular
              | BrIndentSpecial Int
  deriving (Eq, Typeable, Data.Data.Data, Show)

type ToBriDocM = MultiRWSS.MultiRWS '[Config, Anns] '[[LayoutError], Seq String] '[]

type ToBriDoc (sym :: * -> *) = GenLocated SrcSpan (sym RdrName) -> ToBriDocM BriDoc
type ToBriDoc' sym            = GenLocated SrcSpan sym           -> ToBriDocM BriDoc
type ToBriDocC sym c          = GenLocated SrcSpan sym           -> ToBriDocM c

data DocMultiLine
  = MultiLineNo
  | MultiLinePossible
  deriving (Eq, Typeable)

data BriDoc
  = -- BDWrapAnnKey AnnKey BriDoc
    BDEmpty
  | BDLit Text
  | BDSeq [BriDoc] -- elements other than the last should
                   -- not contains BDPars.
  | BDCols ColSig [BriDoc] -- elements other than the last
                         -- should not contains BDPars
  | BDSeparator -- semantically, space-unless-at-end-of-line.
  | BDAddBaseY BrIndent BriDoc
  | BDSetBaseY BriDoc
  | BDSetIndentLevel BriDoc
  | BDPar
    { _bdpar_indent :: BrIndent
    , _bdpar_restOfLine :: BriDoc -- should not contain other BDPars
    , _bdpar_indented :: BriDoc
    }
  -- | BDAddIndent BrIndent BriDoc
  -- | BDNewline
  | BDAlt [BriDoc]
  | BDForceMultiline BriDoc
  | BDForceSingleline BriDoc
  | BDForwardLineMode BriDoc
  | BDExternal AnnKey
               (Set AnnKey) -- set of annkeys contained within the node
                            -- to be printed via exactprint
               Bool -- should print extra comment ?
               Text
  | BDAnnotationPrior AnnKey BriDoc
  | BDAnnotationPost  AnnKey BriDoc
  | BDLines [BriDoc]
  | BDEnsureIndent BrIndent BriDoc
  | BDProhibitMTEL BriDoc -- move to exact location
                          -- TODO: this constructor is deprecated. should
                          --       still work, but i should probably completely
                          --       remove it, as i have no proper usecase for
                          --       it anymore.
  deriving Data.Data.Data

data VerticalSpacing
  = VerticalSpacing
    { _vs_sameLine  :: !Int
    , _vs_paragraph :: !(Strict.Maybe Int)
    }
    deriving Show

newtype LineModeValidity a = LineModeValidity (Strict.Maybe a)
  deriving (Functor, Applicative, Monad, Show)

pattern LineModeValid :: forall t. t -> LineModeValidity t
pattern LineModeValid x = LineModeValidity (Strict.Just x) :: LineModeValidity t
pattern LineModeInvalid :: forall t. LineModeValidity t
pattern LineModeInvalid = LineModeValidity Strict.Nothing :: LineModeValidity t
