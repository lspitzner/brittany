{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.Brittany.Internal.Utils
  ( parDoc
  , parDocW
  , fromMaybeIdentity
  , fromOptionIdentity
  , traceIfDumpConf
  , mModify
  , customLayouterF
  , astToDoc
  , briDocToDoc
  -- , displayBriDocSimpleTree
  , annsDoc
  , Max (..)
  , tellDebugMess
  , tellDebugMessShow
  , briDocToDocWithAnns
  , breakEither
  , spanMaybe
  , transformUp
  , transformDownMay
  , FirstLastView(..)
  , splitFirstLast
  , lines'
  , showOutputable
  , absurdExt
  )
where



#include "prelude.inc"

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint.Utils

import           Data.Data
import           Data.Generics.Schemes
import           Data.Generics.Aliases

import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint ( ($+$), (<+>) )

import qualified Outputable    as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified SrcLoc        as GHC
import           OccName ( occNameString )
import qualified Data.ByteString as B

import           DataTreePrint

import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Types

import qualified Data.Generics.Uniplate.Direct as Uniplate
import           HsExtension (NoExt)



parDoc :: String -> PP.Doc
parDoc = PP.fsep . fmap PP.text . List.words

parDocW :: [String] -> PP.Doc
parDocW = PP.fsep . fmap PP.text . List.words . List.unwords


showSDoc_ :: GHC.SDoc -> String
showSDoc_ = GHC.showSDoc GHC.unsafeGlobalDynFlags

showOutputable :: (GHC.Outputable a) => a -> String
showOutputable = GHC.showPpr GHC.unsafeGlobalDynFlags

fromMaybeIdentity :: Identity a -> Maybe a -> Identity a
fromMaybeIdentity x y = Data.Coerce.coerce $ fromMaybe (Data.Coerce.coerce x) y

fromOptionIdentity :: Identity a -> Option a -> Identity a
fromOptionIdentity x y =
  Data.Coerce.coerce $ fromMaybe (Data.Coerce.coerce x) $ getOption y

-- maximum monoid over N+0
-- or more than N, because Num is allowed.
newtype Max a = Max { getMax :: a }
  deriving (Eq, Ord, Show, Bounded, Num)

instance (Num a, Ord a) => Semigroup (Max a) where
  (<>) = Data.Coerce.coerce (max :: a -> a -> a)

instance (Num a, Ord a) => Monoid (Max a) where
  mempty  = Max 0
  mappend = (<>)

newtype ShowIsId = ShowIsId String deriving Data

instance Show ShowIsId where show (ShowIsId x) = x

data A x = A ShowIsId x deriving Data

customLayouterF :: ExactPrint.Types.Anns -> LayouterF
customLayouterF anns layoutF =
  DataToLayouter
    $       f
    `extQ`  showIsId
    `extQ`  fastString
    `extQ`  bytestring
    `extQ`  occName
    `extQ`  srcSpan
    `ext2Q` located
 where
  DataToLayouter f = defaultLayouterF layoutF
  simpleLayouter :: String -> NodeLayouter
  simpleLayouter s = NodeLayouter (length s) False (const $ PP.text s)
  showIsId :: ShowIsId -> NodeLayouter
  showIsId (ShowIsId s) = NodeLayouter (length s + 2) True $ \case
    Left  True -> PP.parens $ PP.text s
    Left  False -> PP.text s
    Right _    -> PP.text s
  fastString =
    simpleLayouter . ("{FastString: "++) . (++"}") . show :: GHC.FastString
      -> NodeLayouter
  bytestring = simpleLayouter . show :: B.ByteString -> NodeLayouter
  occName = simpleLayouter . ("{OccName: "++) . (++"}") . OccName.occNameString
  srcSpan :: GHC.SrcSpan -> NodeLayouter
  srcSpan ss = simpleLayouter
             -- - $ "{"++ showSDoc_ (GHC.ppr ss)++"}"
                              $ "{" ++ showOutputable ss ++ "}"
  located :: (Data b, Data loc) => GHC.GenLocated loc b -> NodeLayouter
  located (GHC.L ss a) = runDataToLayouter layoutF $ A annStr a
   where
    annStr = case cast ss of
      Just (s :: GHC.SrcSpan) ->
        ShowIsId $ show (ExactPrint.Utils.getAnnotationEP (GHC.L s a) anns)
      Nothing -> ShowIsId "nnnnnnnn"

customLayouterNoAnnsF :: LayouterF
customLayouterNoAnnsF layoutF =
  DataToLayouter
    $       f
    `extQ`  showIsId
    `extQ`  fastString
    `extQ`  bytestring
    `extQ`  occName
    `extQ`  srcSpan
    `ext2Q` located
 where
  DataToLayouter f = defaultLayouterF layoutF
  simpleLayouter :: String -> NodeLayouter
  simpleLayouter s = NodeLayouter (length s) False (const $ PP.text s)
  showIsId :: ShowIsId -> NodeLayouter
  showIsId (ShowIsId s) = NodeLayouter (length s + 2) True $ \case
    Left  True -> PP.parens $ PP.text s
    Left  False -> PP.text s
    Right _    -> PP.text s
  fastString =
    simpleLayouter . ("{FastString: "++) . (++"}") . show :: GHC.FastString
      -> NodeLayouter
  bytestring = simpleLayouter . show :: B.ByteString -> NodeLayouter
  occName = simpleLayouter . ("{OccName: "++) . (++"}") . OccName.occNameString
  srcSpan :: GHC.SrcSpan -> NodeLayouter
  srcSpan ss = simpleLayouter $ "{" ++ showSDoc_ (GHC.ppr ss) ++ "}"
  located :: (Data b) => GHC.GenLocated loc b -> NodeLayouter
  located (GHC.L _ss a) = runDataToLayouter layoutF a

-- displayBriDocTree :: BriDoc -> PP.Doc
-- displayBriDocTree = \case
--   BDWrapAnnKey annKey doc -> def "BDWrapAnnKey"
--                            $ PP.text (show annKey)
--                          $+$ displayBriDocTree doc
--   BDEmpty         -> PP.text "BDEmpty"
--   BDLit t         -> def "BDLit" $ PP.text (show t)
--   BDSeq list      -> def "BDSeq" $ displayList list
--   BDCols sig list -> def "BDCols" $ PP.text (show sig)
--                                 $+$ displayList list
--   BDSeparator     -> PP.text "BDSeparator"
--   BDPar rol indent lines -> def "BDPar" $ displayBriDocTree rol
--                                       $+$ PP.text (show indent)
--                                       $+$ displayList lines
--   BDAlt alts      -> def "BDAlt" $ displayList alts
--   BDExternal ast _t -> def "BDExternal" (astToDoc ast)
--   BDSpecialPostCommentLoc _ -> PP.text "BDSpecialPostCommentLoc"
--  where
--   def x r = PP.text x $+$ PP.nest 2 r
--   displayList :: [BriDoc] -> PP.Doc
--   displayList [] = PP.text "[]"
--   displayList (x:xr) = PP.cat $ PP.text "[" <+> displayBriDocTree x
--                               : [PP.text "," <+> displayBriDocTree t | t<-xr]
--                              ++ [PP.text "]"]

-- displayBriDocSimpleTree :: BriDocSimple -> PP.Doc
-- displayBriDocSimpleTree = \case
--   BDSWrapAnnKey annKey doc -> def "BDSWrapAnnKey"
--                            $ PP.text (show annKey)
--                          $+$ displayBriDocSimpleTree doc
--   BDSLit t         -> def "BDSLit" $ PP.text (show t)
--   BDSSeq list      -> def "BDSSeq" $ displayList list
--   BDSCols sig list -> def "BDSCols" $ PP.text (show sig)
--                                 $+$ displayList list
--   BDSSeparator     -> PP.text "BDSSeparator"
--   BDSPar rol indent lines -> def "BDSPar" $ displayBriDocSimpleTree rol
--                                       $+$ PP.text (show indent)
--                                       $+$ displayList lines
--   BDSExternal annKey _subKeys _t -> def "BDSExternal" (PP.text $ show annKey)
--   BDSSpecialPostCommentLoc _ -> PP.text "BDSSpecialPostCommentLoc"
--  where
--   def x r = PP.text x $+$ PP.nest 2 r
--   displayList :: [BriDocSimple] -> PP.Doc
--   displayList [] = PP.text "[]"
--   displayList (x:xr) = PP.cat $ PP.text "[" <+> displayBriDocSimpleTree x
--                               : [PP.text "," <+> displayBriDocSimpleTree t | t<-xr]
--                              ++ [PP.text "]"]

traceIfDumpConf
  :: (MonadMultiReader Config m, Show a)
  => String
  -> (DebugConfig -> Identity (Semigroup.Last Bool))
  -> a
  -> m ()
traceIfDumpConf s accessor val = do
  whenM (mAsk <&> _conf_debug .> accessor .> confUnpack) $ do
    trace ("---- " ++ s ++ " ----\n" ++ show val) $ return ()

tellDebugMess :: MonadMultiWriter
  (Seq String) m => String -> m ()
tellDebugMess s = mTell $ Seq.singleton s

tellDebugMessShow :: forall a m . (MonadMultiWriter
  (Seq String) m, Show a) => a -> m ()
tellDebugMessShow = tellDebugMess . show

-- i should really put that into multistate..
mModify :: MonadMultiState s m => (s -> s) -> m ()
mModify f = mGet >>= mSet . f

astToDoc :: Data ast => ast -> PP.Doc
astToDoc ast = printTreeWithCustom 160 customLayouterNoAnnsF ast

briDocToDoc :: BriDoc -> PP.Doc
briDocToDoc = astToDoc . removeAnnotations
 where
  removeAnnotations = Uniplate.transform $ \case
    BDAnnotationPrior _ x -> x
    BDAnnotationKW _ _ x  -> x
    BDAnnotationRest _ x  -> x
    x                     -> x

briDocToDocWithAnns :: BriDoc -> PP.Doc
briDocToDocWithAnns = astToDoc

annsDoc :: ExactPrint.Types.Anns -> PP.Doc
annsDoc = printTreeWithCustom 100 customLayouterNoAnnsF . fmap (ShowIsId . show)

breakEither :: (a -> Either b c) -> [a] -> ([b], [c])
breakEither _  []      = ([], [])
breakEither fn (a1:aR) = case fn a1 of
  Left  b -> (b : bs, cs)
  Right c -> (bs, c : cs)
 where
  (bs, cs) = breakEither fn aR

spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f (x1:xR) | Just y <- f x1 = (y : ys, xs)
 where
  (ys, xs) = spanMaybe f xR
spanMaybe _ xs                       = ([], xs)

data FirstLastView a
  = FirstLastEmpty
  | FirstLastSingleton a
  | FirstLast a [a] a

splitFirstLast :: [a] -> FirstLastView a
splitFirstLast [] = FirstLastEmpty
splitFirstLast [x] = FirstLastSingleton x
splitFirstLast (x1:xr) = FirstLast x1 (List.init xr) (List.last xr)

-- TODO: move to uniplate upstream?
-- aka `transform`
transformUp :: Uniplate.Uniplate on => (on -> on) -> (on -> on)
transformUp f = g where g = f . Uniplate.descend g
_transformDown :: Uniplate.Uniplate on => (on -> on) -> (on -> on)
_transformDown f = g where g = Uniplate.descend g . f
transformDownMay :: Uniplate.Uniplate on => (on -> Maybe on) -> (on -> on)
transformDownMay f = g where g x = maybe x (Uniplate.descend g) $ f x
_transformDownRec :: Uniplate.Uniplate on => (on -> Maybe on) -> (on -> on)
_transformDownRec f = g where g x = maybe (Uniplate.descend g x) g $ f x

-- | similar to List.lines, but treating the case of final newline character
-- in such a manner that this function is the inverse of @intercalate "\n"@.
lines' :: String -> [String]
lines' s = case break (== '\n') s of
  (s1, []) -> [s1]
  (s1, [_]) -> [s1, ""]
  (s1, (_:r)) -> s1 : lines' r

-- | A method to dismiss NoExt patterns for total matches
absurdExt :: NoExt -> a
absurdExt = error "cannot construct NoExt"
