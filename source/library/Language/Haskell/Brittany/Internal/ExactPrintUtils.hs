{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Brittany.Internal.ExactPrintUtils where

import qualified Control.Monad.State.Class as State.Class
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import Data.Data
import qualified Data.Foldable as Foldable
import qualified Data.Generics as SYB
import Data.HList.HList
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import GHC (GenLocated(L))
import qualified GHC hiding (parseModule)
import qualified GHC.Driver.CmdLine as GHC
import GHC.Hs
import qualified GHC.Types.SrcLoc as GHC
import GHC.Types.SrcLoc (Located, SrcSpan)
import Language.Haskell.Brittany.Internal.Config.Types
import qualified Language.Haskell.Brittany.Internal.ParseModule as ParseModule
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import qualified System.IO

import Language.Haskell.Brittany.Internal.EPCompat

parseModule
  :: [String]
  -> System.IO.FilePath
  -> (GHC.DynFlags -> IO (Either String a))
  -> IO (Either String (GHC.ParsedSource, a))
parseModule args fp dynCheck = do
  str <- System.IO.readFile fp
  parseModuleFromString args fp dynCheck str

parseModuleFromString
  :: [String]
  -> System.IO.FilePath
  -> (GHC.DynFlags -> IO (Either String a))
  -> String
  -> IO (Either String (GHC.ParsedSource, a))
parseModuleFromString = ParseModule.parseModule


commentAnnFixTransformGlob :: SYB.Data ast => ast -> ExactPrint.Transform ()
commentAnnFixTransformGlob ast = undefined
--   do
--   let
--     extract :: forall a . SYB.Data a => a -> Seq (SrcSpan, ExactPrint.AnnKey)
--     extract = -- traceFunctionWith "extract" (show . SYB.typeOf) show $
--       const Seq.empty
--         `SYB.ext1Q` (\l@(L span _) ->
--                       Seq.singleton (span, ExactPrint.mkAnnKey l)
--                     )
--   let nodes = SYB.everything (<>) extract ast
--   let
--     annsMap :: Map GHC.RealSrcLoc ExactPrint.AnnKey
--     annsMap = Map.fromListWith
--       (const id)
--       [ (GHC.realSrcSpanEnd span, annKey)
--       | (GHC.RealSrcSpan span _, annKey) <- Foldable.toList nodes
--       ]
--   nodes `forM_` (snd .> processComs annsMap)
--  where
--   processComs annsMap annKey1 = do
--     mAnn <- State.Class.gets fst <&> Map.lookup annKey1
--     mAnn `forM_` \ann1 -> do
--       let
--         priors = ExactPrint.annPriorComments ann1
--         follows = ExactPrint.annFollowingComments ann1
--         assocs = ExactPrint.annsDP ann1
--       let
--         processCom
--           :: (ExactPrint.Comment, ExactPrint.DeltaPos)
--           -> ExactPrint.TransformT Identity Bool
--         processCom comPair@(com, _) =
--           case GHC.realSrcSpanStart $ ExactPrint.commentIdentifier com of
--             comLoc -> case Map.lookupLE comLoc annsMap of
--               Just (_, annKey2) | loc1 /= loc2 -> case (con1, con2) of
--                 (ExactPrint.CN "RecordCon", ExactPrint.CN "HsRecField") ->
--                   move $> False
--                 (x, y) | x == y -> move $> False
--                 _ -> return True
--                where
--                 ExactPrint.AnnKey annKeyLoc1 con1 = annKey1
--                 ExactPrint.AnnKey annKeyLoc2 con2 = annKey2
--                 loc1 = GHC.realSrcSpanStart annKeyLoc1
--                 loc2 = GHC.realSrcSpanStart annKeyLoc2
--                 move = ExactPrint.modifyAnnsT $ \anns ->
--                   let
--                     ann2 = Data.Maybe.fromJust $ Map.lookup annKey2 anns
--                     ann2' = ann2
--                       { ExactPrint.annFollowingComments =
--                         ExactPrint.annFollowingComments ann2 ++ [comPair]
--                       }
--                   in Map.insert annKey2 ann2' anns
--               _ -> return True -- retain comment at current node.
--       priors' <- filterM processCom priors
--       follows' <- filterM processCom follows
--       assocs' <- flip filterM assocs $ \case
--         (ExactPrint.AnnComment com, dp) -> processCom (com, dp)
--         _ -> return True
--       let
--         ann1' = ann1
--           { ExactPrint.annPriorComments = priors'
--           , ExactPrint.annFollowingComments = follows'
--           , ExactPrint.annsDP = assocs'
--           }
--       ExactPrint.modifyAnnsT $ \anns -> Map.insert annKey1 ann1' anns


-- TODO: this is unused by now, but it contains one detail that
--       commentAnnFixTransformGlob does not include: Moving of comments for
--       "RecordUpd"s.
-- commentAnnFixTransform :: GHC.ParsedSource -> ExactPrint.Transform ()
-- commentAnnFixTransform modul = SYB.everything (>>) genF modul
--  where
--   genF :: Data.Data.Data a => a -> ExactPrint.Transform ()
--   genF = (\_ -> return ()) `SYB.extQ` exprF
--   exprF :: Located (HsExpr GhcPs) -> ExactPrint.Transform ()
--   exprF lexpr@(L _ expr) = case expr of
-- #if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
--     RecordCon _ _ (HsRecFields fs@(_:_) Nothing) ->
-- #else
--     RecordCon _ _ _ (HsRecFields fs@(_:_) Nothing) ->
-- #endif
--       moveTrailingComments lexpr (List.last fs)
-- #if MIN_VERSION_ghc(8,6,0)   /* ghc-8.6 */
--     RecordUpd _ _e fs@(_:_) ->
-- #else
--     RecordUpd _e fs@(_:_) _cons _ _ _ ->
-- #endif
--       moveTrailingComments lexpr (List.last fs)
--     _ -> return ()

-- commentAnnFixTransform :: GHC.ParsedSource -> ExactPrint.Transform ()
-- commentAnnFixTransform modul = SYB.everything (>>) genF modul
--  where
--   genF :: Data.Data.Data a => a -> ExactPrint.Transform ()
--   genF = (\_ -> return ()) `SYB.extQ` exprF
--   exprF :: Located (HsExpr GhcPs) -> ExactPrint.Transform ()
--   exprF lexpr@(L _ expr) = case expr of
--     RecordCon _ _ (HsRecFields fs@(_:_) Nothing) ->
--       moveTrailingComments lexpr (List.last fs)
--     RecordUpd _ _e fs@(_:_) ->
--       moveTrailingComments lexpr (List.last fs)
--     _ -> return ()

-- moveTrailingComments :: (Data.Data.Data a,Data.Data.Data b)
--                      => GHC.Located a -> GHC.Located b -> ExactPrint.Transform ()
-- moveTrailingComments astFrom astTo = do
--   let
--     k1 = ExactPrint.mkAnnKey astFrom
--     k2 = ExactPrint.mkAnnKey astTo
--     moveComments ans = ans'
--       where
--         an1 = Data.Maybe.fromJust $ Map.lookup k1 ans
--         an2 = Data.Maybe.fromJust $ Map.lookup k2 ans
--         cs1f = ExactPrint.annFollowingComments an1
--         cs2f = ExactPrint.annFollowingComments an2
--         (comments, nonComments) = flip breakEither (ExactPrint.annsDP an1)
--              $ \case
--                (ExactPrint.AnnComment com, dp) -> Left (com, dp)
--                x -> Right x
--         an1' = an1
--           { ExactPrint.annsDP               = nonComments
--           , ExactPrint.annFollowingComments = []
--           }
--         an2' = an2
--           { ExactPrint.annFollowingComments = cs1f ++ cs2f ++ comments
--           }
--         ans' = Map.insert k1 an1' $ Map.insert k2 an2' ans

--   ExactPrint.modifyAnnsT moveComments

{--

-- | split a set of annotations in a module into a map from top-level module
-- elements to the relevant annotations. Avoids quadratic behaviour a trivial
-- implementation would have.
extractToplevelAnns
  :: Located HsModule
  -> ExactPrint.Anns
  -> Map ExactPrint.AnnKey ExactPrint.Anns
extractToplevelAnns lmod anns = output
 where
  (L _ (HsModule _ _ _ _ ldecls _ _)) = lmod
  declMap1 :: Map ExactPrint.AnnKey ExactPrint.AnnKey
  declMap1 = Map.unions $ ldecls <&> \ldecl ->
    Map.fromSet (const (ExactPrint.mkAnnKey ldecl)) (foldedAnnKeys ldecl)
  declMap2 :: Map ExactPrint.AnnKey ExactPrint.AnnKey
  declMap2 =
    Map.fromList
      $ [ (captured, declMap1 Map.! k)
        | (k, ExactPrint.Ann _ _ _ _ _ (Just captured)) <- Map.toList anns
        ]
  declMap = declMap1 `Map.union` declMap2
  modKey = ExactPrint.mkAnnKey lmod
  output = groupMap (\k _ -> Map.findWithDefault modKey k declMap) anns

groupMap :: (Ord k, Ord l) => (k -> a -> l) -> Map k a -> Map l (Map k a)
groupMap f = Map.foldlWithKey'
  (\m k a -> Map.alter (insert k a) (f k a) m)
  Map.empty
 where
  insert k a Nothing = Just (Map.singleton k a)
  insert k a (Just m) = Just (Map.insert k a m)

foldedAnnKeys :: Data.Data.Data ast => ast -> Set ExactPrint.AnnKey
foldedAnnKeys ast = SYB.everything
  Set.union
  (\x -> maybe
    Set.empty
    Set.singleton
    [ SYB.gmapQi 1 (ExactPrint.mkAnnKey . L l) x
    | locTyCon == SYB.typeRepTyCon (SYB.typeOf x)
    , l :: SrcSpan <- SYB.gmapQi 0 SYB.cast x
    ]
      -- for some reason, ghc-8.8 has forgotten how to infer the type of l,
      -- even though it is passed to mkAnnKey above, which only accepts
      -- SrcSpan.
  )
  ast
  where locTyCon = SYB.typeRepTyCon (SYB.typeOf (L () ()))

-}

withTransformedAnns
  :: Data ast
  => ast
  -> MultiRWSS.MultiRWS '[Config , Anns] w s a
  -> MultiRWSS.MultiRWS '[Config , Anns] w s a
withTransformedAnns ast m = MultiRWSS.mGetRawR >>= \case
  readers@(conf :+: anns :+: HNil) -> do
    -- TODO: implement `local` for MultiReader/MultiRWS
    MultiRWSS.mPutRawR (conf :+: f anns :+: HNil)
    x <- m
    MultiRWSS.mPutRawR readers
    pure x
 where
  f anns =
    let
      ((), _, _) =
        ExactPrint.runTransform (commentAnnFixTransformGlob ast)
    in anns


warnExtractorCompat :: GHC.Warn -> String
warnExtractorCompat (GHC.Warn _ (L _ s)) = s
