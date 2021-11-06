{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Internal.ExactPrintUtils
  ( parseModule
  , parseModuleFromString
  , commentAnnFixTransformGlob
  , extractToplevelAnns
  , foldedAnnKeys
  , withTransformedAnns
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
import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Utils
import           Data.Data
import           Data.HList.HList

import           GHC.Driver.Session ( getDynFlags )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import qualified GHC.Driver.Session      as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified GHC.Parser        as GHC
import qualified GHC.Types.SrcLoc        as GHC
import qualified GHC.Data.FastString    as GHC
import qualified GHC.Parser.Lexer         as GHC
import qualified GHC.Data.StringBuffer  as GHC
import qualified GHC.Utils.Outputable    as GHC
import qualified GHC.Driver.CmdLine as GHC

import           GHC.Hs
import           GHC.Data.Bag

import           GHC.Types.SrcLoc ( SrcSpan, Located )


import qualified Language.Haskell.GHC.ExactPrint            as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate   as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types      as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Parsers    as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Preprocess as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Delta      as ExactPrint

import qualified Data.Generics as SYB

import           Control.Exception
-- import           Data.Generics.Schemes



parseModule
  :: [String]
  -> System.IO.FilePath
  -> (GHC.DynFlags -> IO (Either String a))
  -> IO (Either String (ExactPrint.Anns, GHC.ParsedSource, a))
parseModule =
  parseModuleWithCpp ExactPrint.defaultCppOptions ExactPrint.normalLayout

-- | Parse a module with specific instructions for the C pre-processor.
parseModuleWithCpp
  :: ExactPrint.CppOptions
  -> ExactPrint.DeltaOptions
  -> [String]
  -> System.IO.FilePath
  -> (GHC.DynFlags -> IO (Either String a))
  -> IO (Either String (ExactPrint.Anns, GHC.ParsedSource, a))
parseModuleWithCpp cpp opts args fp dynCheck =
  ExactPrint.ghcWrapper $ ExceptT.runExceptT $ do
    dflags0                       <- lift $ GHC.getSessionDynFlags
    (dflags1, leftover, warnings) <- lift $ GHC.parseDynamicFlagsCmdLine
      dflags0
      (GHC.noLoc <$> ("-hide-all-packages" : args))
      -- that we pass -hide-all-packages here is a duplication, because
      -- ExactPrint.initDynFlags also does it, but necessary because of
      -- stupid and careless GHC API design. We explicitly want to pass
      -- our args before calling that, so this is what we do. Should be
      -- harmless. See commit 1b7576dcd1823e1c685a44927b1fcaade1319063.
    void $ lift $ GHC.setSessionDynFlags dflags1
    dflags2 <- lift $ ExactPrint.initDynFlags fp
    when (not $ null leftover)
      $  ExceptT.throwE
      $  "when parsing ghc flags: leftover flags: "
      ++ show (leftover <&> \(L _ s) -> s)
    when (not $ null warnings)
      $  ExceptT.throwE
      $  "when parsing ghc flags: encountered warnings: "
      ++ show (warnings <&> warnExtractorCompat)
    x   <- ExceptT.ExceptT $ liftIO $ dynCheck dflags2
    res <- lift $ ExactPrint.parseModuleApiAnnsWithCppInternal cpp dflags2 fp
    either (\err -> ExceptT.throwE $ "transform error: " ++ show (bagToList (show <$> err)))
           (\(a, m) -> pure (a, m, x))
      $ ExactPrint.postParseTransform res opts

parseModuleFromString
  :: [String]
  -> System.IO.FilePath
  -> (GHC.DynFlags -> IO (Either String a))
  -> String
  -> IO (Either String (ExactPrint.Anns, GHC.ParsedSource, a))
parseModuleFromString args fp dynCheck str =
  -- We mask here because otherwise using `throwTo` (i.e. for a timeout) will
  -- produce nasty looking errors ("ghc panic"). The `mask_` makes it so we
  -- cannot kill the parsing thread - not very nice. But i'll
  -- optimistically assume that most of the time brittany uses noticable or
  -- longer time, the majority of the time is not spend in parsing, but in
  -- bridoc transformation stuff.
  -- (reminder to update note on `parsePrintModule` if this changes.)
  mask_ $ ExactPrint.ghcWrapper $ ExceptT.runExceptT $ do
    dflags0                       <- lift $ ExactPrint.initDynFlagsPure fp str
    (dflags1, leftover, warnings) <- lift
      $ GHC.parseDynamicFlagsCmdLine dflags0 (GHC.noLoc <$> args)
    when (not $ null leftover)
      $  ExceptT.throwE
      $  "when parsing ghc flags: leftover flags: "
      ++ show (leftover <&> \(L _ s) -> s)
    when (not $ null warnings)
      $  ExceptT.throwE
      $  "when parsing ghc flags: encountered warnings: "
      ++ show (warnings <&> warnExtractorCompat)
    dynCheckRes <- ExceptT.ExceptT $ liftIO $ dynCheck dflags1
    let res = ExactPrint.parseModuleFromStringInternal dflags1 fp str
    case res of
      Left  err -> ExceptT.throwE $ "parse error: " ++ show (bagToList (show <$> err))
      Right (a   , m  ) -> pure (a, m, dynCheckRes)


commentAnnFixTransformGlob :: SYB.Data ast => ast -> ExactPrint.Transform ()
commentAnnFixTransformGlob ast = do
  let extract :: forall a . SYB.Data a => a -> Seq (SrcSpan, ExactPrint.AnnKey)
      extract = -- traceFunctionWith "extract" (show . SYB.typeOf) show $
        const Seq.empty
          `SYB.ext1Q`
            (\l@(L span _) -> Seq.singleton (span, ExactPrint.mkAnnKey l))
  let nodes = SYB.everything (<>) extract ast
  let annsMap :: Map GHC.RealSrcLoc ExactPrint.AnnKey
      annsMap = Map.fromListWith
        (flip const)
        [ (GHC.realSrcSpanEnd span, annKey)
        | (GHC.RealSrcSpan span _, annKey) <- Foldable.toList nodes
        ]
  nodes `forM_` (snd .> processComs annsMap)
 where
  processComs annsMap annKey1 = do
    mAnn <- State.Class.gets fst <&> Map.lookup annKey1
    mAnn `forM_` \ann1 -> do
      let priors  = ExactPrint.annPriorComments ann1
          follows = ExactPrint.annFollowingComments ann1
          assocs  = ExactPrint.annsDP ann1
      let
        processCom
          :: (ExactPrint.Comment, ExactPrint.DeltaPos)
          -> ExactPrint.TransformT Identity Bool
        processCom comPair@(com, _) =
          case GHC.realSrcSpanStart $ ExactPrint.commentIdentifier com of
            comLoc -> case Map.lookupLE comLoc annsMap of
              Just (_, annKey2) | loc1 /= loc2 -> case (con1, con2) of
                (ExactPrint.CN "RecordCon", ExactPrint.CN "HsRecField") ->
                  move $> False
                (x, y) | x == y -> move $> False
                _               -> return True
               where
                ExactPrint.AnnKey annKeyLoc1 con1 = annKey1
                ExactPrint.AnnKey annKeyLoc2 con2 = annKey2
                loc1                              = GHC.realSrcSpanStart annKeyLoc1
                loc2                              = GHC.realSrcSpanStart annKeyLoc2
                move = ExactPrint.modifyAnnsT $ \anns ->
                  let
                    ann2  = Data.Maybe.fromJust $ Map.lookup annKey2 anns
                    ann2' = ann2
                      { ExactPrint.annFollowingComments =
                          ExactPrint.annFollowingComments ann2 ++ [comPair]
                      }
                  in
                    Map.insert annKey2 ann2' anns
              _ -> return True -- retain comment at current node.
      priors'  <- flip filterM priors processCom
      follows' <- flip filterM follows $ processCom
      assocs'  <- flip filterM assocs $ \case
        (ExactPrint.AnnComment com, dp) -> processCom (com, dp)
        _                               -> return True
      let ann1' = ann1 { ExactPrint.annPriorComments     = priors'
                       , ExactPrint.annFollowingComments = follows'
                       , ExactPrint.annsDP               = assocs'
                       }
      ExactPrint.modifyAnnsT $ \anns -> Map.insert annKey1 ann1' anns


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
  modKey  = ExactPrint.mkAnnKey lmod
  output  = groupMap (\k _ -> Map.findWithDefault modKey k declMap) anns

groupMap :: (Ord k, Ord l) => (k -> a -> l) -> Map k a -> Map l (Map k a)
groupMap f = Map.foldlWithKey' (\m k a -> Map.alter (insert k a) (f k a) m)
                               Map.empty
 where
  insert k a Nothing  = Just (Map.singleton k a)
  insert k a (Just m) = Just (Map.insert k a m)

foldedAnnKeys :: Data.Data.Data ast => ast -> Set ExactPrint.AnnKey
foldedAnnKeys ast = SYB.everything
  Set.union
  ( \x -> maybe
    Set.empty
    Set.singleton
    [ SYB.gmapQi 1 (\t -> ExactPrint.mkAnnKey $ L l t) x
    | locTyCon == SYB.typeRepTyCon (SYB.typeOf x)
    , l :: SrcSpan <- SYB.gmapQi 0 SYB.cast x
      -- for some reason, ghc-8.8 has forgotten how to infer the type of l,
      -- even though it is passed to mkAnnKey above, which only accepts
      -- SrcSpan.
    ]
  )
  ast
  where locTyCon = SYB.typeRepTyCon (SYB.typeOf (L () ()))


withTransformedAnns
  :: Data ast
  => ast
  -> MultiRWSS.MultiRWS '[Config, ExactPrint.Anns] w s a
  -> MultiRWSS.MultiRWS '[Config, ExactPrint.Anns] w s a
withTransformedAnns ast m = MultiRWSS.mGetRawR >>= \case
  readers@(conf :+: anns :+: HNil) -> do
    -- TODO: implement `local` for MultiReader/MultiRWS
    MultiRWSS.mPutRawR (conf :+: f anns :+: HNil)
    x <- m
    MultiRWSS.mPutRawR readers
    pure x
 where
  f anns =
    let ((), (annsBalanced, _), _) =
          ExactPrint.runTransform anns (commentAnnFixTransformGlob ast)
    in  annsBalanced


warnExtractorCompat :: GHC.Warn -> String
warnExtractorCompat (GHC.Warn _ (L _ s)) = s
