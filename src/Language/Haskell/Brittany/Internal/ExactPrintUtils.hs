{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.Internal.ExactPrintUtils
  ( parseModule
  , parseModuleFromString
  , commentAnnFixTransform
  , commentAnnFixTransformGlob
  , extractToplevelAnns
  , foldedAnnKeys
  )
where



#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.Brittany.Internal.Utils

import           DynFlags ( getDynFlags )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import qualified DynFlags      as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified Parser        as GHC
import qualified SrcLoc        as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified Lexer         as GHC
import qualified StringBuffer  as GHC
import qualified Outputable    as GHC
import           RdrName ( RdrName(..) )
import           HsSyn
import           SrcLoc ( SrcSpan, Located )
import           RdrName ( RdrName(..) )


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
    (dflags1, leftover, warnings) <- lift
      $ GHC.parseDynamicFlagsCmdLine dflags0 (GHC.noLoc <$> args)
    void $ lift $ GHC.setSessionDynFlags dflags1
    dflags2 <- lift $ ExactPrint.initDynFlags fp
    when (not $ null leftover)
      $  ExceptT.throwE
      $  "when parsing ghc flags: leftover flags: "
      ++ show (leftover <&> \(L _ s) -> s)
    when (not $ null warnings)
      $  ExceptT.throwE
      $  "when parsing ghc flags: encountered warnings: "
      ++ show (warnings <&> \(L _ s) -> s)
    x   <- ExceptT.ExceptT $ liftIO $ dynCheck dflags2
    res <- lift $ ExactPrint.parseModuleApiAnnsWithCppInternal cpp dflags2 fp
    either (\(span, err) -> ExceptT.throwE $ show span ++ ": " ++ err)
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
      ++ show (warnings <&> \(L _ s) -> s)
    dynCheckRes <- ExceptT.ExceptT $ liftIO $ dynCheck dflags1
    let res = parseModulePure dflags1 fp str
    case res of
      Left  (span, err) -> ExceptT.throwE $ show span ++ ": " ++ err
      Right (a   , m  ) -> pure (a, m, dynCheckRes)

-----------

-- this function should move to ghc-exactprint. btw, we can deprecate/remove
-- the `parseModuleFromString` function that I added initially to
-- ghc-exactprint.
parseModulePure
  :: GHC.DynFlags
  -> System.IO.FilePath
  -> String
  -> Either (SrcSpan, String) (ExactPrint.Anns, GHC.ParsedSource)
parseModulePure dflags fileName str =
  let (str1, lp) = ExactPrint.stripLinePragmas str
      res        = case runParser GHC.parseModule dflags fileName str1 of
        GHC.PFailed ss m    -> Left (ss, GHC.showSDoc dflags m)
        GHC.POk     x  pmod -> Right $ (mkApiAnns x, lp, dflags, pmod)
  in  ExactPrint.postParseTransform res ExactPrint.normalLayout

-- copied from exactprint until exactprint exposes a proper interface.
runParser
  :: GHC.P a
  -> GHC.DynFlags
  -> System.IO.FilePath
  -> String
  -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
 where
  location   = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
  buffer     = GHC.stringToStringBuffer str
  parseState = GHC.mkPState flags buffer location
mkApiAnns :: GHC.PState -> GHC.ApiAnns
mkApiAnns pstate =
  ( Map.fromListWith (++) . GHC.annotations $ pstate
  , Map.fromList
    ((GHC.noSrcSpan, GHC.comment_q pstate) : GHC.annotations_comments pstate)
  )

-----------

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
        | (GHC.RealSrcSpan span, annKey) <- Foldable.toList nodes
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
          case GHC.srcSpanStart $ ExactPrint.commentIdentifier com of
            GHC.UnhelpfulLoc{}    -> return True -- retain comment at current node.
            GHC.RealSrcLoc comLoc -> case Map.lookupLE comLoc annsMap of
              Just (_, annKey2) | loc1 /= loc2 -> case (con1, con2) of
                (ExactPrint.CN "RecordCon", ExactPrint.CN "HsRecField") ->
                  move $> False
                (x, y) | x == y -> move $> False
                _               -> return True
               where
                ExactPrint.AnnKey annKeyLoc1 con1 = annKey1
                ExactPrint.AnnKey annKeyLoc2 con2 = annKey2
                loc1                              = GHC.srcSpanStart annKeyLoc1
                loc2                              = GHC.srcSpanStart annKeyLoc2
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
  


commentAnnFixTransform :: GHC.ParsedSource -> ExactPrint.Transform ()
commentAnnFixTransform modul = SYB.everything (>>) genF modul
 where
  genF :: Data.Data.Data a => a -> ExactPrint.Transform ()
  genF = (\_ -> return ()) `SYB.extQ` exprF
  exprF :: Located (HsExpr RdrName) -> ExactPrint.Transform ()
  exprF lexpr@(L _ expr) = case expr of
    RecordCon _lname _ _ (HsRecFields fs@(_:_) Nothing) ->
      moveTrailingComments lexpr (List.last fs)
    RecordUpd _lname fs@(_:_) _ _ _ _ ->
      moveTrailingComments lexpr (List.last fs)
    _ -> return ()

moveTrailingComments :: (Data.Data.Data a,Data.Data.Data b)
                     => GHC.Located a -> GHC.Located b -> ExactPrint.Transform ()
moveTrailingComments astFrom astTo = do
  let
    k1 = ExactPrint.mkAnnKey astFrom
    k2 = ExactPrint.mkAnnKey astTo
    moveComments ans = ans'
      where
        an1 = Data.Maybe.fromJust $ Map.lookup k1 ans
        an2 = Data.Maybe.fromJust $ Map.lookup k2 ans
        cs1f = ExactPrint.annFollowingComments an1
        cs2f = ExactPrint.annFollowingComments an2
        (comments, nonComments) = flip breakEither (ExactPrint.annsDP an1)
             $ \case
               (ExactPrint.AnnComment com, dp) -> Left (com, dp)
               x -> Right x
        an1' = an1
          { ExactPrint.annsDP               = nonComments
          , ExactPrint.annFollowingComments = []
          }
        an2' = an2
          { ExactPrint.annFollowingComments = cs1f ++ cs2f ++ comments
          }
        ans' = Map.insert k1 an1' $ Map.insert k2 an2' ans

  ExactPrint.modifyAnnsT moveComments

-- | split a set of annotations in a module into a map from top-level module
-- elements to the relevant annotations. Avoids quadratic behaviour a trivial
-- implementation would have.
extractToplevelAnns
  :: Located (HsModule RdrName)
  -> ExactPrint.Anns
  -> Map ExactPrint.AnnKey ExactPrint.Anns
extractToplevelAnns lmod anns = output
 where
  (L _ (HsModule _ _ _ ldecls _ _)) = lmod
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
    , l <- SYB.gmapQi 0 SYB.cast x
    ]
  )
  ast
  where locTyCon = SYB.typeRepTyCon (SYB.typeOf (L () ()))
