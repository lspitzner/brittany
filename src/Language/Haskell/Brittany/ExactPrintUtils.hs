{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany.ExactPrintUtils
  ( parseModule
  , parseModuleFromString
  , commentAnnFixTransform
  )
where



#include "prelude.inc"

import DynFlags ( getDynFlags )
import GHC ( runGhc, GenLocated(L), moduleNameString )
import qualified Parser        as GHC
import qualified ApiAnnotation as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified HeaderInfo    as GHC
import qualified Lexer         as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC
import qualified Parser        as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC
import RdrName ( RdrName(..) )
import Control.Monad.IO.Class
import HsSyn
import SrcLoc ( SrcSpan, Located )
import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           Name
import qualified FastString
import           BasicTypes

import           ApiAnnotation ( AnnKeywordId(..) )
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Preprocess as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Delta as ExactPrint

import qualified Data.Generics as SYB

import qualified Data.Map as Map

import qualified Data.Text.Lazy.Builder as Text.Builder

import qualified Debug.Trace as Trace

import Language.Haskell.Brittany.Types
import Language.Haskell.Brittany.Config.Types
import Language.Haskell.Brittany.LayoutBasics



parseModule
  :: [String]
  -> System.IO.FilePath
  -> IO (Either String (ExactPrint.Anns, GHC.ParsedSource))
parseModule =
  parseModuleWithCpp ExactPrint.defaultCppOptions ExactPrint.normalLayout

-- | Parse a module with specific instructions for the C pre-processor.
parseModuleWithCpp
  :: ExactPrint.CppOptions
  -> ExactPrint.DeltaOptions
  -> [String]
  -> System.IO.FilePath
  -> IO (Either String (ExactPrint.Anns, GHC.ParsedSource))
parseModuleWithCpp cpp opts args fp =
  ExactPrint.ghcWrapper $ EitherT.runEitherT $ do
    dflags0                       <- lift $ ExactPrint.initDynFlags fp
    (dflags1, leftover, warnings) <- lift $ GHC.parseDynamicFlagsCmdLine
      dflags0
      (GHC.noLoc <$> args)
    when (not $ null leftover)
      $  EitherT.left
      $  "when parsing ghc flags: leftover flags: "
      ++ show (leftover <&> \(L _ s) -> s)
    when (not $ null warnings)
      $  EitherT.left
      $  "when parsing ghc flags: encountered warnings: "
      ++ show (warnings <&> \(L _ s) -> s)
    res <- lift $ ExactPrint.parseModuleApiAnnsWithCppInternal cpp dflags1 fp
    EitherT.hoistEither
      $ either (\(span, err) -> Left $ show span ++ ": " ++ err) Right
      $ ExactPrint.postParseTransform res opts

parseModuleFromString
  :: [String]
  -> System.IO.FilePath
  -> String
  -> IO (Either String (ExactPrint.Anns, GHC.ParsedSource))
parseModuleFromString args fp str =
  ExactPrint.ghcWrapper $ EitherT.runEitherT $ do
    dflags0                       <- lift $ ExactPrint.initDynFlagsPure fp str
    (dflags1, leftover, warnings) <-
      lift $ GHC.parseDynamicFlagsCmdLine dflags0 (GHC.noLoc <$> args)
    when (not $ null leftover)
      $  EitherT.left
      $  "when parsing ghc flags: leftover flags: "
      ++ show (leftover <&> \(L _ s) -> s)
    when (not $ null warnings)
      $  EitherT.left
      $  "when parsing ghc flags: encountered warnings: "
      ++ show (warnings <&> \(L _ s) -> s)
    EitherT.hoistEither
      $ either (\(span, err) -> Left $ show span ++ ": " ++ err) Right
      $ ExactPrint.parseWith dflags1 fp GHC.parseModule str

-----------

-- data LNode = forall a . LNode (Located a)
-- 
-- commentAnnFixTransformGlob :: GHC.ParsedSource -> ExactPrint.Transform ()
-- commentAnnFixTransformGlob modul = do
--   let extract :: forall a . SYB.Data a => a -> Seq LNode
--       extract = const Seq.empty `SYB.ext1Q` (Seq.singleton . LNode)
--   let nodes  = SYB.everything (<>) extract modul
--   let comp = _
--   let sorted = Seq.sortBy (comparing _) nodes
--   _

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
    breakHet :: (a -> Either b c) -> [a] -> ([b],[c])
    breakHet _ [] = ([],[])
    breakHet fn (a1:aR) = case fn a1 of
      Left  b -> (b:bs,cs)
      Right c -> (bs,c:cs)
     where
      (bs,cs) = breakHet fn aR
          
    k1 = ExactPrint.mkAnnKey astFrom
    k2 = ExactPrint.mkAnnKey astTo
    moveComments ans = ans'
      where
        an1 = Data.Maybe.fromJust $ Map.lookup k1 ans
        an2 = Data.Maybe.fromJust $ Map.lookup k2 ans
        cs1f = ExactPrint.annFollowingComments an1
        cs2f = ExactPrint.annFollowingComments an2
        (comments, nonComments) = flip breakHet (ExactPrint.annsDP an1)
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
