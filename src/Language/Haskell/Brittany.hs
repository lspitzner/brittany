{-# LANGUAGE DataKinds #-}

module Language.Haskell.Brittany
  ( parsePrintModule
  , pPrintModule
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
import GHC.Paths (libdir)
import HsSyn
import SrcLoc ( SrcSpan, Located )
-- import Outputable ( ppr, runSDoc )
-- import DynFlags ( unsafeGlobalDynFlags )

import           ApiAnnotation ( AnnKeywordId(..) )
import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Parsers as ExactPrint.Parsers
import qualified Language.Haskell.GHC.ExactPrint.Preprocess as ExactPrint.Preprocess

import qualified Data.Generics as SYB

import qualified Data.Map as Map

import qualified Data.Text.Lazy.Builder as Text.Builder

import qualified Debug.Trace as Trace

import Language.Haskell.Brittany.Types
import Language.Haskell.Brittany.Config.Types
import Language.Haskell.Brittany.LayoutBasics
import           Language.Haskell.Brittany.Layouters.Type
import           Language.Haskell.Brittany.Layouters.Decl
import           Language.Haskell.Brittany.Utils
import           Language.Haskell.Brittany.BriLayouter

import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import           SrcLoc ( SrcSpan )
import           HsSyn
import           Name
import qualified FastString
import           BasicTypes



-- LayoutErrors can be non-fatal warnings, thus both are returned instead
-- of an Either.
-- This should be cleaned up once it is clear what kinds of errors really
-- can occur.
pPrintModule
  :: Config
  -> ExactPrint.Types.Anns
  -> GHC.ParsedSource
  -> ([LayoutError], TextL.Text)
pPrintModule conf anns parsedModule =
  let ((), (annsBalanced, _), _) =
        ExactPrint.runTransform anns (commentAnnFixTransform parsedModule)
      ((out, errs), debugStrings)
        = runIdentity
        $ MultiRWSS.runMultiRWSTNil
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterAW
        $ MultiRWSS.withMultiWriterW
        $ MultiRWSS.withMultiReader annsBalanced
        $ MultiRWSS.withMultiReader conf
        $ do
            traceIfDumpConf "bridoc annotations" _dconf_dump_annotations $ annsDoc annsBalanced
            ppModule parsedModule
      tracer = if Seq.null debugStrings
        then id
        else trace ("---- DEBUGMESSAGES ---- ")
           . foldr (seq . join trace) id debugStrings
  in tracer $ (errs, Text.Builder.toLazyText out)
  -- unless () $ do
  --   
  --   debugStrings `forM_` \s ->
  --     trace s $ return ()

-- used for testing mostly, currently.
parsePrintModule
  :: Config
  -> String
  -> Text
  -> IO (Either String Text)
parsePrintModule conf filename input = do
  let inputStr = Text.unpack input
  parseResult <- ExactPrint.Parsers.parseModuleFromString filename inputStr
  return $ case parseResult of
    Left (_, s) -> Left $ "parsing error: " ++ s
    Right (anns, parsedModule) ->
      let (errs, ltext) = pPrintModule conf anns parsedModule
      in if null errs
        then Right $ TextL.toStrict $ ltext
        else
          let errStrs = errs <&> \case
                LayoutErrorUnusedComment str -> str
                LayoutWarning str -> str
                LayoutErrorUnknownNode str _ -> str
          in Left $ "pretty printing error(s):\n" ++ List.unlines errStrs

-- TODO: move to separate module
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
          
    k1 = ExactPrint.Types.mkAnnKey astFrom
    k2 = ExactPrint.Types.mkAnnKey astTo
    moveComments ans = ans'
      where
        an1 = Data.Maybe.fromJust $ Map.lookup k1 ans
        an2 = Data.Maybe.fromJust $ Map.lookup k2 ans
        cs1f = ExactPrint.Types.annFollowingComments an1
        cs2f = ExactPrint.Types.annFollowingComments an2
        (comments, nonComments) = flip breakHet (ExactPrint.Types.annsDP an1)
             $ \case
               (ExactPrint.Types.AnnComment com, dp) -> Left (com, dp)
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

-- this approach would for with there was a pure GHC.parseDynamicFilePragma.
-- Unfortunately that does not exist yet, so we cannot provide a nominally
-- pure interface.

-- parsePrintModule :: Text -> Either String Text
-- parsePrintModule input = do
--   let dflags = GHC.unsafeGlobalDynFlags
--   let fakeFileName = "SomeTestFakeFileName.hs"
--   let pragmaInfo = GHC.getOptions
--         dflags
--         (GHC.stringToStringBuffer $ Text.unpack input)
--         fakeFileName
--   (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags pragmaInfo
--   let parseResult = ExactPrint.Parsers.parseWith
--         dflags1
--         fakeFileName
--         GHC.parseModule
--         inputStr
--   case parseResult of
--     Left (_, s) -> Left $ "parsing error: " ++ s
--     Right (anns, parsedModule) -> do
--       let (out, errs) = runIdentity
--                       $ runMultiRWSTNil
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiWriterAW
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiWriterW
--                       $ Control.Monad.Trans.MultiRWS.Lazy.withMultiReader anns
--                       $ ppModule parsedModule
--       if (not $ null errs)
--         then do
--           let errStrs = errs <&> \case
--                 LayoutErrorUnusedComment str -> str
--           Left $ "pretty printing error(s):\n" ++ List.unlines errStrs
--         else return $ TextL.toStrict $ Text.Builder.toLazyText out

ppModule :: GenLocated SrcSpan (HsModule RdrName) -> PPM ()
ppModule lmod@(L loc m@(HsModule _name _exports _imports decls _ _)) = do
  let emptyModule = L loc m { hsmodDecls = [] }
  (anns', post) <- do
    anns <- mAsk
    -- evil partiality. but rather unlikely.
    return $ case Map.lookup (ExactPrint.Types.mkAnnKey lmod) anns of
      Nothing -> (anns, [])
      Just mAnn ->
        let
          modAnnsDp = ExactPrint.Types.annsDP mAnn
          isWhere (ExactPrint.Types.G AnnWhere) = True
          isWhere _ = False
          isEof   (ExactPrint.Types.G AnnEofPos) = True
          isEof   _ = False
          whereInd = List.findIndex (isWhere . fst) modAnnsDp
          eofInd   = List.findIndex (isEof   . fst) modAnnsDp
          (pre, post) = case (whereInd, eofInd) of
            (Nothing, Nothing) -> ([], modAnnsDp)
            (Just i, Nothing) -> List.splitAt (i+1) modAnnsDp
            (Nothing, Just _i) -> ([], modAnnsDp)
            (Just i, Just j) -> List.splitAt (min (i+1) j) modAnnsDp
          mAnn' = mAnn { ExactPrint.Types.annsDP = pre }
          anns' = Map.insert (ExactPrint.Types.mkAnnKey lmod) mAnn' anns
        in (anns', post)
  MultiRWSS.withMultiReader anns' $ processDefault emptyModule
  decls `forM_` ppDecl
  let
    finalComments = filter (fst .> \case ExactPrint.Types.AnnComment{} -> True
                                         _ -> False)
                           post
  post `forM_` \case
    (ExactPrint.Types.AnnComment (ExactPrint.Types.Comment cmStr _ _), l) -> do
      ppmMoveToExactLoc l
      mTell $ Text.Builder.fromString cmStr
    (ExactPrint.Types.G AnnEofPos, (ExactPrint.Types.DP (eofX,eofY))) ->
      let cmX = foldl' (\acc (_, ExactPrint.Types.DP (x, _)) -> acc+x) 0 finalComments
      in ppmMoveToExactLoc $ ExactPrint.Types.DP (eofX - cmX, eofY)
    _ -> return ()

ppDecl :: LHsDecl RdrName -> PPM ()
ppDecl d@(L loc decl) = case decl of
  SigD sig  -> -- trace (_sigHead sig) $
               do
    -- runLayouter $ Old.layoutSig (L loc sig)
    briDoc <- briDocMToPPM $ layoutSig (L loc sig)
    layoutBriDoc d briDoc
  ValD bind -> -- trace (_bindHead bind) $
               do
    -- Old.layoutBind (L loc bind)
    briDoc <- briDocMToPPM $ do
      eitherNode <- layoutBind (L loc bind)
      case eitherNode of
        Left ns -> docLines $ return <$> ns
        Right n -> return n
    layoutBriDoc d briDoc
  _         ->
    briDocMToPPM (briDocByExactNoComment d) >>= layoutBriDoc d

_sigHead :: Sig RdrName -> String
_sigHead = \case
  TypeSig names _ -> "TypeSig " ++ intercalate "," (Text.unpack . lrdrNameToText <$> names)
  _ -> "unknown sig"

_bindHead :: HsBind RdrName -> String
_bindHead = \case
  FunBind fId _ _ _ [] -> "FunBind " ++ (Text.unpack $ lrdrNameToText $ fId)
  PatBind _pat _ _ _ ([], []) -> "PatBind smth"
  _ -> "unknown bind"
