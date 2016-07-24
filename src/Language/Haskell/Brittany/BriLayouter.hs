#define INSERTTRACESGETSPACING 0
#define INSERTTRACESALT 0

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Language.Haskell.Brittany.BriLayouter
  ( layoutBriDoc
  )
where



#include "prelude.inc"

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint.Utils

import qualified Data.StableMemo as StableMemo
import qualified Data.StableMemo.Weak as StableMemo.Weak
import qualified System.Mem.StableName as StableName
import qualified System.Unsafe as Unsafe

import           Language.Haskell.GHC.ExactPrint.Types ( AnnKey, Annotation )
import           Language.Haskell.Brittany.LayoutBasics
import           Language.Haskell.Brittany.Utils

import qualified Data.Text.Lazy.Builder as Text.Builder

import Language.Haskell.Brittany.Config.Types
import Language.Haskell.Brittany.Types

import           RdrName ( RdrName(..) )
import           GHC ( runGhc, GenLocated(L), moduleNameString )
import qualified Outputable    as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified SrcLoc        as GHC
import           SrcLoc ( SrcSpan )
import           OccName ( occNameString )
import           Name ( getOccString )
import           Module ( moduleName )
import           ApiAnnotation ( AnnKeywordId(..) )
import           Data.HList.ContainsType

import           Data.Data
import           Data.Generics.Schemes
import           Data.Generics.Aliases

import qualified Data.ByteString as B

import           DataTreePrint

import qualified Text.PrettyPrint as PP

import           Data.Function ( fix )

import           Control.Monad.Extra ( whenM )

import qualified Data.Generics.Uniplate.Data as Uniplate
-- import qualified Data.Generics.Uniplate as Uniplate



layoutBriDoc :: Data.Data.Data ast
             => ast
             -> BriDoc
             -> PPM ()
layoutBriDoc ast briDoc = do
  -- first step: transform the briDoc.
  briDoc' <- MultiRWSS.withMultiStateS briDoc $ do
    mGet >>= traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw . briDocToDoc
    -- bridoc transformation: remove alts
    mGet >>= transformAlts >>= mSet
    mGet >>= traceIfDumpConf "bridoc post-alt" _dconf_dump_bridoc_simpl_alt . briDocToDoc
    -- bridoc transformation: float stuff in
    mGet <&> transformSimplifyFloating >>= mSet
    mGet >>= traceIfDumpConf "bridoc post-floating" _dconf_dump_bridoc_simpl_floating . briDocToDoc
    -- bridoc transformation: par removal
    mGet <&> transformSimplifyPar >>= mSet
    mGet >>= traceIfDumpConf "bridoc post-par" _dconf_dump_bridoc_simpl_par . briDocToDoc
    -- bridoc transformation: float stuff in
    mGet <&> transformSimplifyColumns >>= mSet
    mGet >>= traceIfDumpConf "bridoc post-columns" _dconf_dump_bridoc_simpl_columns . briDocToDoc
    -- -- bridoc transformation: indent
    mGet <&> transformSimplifyIndent >>= mSet
    mGet >>= traceIfDumpConf "bridoc post-indent" _dconf_dump_bridoc_simpl_indent . briDocToDoc
    mGet >>= traceIfDumpConf "bridoc final" _dconf_dump_bridoc_final . briDocToDoc
    -- -- convert to Simple type
    -- simpl <- mGet <&> transformToSimple
    -- return simpl
  
  anns :: ExactPrint.Types.Anns <- mAsk
  let filteredAnns = filterAnns ast anns
  
  let state = LayoutState
        { _lstate_baseY          = 0
        , _lstate_curY           = 0
        , _lstate_indLevel       = 0
        , _lstate_indLevelLinger = 0
        , _lstate_commentsPrior = extractCommentsPrior filteredAnns
        , _lstate_commentsPost  = extractCommentsPost  filteredAnns
        , _lstate_commentCol  = Nothing
        , _lstate_addSepSpace = Nothing
        , _lstate_inhibitMTEL = False
        , _lstate_isNewline   = NewLineStateInit
        }

  state' <- MultiRWSS.withMultiStateS state
          $ layoutBriDocM briDoc'
  
  let remainingComments = Map.elems (_lstate_commentsPrior state')
                       ++ Map.elems (_lstate_commentsPost  state')
  remainingComments `forM_` (mTell . (:[]) . LayoutErrorUnusedComment . show . fmap fst)
  
  return $ ()

data AltCurPos = AltCurPos
  { _acp_line :: Int -- chars in the current line
  , _acp_indent :: Int -- current indentation level
  , _acp_forceMLFlag :: AltLineModeState
  }
  deriving (Show)

data AltLineModeState
  = AltLineModeStateNone
  | AltLineModeStateForceML Bool -- true ~ decays on next wrap
  | AltLineModeStateForceSL
  | AltLineModeStateContradiction
  -- i.e. ForceX False -> ForceX True -> None
  deriving (Show)

altLineModeDecay :: AltLineModeState -> AltLineModeState
altLineModeDecay AltLineModeStateNone = AltLineModeStateNone
altLineModeDecay (AltLineModeStateForceML False) = AltLineModeStateForceML True
altLineModeDecay (AltLineModeStateForceML True) = AltLineModeStateNone
altLineModeDecay AltLineModeStateForceSL = AltLineModeStateForceSL
altLineModeDecay AltLineModeStateContradiction = AltLineModeStateContradiction

altLineModeRefresh :: AltLineModeState -> AltLineModeState
altLineModeRefresh AltLineModeStateNone = AltLineModeStateNone
altLineModeRefresh AltLineModeStateForceML{} = AltLineModeStateForceML False
altLineModeRefresh AltLineModeStateForceSL = AltLineModeStateForceSL
altLineModeRefresh AltLineModeStateContradiction = AltLineModeStateContradiction

mergeLineMode :: AltCurPos -> AltLineModeState -> AltCurPos
mergeLineMode acp s = case (_acp_forceMLFlag acp, s) of
  (AltLineModeStateContradiction, _) -> acp
  (AltLineModeStateNone, x) -> acp { _acp_forceMLFlag = x }
  (AltLineModeStateForceSL, AltLineModeStateForceSL) -> acp
  (AltLineModeStateForceML{}, AltLineModeStateForceML{}) -> acp { _acp_forceMLFlag = s }
  _ -> acp { _acp_forceMLFlag = AltLineModeStateContradiction }

-- removes any BDAlt's from the BriDoc
transformAlts
  :: forall r w s
   . ( Data.HList.ContainsType.ContainsType Config r
     , Data.HList.ContainsType.ContainsType (Seq String) w
     )
  => BriDoc
  -> MultiRWSS.MultiRWS r w s BriDoc
transformAlts briDoc
    = MultiRWSS.withMultiStateA
        (AltCurPos 0 0 AltLineModeStateNone)
    $ rec briDoc
  where
    rec :: BriDoc -> MultiRWSS.MultiRWS r w (AltCurPos ': s) BriDoc
    rec brDc = do
#if INSERTTRACESALT
      tellDebugMess $ "transformAlts: visiting: " ++ show (toConstr brDc)
#endif
      -- debugAcp :: AltCurPos <- mGet
      case brDc of
        -- BDWrapAnnKey annKey bd -> do
        --   acp <- mGet
        --   mSet $ acp { _acp_forceMLFlag = altLineModeDecay $ _acp_forceMLFlag acp }
        --   BDWrapAnnKey annKey <$> rec bd
        bd@BDEmpty{}    -> processSpacingSimple bd $> bd
        bd@BDLit{}      -> processSpacingSimple bd $> bd
        BDSeq list      -> BDSeq <$> rec `mapM` list
        BDCols sig list -> BDCols sig <$> rec `mapM` list
        bd@BDSeparator  -> processSpacingSimple bd $> bd
        BDAddBaseY indent bd -> do
          acp <- mGet
          indAdd <- mAsk <&> _conf_layout .> _lconfig_indentAmount .> runIdentity
          let ind = case indent of
                BrIndentNone -> _acp_indent acp
                BrIndentRegular -> _acp_indent acp + indAdd
                BrIndentSpecial i -> _acp_indent acp + i
          mSet $ acp { _acp_indent = ind }
          r <- rec bd
          acp' <- mGet
          mSet $ acp' { _acp_indent = _acp_indent acp }
          return $ case indent of
            BrIndentNone -> r
            BrIndentRegular -> BDAddBaseY (BrIndentSpecial indAdd) r
            BrIndentSpecial i -> BDAddBaseY (BrIndentSpecial i) r
        BDSetBaseY bd -> do
          acp <- mGet
          mSet $ acp { _acp_indent = _acp_line acp }
          r <- rec bd
          acp' <- mGet
          mSet $ acp' { _acp_indent = _acp_indent acp }
          return $ BDSetBaseY r
        BDSetIndentLevel bd -> do
          BDSetIndentLevel <$> rec bd
        BDPar indent sameLine indented -> do
          acp <- mGet
          indAdd <- mAsk <&> _conf_layout .> _lconfig_indentAmount .> runIdentity
          let ind = case indent of
                BrIndentNone -> _acp_indent acp
                BrIndentRegular -> _acp_indent acp + indAdd
                BrIndentSpecial i -> _acp_indent acp + i
          mSet $ acp
            { _acp_indent = ind
            }
          sameLine' <- rec sameLine
          mSet $ acp
            { _acp_line   = ind
            , _acp_indent = ind
            }
          indented' <- rec indented
          return $ BDPar indent sameLine' indented'
        BDAlt [] -> error "empty BDAlt" -- returning BDEmpty instead is a
                                        -- possibility, but i will prefer a
                                        -- fail-early approach; BDEmpty does not
                                        -- make sense semantically for Alt[].
        BDAlt alts -> do
          altChooser <- mAsk <&> _conf_layout .> _lconfig_altChooser .> runIdentity
          case altChooser of
            AltChooserSimpleQuick -> do
              rec $ head alts
            AltChooserShallowBest -> do
              spacings <- alts `forM` getSpacing
              acp <- mGet
              let lineCheck LineModeInvalid = False
                  lineCheck (LineModeValid (VerticalSpacing _ p)) =
                    case _acp_forceMLFlag acp of
                      AltLineModeStateNone      -> True
                      AltLineModeStateForceSL{} -> Strict.isNothing p
                      AltLineModeStateForceML{} -> Strict.isJust    p
                      AltLineModeStateContradiction -> False
                  lineCheck _ = error "ghc exhaustive check is insufficient"
              lconf <- _conf_layout <$> mAsk
#if INSERTTRACESALT
              tellDebugMess $ "considering options with " ++ show (length alts, acp)
#endif
              let options = -- trace ("considering options:" ++ show (length alts, acp)) $
                            (zip spacings alts
                             <&> \(vs, bd) -> -- trace ("spacing=" ++ show vs ++ ",hasSpace=" ++ show (hasSpace lconf acp vs) ++ ",lineCheck=" ++ show (lineCheck vs))
                               ( hasSpace1 lconf acp vs && lineCheck vs, bd))
#if INSERTTRACESALT
              zip spacings options `forM_` \(vs, (_, bd)) ->
                tellDebugMess $ "  " ++ "spacing=" ++ show vs
                             ++ ",hasSpace=" ++ show (hasSpace1 lconf acp vs)
                             ++ ",lineCheck=" ++ show (lineCheck vs)
                             ++ " " ++ show (toConstr bd)
#endif
              id -- $ (fmap $ \x -> traceShow (briDocToDoc x) x)
                 $ rec
                 $ fromMaybe (-- trace ("choosing last") $
                              List.last alts)
                 $ Data.List.Extra.firstJust (\(_i::Int, (b,x)) ->
                     [ -- traceShow ("choosing option " ++ show i) $
                       x
                     | b
                     ])
                 $ zip [1..] options
            AltChooserBoundedSearch limit -> do
              spacings <- alts `forM` getSpacings limit
              acp <- mGet
              let lineCheck (VerticalSpacing _ p) =
                    case _acp_forceMLFlag acp of
                      AltLineModeStateNone      -> True
                      AltLineModeStateForceSL{} -> Strict.isNothing p
                      AltLineModeStateForceML{} -> Strict.isJust    p
                      AltLineModeStateContradiction -> False
              lconf <- _conf_layout <$> mAsk
#if INSERTTRACESALT
              tellDebugMess $ "considering options with " ++ show (length alts, acp)
#endif
              let options = -- trace ("considering options:" ++ show (length alts, acp)) $
                            (zip spacings alts
                             <&> \(vs, bd) -> -- trace ("spacing=" ++ show vs ++ ",hasSpace=" ++ show (hasSpace lconf acp vs) ++ ",lineCheck=" ++ show (lineCheck vs))
                               (  any (hasSpace2 lconf acp) vs
                               && any lineCheck vs, bd))
#if INSERTTRACESALT
              zip spacings options `forM_` \(vs, (_, bd)) ->
                tellDebugMess $ "  " ++ "spacing=" ++ show vs
                             ++ ",hasSpace=" ++ show (hasSpace2 lconf acp <$> vs)
                             ++ ",lineCheck=" ++ show (lineCheck <$> vs)
                             ++ " " ++ show (toConstr bd)
#endif
              id -- $ (fmap $ \x -> traceShow (briDocToDoc x) x)
                 $ rec
                 $ fromMaybe (-- trace ("choosing last") $
                              List.last alts)
                 $ Data.List.Extra.firstJust (\(_i::Int, (b,x)) ->
                     [ -- traceShow ("choosing option " ++ show i) $
                       x
                     | b
                     ])
                 $ zip [1..] options
        BDForceMultiline bd -> do
          acp <- mGet
          x <- do
            mSet $ mergeLineMode acp (AltLineModeStateForceML False)
            rec bd
          acp' <- mGet
          mSet $ acp' { _acp_forceMLFlag = _acp_forceMLFlag acp }
          return $ x
        BDForceSingleline bd -> do
          acp <- mGet
          x <- do
            mSet $ mergeLineMode acp AltLineModeStateForceSL
            rec bd
          acp' <- mGet
          mSet $ acp' { _acp_forceMLFlag = _acp_forceMLFlag acp }
          return $ x
        BDForwardLineMode bd -> do
          acp <- mGet
          x <- do
            mSet $ acp { _acp_forceMLFlag = altLineModeRefresh $ _acp_forceMLFlag acp }
            rec bd
          acp' <- mGet
          mSet $ acp' { _acp_forceMLFlag = _acp_forceMLFlag acp }
          return $ x
        bd@BDExternal{} -> processSpacingSimple bd $> bd
        BDAnnotationPrior annKey bd -> do
          acp <- mGet
          mSet $ acp { _acp_forceMLFlag = altLineModeDecay $ _acp_forceMLFlag acp }
          bd' <- rec bd
          return $ BDAnnotationPrior annKey bd'
        BDAnnotationPost annKey bd -> BDAnnotationPost annKey <$> rec bd
        BDLines [] -> return $ BDEmpty -- evil transformation. or harmless.
        BDLines (l:lr) -> do
          acp <- mGet
          let ind = _acp_indent acp
          l' <- rec l
          lr' <- lr `forM` \x -> do
            mSet $ acp
              { _acp_line   = ind
              , _acp_indent = ind
              }
            rec x
          return $ BDLines (l':lr')
        BDEnsureIndent indent bd -> BDEnsureIndent indent <$> rec bd
        BDProhibitMTEL bd -> BDProhibitMTEL <$> rec bd
    processSpacingSimple :: (MonadMultiReader
                                                     Config m,
                                                   MonadMultiState AltCurPos m, MonadMultiWriter (Seq String) m) => BriDoc -> m ()
    processSpacingSimple bd = getSpacing bd >>= \case
      LineModeInvalid                           -> error "processSpacingSimple inv"
      LineModeValid (VerticalSpacing _ Strict.Just{})  -> error "processSpacingSimple par"
      LineModeValid (VerticalSpacing i Strict.Nothing) -> do
        acp <- mGet
        mSet $ acp { _acp_line = _acp_line acp + i }
      _ -> error "ghc exhaustive check is insufficient"
    hasSpace1 :: LayoutConfig -> AltCurPos -> LineModeValidity VerticalSpacing -> Bool
    hasSpace1 _ _ LineModeInvalid = False
    hasSpace1 lconf (AltCurPos line _indent _) (LineModeValid (VerticalSpacing sameLine Strict.Nothing))
      = line + sameLine <= runIdentity (_lconfig_cols lconf)
    hasSpace1 lconf (AltCurPos line indent _) (LineModeValid (VerticalSpacing sameLine (Strict.Just par)))
      = line + sameLine <= runIdentity (_lconfig_cols lconf)
        && indent + par <= runIdentity (_lconfig_cols lconf)
    hasSpace1 _ _ _ = error "ghc exhaustive check is insufficient"
    hasSpace2 :: LayoutConfig -> AltCurPos -> VerticalSpacing -> Bool
    hasSpace2 lconf (AltCurPos line _indent _) (VerticalSpacing sameLine Strict.Nothing)
      = line + sameLine <= runIdentity (_lconfig_cols lconf)
    hasSpace2 lconf (AltCurPos line indent _) (VerticalSpacing sameLine (Strict.Just par))
      = line + sameLine <= runIdentity (_lconfig_cols lconf)
        && indent + par <= runIdentity (_lconfig_cols lconf)

getSpacing :: forall m . (MonadMultiReader Config m, MonadMultiWriter (Seq String) m) => BriDoc -> m (LineModeValidity VerticalSpacing)
getSpacing !bridoc = do
  !config <- mAsk
  let (w, x) = getSpacingMemo config bridoc
  mTell w
  return x

getSpacingMemo :: Config -> BriDoc -> (Seq String, LineModeValidity VerticalSpacing)
getSpacingMemo !config' !bridoc' = -- traceShow ((\x -> (toConstr x, StableName.hashStableName $ Unsafe.performIO $ StableName.makeStableName x)) <$> Uniplate.universe bridoc') $
                                   StableMemo.memo go config'
 where
  go :: Config -> (Seq String, LineModeValidity VerticalSpacing)
  go config = rec bridoc'
   where
    rec :: BriDoc -> (Seq String, LineModeValidity VerticalSpacing)
    rec = StableMemo.memo $ \bridoc -> -- traceShow ("getSpacingMemo1", toConstr bridoc, StableName.hashStableName $ Unsafe.performIO $ StableName.makeStableName bridoc) $
      let result = case bridoc of
            -- BDWrapAnnKey _annKey bd -> rec bd
            BDEmpty ->
              return $ LineModeValid $ VerticalSpacing 0 Strict.Nothing
            BDLit t ->
              return $ LineModeValid $ VerticalSpacing (Text.length t) Strict.Nothing
            BDSeq list ->
              sumVs <$> rec `mapM` list
            BDCols _sig list -> sumVs <$> rec `mapM` list
            BDSeparator ->
              return $ LineModeValid $ VerticalSpacing 1 Strict.Nothing
            BDAddBaseY indent bd -> do
              mVs <- rec bd
              return $ mVs <&> \vs -> vs
                { _vs_paragraph = _vs_paragraph vs <&> case indent of
                    BrIndentNone      -> id
                    BrIndentRegular   -> (+)
                                       $ runIdentity
                                       $ _lconfig_indentAmount
                                       $ _conf_layout
                                       $ config
                    BrIndentSpecial i -> (+ i)
                }
            BDSetBaseY bd -> do
              mVs <- rec bd
              return $ mVs <&> \vs -> vs
                -- We leave par as-is, even though it technically is not
                -- accurate (in general).
                -- the reason is that we really want to _keep_ it Just if it is
                -- just so we properly communicate the is-multiline fact.
                -- An alternative would be setting to (Just 0).
                { _vs_sameLine = max (_vs_sameLine vs)
                                     (Strict.fromMaybe 0 $ _vs_paragraph vs)
                }
            BDSetIndentLevel bd -> rec bd
            BDPar BrIndentNone sameLine indented -> do
              mVs <- rec sameLine
              indSp <- rec indented
              return $ [ VerticalSpacing lsp $ Strict.Just $ case mPsp of
                          Strict.Just psp -> max psp lineMax
                          Strict.Nothing  ->         lineMax
                       | VerticalSpacing lsp mPsp <- mVs
                       , lineMax <- getMaxVS $ indSp
                       ]
            BDPar{} -> error "BDPar with indent in getSpacing"
            BDAlt [] -> error "empty BDAlt"
            BDAlt (alt:_) -> rec alt
            BDForceMultiline  bd -> rec bd
            BDForceSingleline bd -> do
              mVs <- rec bd
              return $ mVs >>= \(VerticalSpacing _ psp) ->
                case psp of
                  Strict.Nothing -> mVs
                  Strict.Just{}  -> LineModeInvalid
            BDForwardLineMode bd -> rec bd
            BDExternal{} ->
              return $ LineModeValid $ VerticalSpacing 999 Strict.Nothing
            BDAnnotationPrior _annKey bd -> rec bd
            BDAnnotationPost  _annKey bd -> rec bd
            BDLines [] -> return $ LineModeValid $ VerticalSpacing 0 Strict.Nothing
            BDLines ls@(_:_) -> do
              lSps@(mVs:_) <- rec `mapM` ls
              return $ [ VerticalSpacing lsp $ Strict.Just $ lineMax
                       | VerticalSpacing lsp _ <- mVs
                       , lineMax <- getMaxVS $ maxVs $ lSps
                       ]
            BDEnsureIndent indent bd -> do
              mVs <- rec bd
              let addInd = case indent of
                    BrIndentNone      -> 0
                    BrIndentRegular   -> runIdentity
                                       $ _lconfig_indentAmount
                                       $ _conf_layout
                                       $ config
                    BrIndentSpecial i -> i
              return $ mVs <&> \(VerticalSpacing lsp psp) ->
                VerticalSpacing (lsp + addInd) psp
            BDProhibitMTEL bd -> rec bd
#if INSERTTRACESGETSPACING
          addition = Seq.singleton ("getSpacing: visiting: "
                            ++ show (toConstr bridoc)
                            ++ " -> "
                            ++ show result)
      in (fst result <> addition, snd result)
#else
      in -- traceShow ("getSpacingMemo2", toConstr bridoc, StableName.hashStableName $ Unsafe.performIO $ StableName.makeStableName bridoc) $
         result
#endif
    maxVs :: [LineModeValidity VerticalSpacing] -> LineModeValidity VerticalSpacing
    maxVs = foldl'
      (liftM2 (\(VerticalSpacing x1 x2) (VerticalSpacing y1 y2) ->
          VerticalSpacing (max x1 y1) (case (x2, y2) of
            (x, Strict.Nothing) -> x
            (Strict.Nothing, x) -> x
            (Strict.Just x, Strict.Just y) -> Strict.Just $ max x y)))
      (LineModeValid $ VerticalSpacing 0 Strict.Nothing)
    sumVs :: [LineModeValidity VerticalSpacing] -> LineModeValidity VerticalSpacing
    sumVs = foldl'
      (liftM2 (\(VerticalSpacing x1 x2) (VerticalSpacing y1 y2) ->
          VerticalSpacing (x1 + y1) (case (x2, y2) of
            (x, Strict.Nothing) -> x
            (Strict.Nothing, x) -> x
            (Strict.Just x, Strict.Just y) -> Strict.Just $ x + y)))
      (LineModeValid $ VerticalSpacing 0 Strict.Nothing)
    getMaxVS :: LineModeValidity VerticalSpacing -> LineModeValidity Int
    getMaxVS = fmap $ \(VerticalSpacing x1 x2) -> x1 `max` Strict.fromMaybe 0 x2

getSpacings :: forall m . (MonadMultiReader Config m, MonadMultiWriter (Seq String) m) => Int -> BriDoc -> m [VerticalSpacing]
getSpacings limit !bridoc = do
  !config <- mAsk
  let (w, x) = getSpacingsMemo (limit, config) bridoc
  mTell w
  return x

getSpacingsMemo :: (Int, Config) -> BriDoc -> (Seq String, [VerticalSpacing])
getSpacingsMemo !limitConfig' !bridoc' = -- traceShow ((\x -> (toConstr x, StableName.hashStableName $ Unsafe.performIO $ StableName.makeStableName x)) <$> Uniplate.universe bridoc') $
                                   StableMemo.memo go limitConfig'
 where
  go :: (Int, Config) -> (Seq String, [VerticalSpacing])
  go (limit, config) = rec bridoc'
   where
    rec :: BriDoc -> (Seq String, [VerticalSpacing])
    rec = StableMemo.memo $ \bridoc -> -- traceShow ("getSpacingMemo1", toConstr bridoc, StableName.hashStableName $ Unsafe.performIO $ StableName.makeStableName bridoc) $
      let result = case bridoc of
            -- BDWrapAnnKey _annKey bd -> rec bd
            BDEmpty ->
              return $ [VerticalSpacing 0 Strict.Nothing]
            BDLit t ->
              return $ [VerticalSpacing (Text.length t) Strict.Nothing]
            BDSeq list ->
              filterAndLimit . fmap sumVs . sequence <$> rec `mapM` list
            BDCols _sig list ->
              filterAndLimit . fmap sumVs . sequence <$> rec `mapM` list
            BDSeparator ->
              return $ [VerticalSpacing 1 Strict.Nothing]
            BDAddBaseY indent bd -> do
              mVs <- rec bd
              return $ mVs <&> \vs -> vs
                { _vs_paragraph = _vs_paragraph vs <&> case indent of
                    BrIndentNone      -> id
                    BrIndentRegular   -> (+)
                                       $ runIdentity
                                       $ _lconfig_indentAmount
                                       $ _conf_layout
                                       $ config
                    BrIndentSpecial i -> (+ i)
                }
            BDSetBaseY bd -> do
              mVs <- rec bd
              return $ mVs <&> \vs -> vs
                -- We leave par as-is, even though it technically is not
                -- accurate (in general).
                -- the reason is that we really want to _keep_ it Just if it is
                -- just so we properly communicate the is-multiline fact.
                -- An alternative would be setting to (Just 0).
                { _vs_sameLine = max (_vs_sameLine vs)
                                     (Strict.fromMaybe 0 $ _vs_paragraph vs)
                }
            BDSetIndentLevel bd -> rec bd
            BDPar BrIndentNone sameLine indented -> do
              mVss <- rec sameLine
              indSps <- rec indented
              let mVsIndSp = take limit
                           $ [ (x,y)
                             | x<-mVss
                             , y<-indSps
                             , hasOkColCount x
                             , hasOkColCount y
                             ]
              return $ mVsIndSp <&>
                \(VerticalSpacing lsp mPsp, indSp) ->
                  VerticalSpacing lsp $ Strict.Just $ case mPsp of
                    Strict.Just psp -> max psp $ getMaxVS indSp
                    Strict.Nothing  ->           getMaxVS indSp
            BDPar{} -> error "BDPar with indent in getSpacing"
            BDAlt [] -> error "empty BDAlt"
            -- BDAlt (alt:_) -> rec alt
            BDAlt alts -> filterAndLimit . join . transpose <$> rec `mapM` alts
            BDForceMultiline  bd -> rec bd
            BDForceSingleline bd -> do
              mVs <- rec bd
              return $ filter (Strict.isNothing . _vs_paragraph) mVs
            BDForwardLineMode bd -> rec bd
            BDExternal{} ->
              return $ [VerticalSpacing 999 Strict.Nothing]
            BDAnnotationPrior _annKey bd -> rec bd
            BDAnnotationPost  _annKey bd -> rec bd
            BDLines [] -> return $ [VerticalSpacing 0 Strict.Nothing]
            BDLines ls@(_:_) -> do
              lSpss@(mVs:_) <- rec `mapM` ls
              return $ case transpose lSpss of -- TODO: we currently only
                                   -- consider the first alternative for the
                                   -- line's spacings.
                                   -- also i am not sure if always including
                                   -- the first line length in the paragraph
                                   -- length gives the desired results.
                                   -- it is the safe path though, for now.
                []       -> []
                (lSps:_) -> mVs <&> \(VerticalSpacing lsp _) ->
                  VerticalSpacing lsp $ Strict.Just $ getMaxVS $ maxVs lSps
            BDEnsureIndent indent bd -> do
              mVs <- rec bd
              let addInd = case indent of
                    BrIndentNone      -> 0
                    BrIndentRegular   -> runIdentity
                                       $ _lconfig_indentAmount
                                       $ _conf_layout
                                       $ config
                    BrIndentSpecial i -> i
              return $ mVs <&> \(VerticalSpacing lsp psp) ->
                VerticalSpacing (lsp + addInd) psp
            BDProhibitMTEL bd -> rec bd
#if INSERTTRACESGETSPACING
          addition = Seq.fromList ["getSpacing: visiting: "
                                   ++ show (briDocToDoc bridoc)
                                  , " -> "
                                   ++ show (snd result)
                                  ]
      in (fst result <> addition, snd result)
#else
      in -- traceShow ("getSpacingMemo2", toConstr bridoc, StableName.hashStableName $ Unsafe.performIO $ StableName.makeStableName bridoc) $
         result
#endif
    maxVs :: [VerticalSpacing] -> VerticalSpacing
    maxVs = foldl'
      (\(VerticalSpacing x1 x2) (VerticalSpacing y1 y2) ->
          VerticalSpacing (max x1 y1) (case (x2, y2) of
            (x, Strict.Nothing) -> x
            (Strict.Nothing, x) -> x
            (Strict.Just x, Strict.Just y) -> Strict.Just $ max x y))
      (VerticalSpacing 0 Strict.Nothing)
    sumVs :: [VerticalSpacing] -> VerticalSpacing
    sumVs = foldl'
      (\(VerticalSpacing x1 x2) (VerticalSpacing y1 y2) ->
          VerticalSpacing (x1 + y1) (case (x2, y2) of
            (x, Strict.Nothing) -> x
            (Strict.Nothing, x) -> x
            (Strict.Just x, Strict.Just y) -> Strict.Just $ x + y))
      (VerticalSpacing 0 Strict.Nothing)
    getMaxVS :: VerticalSpacing -> Int
    getMaxVS (VerticalSpacing x1 x2) = x1 `max` Strict.fromMaybe 0 x2
    colMax = config & _conf_layout & _lconfig_cols & runIdentity
    hasOkColCount (VerticalSpacing lsp psp) =
      lsp <= colMax && Strict.maybe True (<=colMax) psp
    filterAndLimit :: [VerticalSpacing] -> [VerticalSpacing]
    filterAndLimit = take limit . filter hasOkColCount

-- note that this is not total, and cannot be with that exact signature.
mergeIndents :: BrIndent -> BrIndent -> BrIndent
mergeIndents BrIndentNone x = x
mergeIndents x BrIndentNone = x
mergeIndents (BrIndentSpecial i) (BrIndentSpecial j) = BrIndentSpecial (max i j)
mergeIndents _ _ = error "mergeIndents"

transformSimplifyFloating :: BriDoc -> BriDoc
transformSimplifyFloating = stepBO .> stepFull
  -- note that semantically, stepFull is completely sufficient.
  -- but the bottom-up switch-to-top-down-on-match transformation has much
  -- better complexity.
  where
    descendPost = Uniplate.descend $ \case
      -- post floating in
      BDAnnotationPost annKey1 (BDPar ind line indented) ->
        BDPar ind line $ BDAnnotationPost annKey1 indented
      BDAnnotationPost annKey1 (BDSeq list) ->
        BDSeq $ List.init list ++ [BDAnnotationPost annKey1 $ List.last list]
      BDAnnotationPost annKey1 (BDLines list) ->
        BDLines $ List.init list ++ [BDAnnotationPost annKey1 $ List.last list]
      BDAnnotationPost annKey1 (BDCols sig cols) ->
        BDCols sig $ List.init cols ++ [BDAnnotationPost annKey1 $ List.last cols]
      BDAnnotationPost annKey1 (BDAddBaseY indent x) ->
        BDAddBaseY indent $ BDAnnotationPost annKey1 x
      x -> x
    descendPrior = Uniplate.descend $ \case
      -- prior floating in
      BDAnnotationPrior annKey1 (BDPar ind line indented) ->
        BDPar ind (BDAnnotationPrior annKey1 line) indented
      BDAnnotationPrior annKey1 (BDSeq (l:lr)) ->
        BDSeq (BDAnnotationPrior annKey1 l:lr)
      BDAnnotationPrior annKey1 (BDLines (l:lr)) ->
        BDLines (BDAnnotationPrior annKey1 l:lr)
      BDAnnotationPrior annKey1 (BDCols sig (l:lr)) ->
        BDCols sig (BDAnnotationPrior annKey1 l:lr)
      BDAnnotationPrior annKey1 (BDAddBaseY indent x) ->
        BDAddBaseY indent $ BDAnnotationPrior annKey1 x
      x -> x
    descendAddB = Uniplate.descend $ \case
      -- AddIndent floats into Lines.
      BDAddBaseY BrIndentNone x ->
        x
      BDAddBaseY indent (BDLines lines) ->
        BDLines $ BDAddBaseY indent <$> lines
      -- AddIndent floats into last column
      BDAddBaseY indent (BDCols sig cols) ->
        BDCols sig $ List.init cols ++ [BDAddBaseY indent $ List.last cols]
      -- merge AddIndent and Par
      BDAddBaseY ind1 (BDPar ind2 line indented) ->
        BDPar (mergeIndents ind1 ind2) line indented
      BDAddBaseY ind (BDAnnotationPrior annKey1 x) ->
        BDAnnotationPrior annKey1 (BDAddBaseY ind x)
      BDAddBaseY ind (BDAnnotationPost annKey1 x) ->
        BDAnnotationPost annKey1 (BDAddBaseY ind x)
      BDAddBaseY ind (BDSeq list) ->
        BDSeq $ List.init list ++ [BDAddBaseY ind (List.last list)]
      x -> x
    stepBO = -- traceFunctionWith "stepB0" (show . briDocToDoc) (show . briDocToDoc) $
             Uniplate.transform $ \case
      -- post floating in
      BDAnnotationPost annKey1 (BDPar ind line indented) ->
        descendPost $ BDPar ind line $ BDAnnotationPost annKey1 indented
      BDAnnotationPost annKey1 (BDSeq list) ->
        descendPost $ BDSeq $ List.init list ++ [BDAnnotationPost annKey1 $ List.last list]
      BDAnnotationPost annKey1 (BDLines list) ->
        descendPost $ BDLines $ List.init list ++ [BDAnnotationPost annKey1 $ List.last list]
      BDAnnotationPost annKey1 (BDCols sig cols) ->
        descendPost $ BDCols sig $ List.init cols ++ [BDAnnotationPost annKey1 $ List.last cols]
      BDAnnotationPost annKey1 (BDAddBaseY indent x) ->
        descendPost $ BDAddBaseY indent $ BDAnnotationPost annKey1 x
      -- prior floating in
      BDAnnotationPrior annKey1 (BDPar ind line indented) ->
        descendPrior $ BDPar ind (BDAnnotationPrior annKey1 line) indented
      BDAnnotationPrior annKey1 (BDSeq (l:lr)) ->
        descendPrior $ BDSeq (BDAnnotationPrior annKey1 l:lr)
      BDAnnotationPrior annKey1 (BDLines (l:lr)) ->
        descendPrior $ BDLines (BDAnnotationPrior annKey1 l:lr)
      BDAnnotationPrior annKey1 (BDCols sig (l:lr)) ->
        descendPrior $ BDCols sig (BDAnnotationPrior annKey1 l:lr)
      BDAnnotationPrior annKey1 (BDAddBaseY indent x) ->
        descendPrior $ BDAddBaseY indent $ BDAnnotationPrior annKey1 x
      -- AddIndent floats into Lines.
      BDAddBaseY BrIndentNone x ->
        x
      BDAddBaseY indent (BDLines lines) ->
        descendAddB $ BDLines $ BDAddBaseY indent <$> lines
      -- AddIndent floats into last column
      BDAddBaseY indent (BDCols sig cols) ->
        descendAddB $ BDCols sig $ List.init cols ++ [BDAddBaseY indent $ List.last cols]
      -- merge AddIndent and Par
      BDAddBaseY ind1 (BDPar ind2 line indented) ->
        descendAddB $ BDPar (mergeIndents ind1 ind2) line indented
      BDAddBaseY ind (BDAnnotationPrior annKey1 x) ->
        descendAddB $ BDAnnotationPrior annKey1 (BDAddBaseY ind x)
      BDAddBaseY ind (BDAnnotationPost annKey1 x) ->
        descendAddB $ BDAnnotationPost annKey1 (BDAddBaseY ind x)
      BDAddBaseY ind (BDSeq list) ->
        descendAddB $ BDSeq $ List.init list ++ [BDAddBaseY ind (List.last list)]
      x -> x
    stepFull = Uniplate.rewrite $ \case
      -- AddIndent floats into Lines.
      BDAddBaseY BrIndentNone x ->
        Just x
      BDAddBaseY indent (BDLines lines) ->
        Just $ BDLines $ BDAddBaseY indent <$> lines
      -- AddIndent floats into last column
      BDAddBaseY indent (BDCols sig cols) ->
        Just $ BDCols sig $ List.init cols ++ [BDAddBaseY indent $ List.last cols]
      BDAddBaseY ind (BDSeq list) ->
        Just $ BDSeq $ List.init list ++ [BDAddBaseY ind (List.last list)]
      -- merge AddIndent and Par
      BDAddBaseY ind1 (BDPar ind2 line indented) ->
        Just $ BDPar (mergeIndents ind1 ind2) line indented
      -- prior floating in
      BDAnnotationPrior annKey1 (BDPar ind line indented) ->
        Just $ BDPar ind (BDAnnotationPrior annKey1 line) indented
      BDAnnotationPrior annKey1 (BDSeq (l:lr)) ->
        Just $ BDSeq ((BDAnnotationPrior annKey1 l):lr)
      BDAnnotationPrior annKey1 (BDLines (l:lr)) ->
        Just $ BDLines ((BDAnnotationPrior annKey1 l):lr)
      BDAnnotationPrior annKey1 (BDCols sig (l:lr)) ->
        Just $ BDCols sig ((BDAnnotationPrior annKey1 l):lr)
      -- EnsureIndent float-in
      -- BDEnsureIndent indent (BDCols sig (col:colr)) ->
      --   Just $ BDCols sig (BDEnsureIndent indent col : (BDAddBaseY indent <$> colr))
      -- not sure if the following rule is necessary; tests currently are
      -- unaffected.
      BDEnsureIndent indent (BDLines lines) ->
        Just $ BDLines $ BDEnsureIndent indent <$> lines
      -- post floating in
      BDAnnotationPost annKey1 (BDPar ind line indented) ->
        Just $ BDPar ind line $ BDAnnotationPost annKey1 indented
      BDAnnotationPost annKey1 (BDSeq list) ->
        Just $ BDSeq $ List.init list ++ [BDAnnotationPost annKey1 $ List.last list]
      BDAnnotationPost annKey1 (BDLines list) ->
        Just $ BDLines $ List.init list ++ [BDAnnotationPost annKey1 $ List.last list]
      BDAnnotationPost annKey1 (BDCols sig cols) ->
        Just $ BDCols sig $ List.init cols ++ [BDAnnotationPost annKey1 $ List.last cols]
      _ -> Nothing

transformSimplifyPar :: BriDoc -> BriDoc
transformSimplifyPar = Uniplate.rewrite $ \case
  -- BDPar BrIndentNone line1 line2 -> Just $ BDLines [line1, line2]
  -- BDPar line indented ->
  --   Just $ BDLines [line, indented]
  -- BDPar ind1 (BDPar ind2 line p1) p2 | ind1==ind2 ->
  --   Just $ BDPar ind1 line (BDLines [p1, p2])
  BDPar _ (BDPar _ BDPar{} _) _ -> Nothing
  BDPar ind1 (BDPar ind2 line p1) p2 ->
    Just $ BDPar ind1 line (BDLines [BDEnsureIndent ind2 p1, p2])
  BDLines lines | any (\case BDLines{} -> True
                             BDEmpty{} -> True
                             _ -> False) lines ->
    Just $ BDLines $ filter isNotEmpty $ lines >>= \case
      BDLines l -> l
      x -> [x]
  BDLines []  -> Just $ BDEmpty
  BDLines [x] -> Just $ x
  -- BDCols sig cols | BDPar ind line indented <- List.last cols ->
  --   Just $ BDPar ind (BDCols sig (List.init cols ++ [line])) indented
  -- BDPar BrIndentNone line indented ->
  --   Just $ BDLines [line, indented]
  BDEnsureIndent BrIndentNone x -> Just $ x
  _ -> Nothing

isNotEmpty :: BriDoc -> Bool
isNotEmpty BDEmpty = False
isNotEmpty _       = True

transformSimplifyColumns :: BriDoc -> BriDoc
transformSimplifyColumns = Uniplate.rewrite $ \case
  -- BDWrapAnnKey annKey bd ->
  --   BDWrapAnnKey annKey $ transformSimplify bd
  BDEmpty -> Nothing
  BDLit{} -> Nothing
  BDSeq list | any (\case BDSeq{} -> True
                          BDEmpty{} -> True
                          _ -> False) list -> Just $ BDSeq $
    filter isNotEmpty list >>= \case
      BDSeq l -> l
      x -> [x]
  BDLines lines | any (\case BDLines{} -> True
                             BDEmpty{} -> True
                             _ -> False) lines ->
    Just $ BDLines $ filter isNotEmpty $ lines >>= \case
      BDLines l -> l
      x -> [x]
  -- prior floating in
  BDAnnotationPrior annKey1 (BDSeq (l:lr)) ->
    Just $ BDSeq (BDAnnotationPrior annKey1 l:lr)
  BDAnnotationPrior annKey1 (BDLines (l:lr)) ->
    Just $ BDLines (BDAnnotationPrior annKey1 l:lr)
  BDAnnotationPrior annKey1 (BDCols sig (l:lr)) ->
    Just $ BDCols sig (BDAnnotationPrior annKey1 l:lr)
  -- post floating in
  BDAnnotationPost annKey1 (BDSeq list) ->
    Just $ BDSeq $ List.init list ++ [BDAnnotationPost annKey1 $ List.last list]
  BDAnnotationPost annKey1 (BDLines list) ->
    Just $ BDLines $ List.init list ++ [BDAnnotationPost annKey1 $ List.last list]
  BDAnnotationPost annKey1 (BDCols sig cols) ->
    Just $ BDCols sig $ List.init cols ++ [BDAnnotationPost annKey1 $ List.last cols]
  -- ensureIndent float-in
  -- not sure if the following rule is necessary; tests currently are
  -- unaffected.
  BDEnsureIndent indent (BDLines lines) ->
    Just $ BDLines $ BDEnsureIndent indent <$> lines
  -- matching col special transformation
  BDCols sig1 cols1@(_:_)
    | BDLines lines@(_:_:_) <- List.last cols1
    , BDCols sig2 cols2 <- List.last lines
    , sig1==sig2 ->
        Just $ BDLines
          [ BDCols sig1 $ List.init cols1 ++ [BDLines $ List.init lines]
          , BDCols sig2 cols2
          ]
  BDCols sig1 cols1@(_:_)
    | BDLines lines@(_:_:_) <- List.last cols1
    , BDEnsureIndent _ (BDCols sig2 cols2) <- List.last lines
    , sig1==sig2 ->
        Just $ BDLines
          [ BDCols sig1 $ List.init cols1 ++ [BDLines $ List.init lines]
          , BDCols sig2 cols2
          ]
  BDPar ind col1@(BDCols sig1 _) col2@(BDCols sig2 _) | sig1==sig2 ->
    Just $ BDAddBaseY ind (BDLines [col1, col2])
  BDPar ind col1@(BDCols sig1 _) (BDLines (col2@(BDCols sig2 _):rest))
    | sig1==sig2 ->
    Just $ BDPar ind (BDLines [col1, col2]) (BDLines rest)
  BDPar ind (BDLines lines1) col2@(BDCols sig2 _)
    | BDCols sig1 _ <- List.last lines1
    , sig1==sig2 ->
    Just $ BDAddBaseY ind (BDLines $ lines1 ++ [col2])
  BDPar ind (BDLines lines1) (BDLines (col2@(BDCols sig2 _):rest))
    | BDCols sig1 _ <- List.last lines1
    , sig1==sig2 ->
    Just $ BDPar ind (BDLines $ lines1 ++ [col2]) (BDLines rest)
  -- BDPar ind1 (BDCols sig1 cols1) (BDPar ind2 line (BDCols sig2 cols2))
  --   | sig1==sig2 ->
  --       Just $ BDPar
  --         ind1
  --         (BDLines [BDCols sig1 cols1, BDCols sig])
  BDCols sig1 cols | BDPar _ind line (BDCols sig2 cols2) <- List.last cols
                   , sig1==sig2 ->
    Just $ BDLines
      [ BDCols sig1 (List.init cols ++ [line])
      , BDCols sig2 cols2
      ]
  BDCols sig1 cols | BDPar ind line (BDLines lines) <- List.last cols
                   , BDCols sig2 cols2 <- List.last lines
                   , sig1==sig2 ->
    Just $ BDLines
      [ BDCols sig1 $ List.init cols ++ [BDPar ind line (BDLines $ List.init lines)]
      , BDCols sig2 cols2
      ]
  BDLines [x]         -> Just $ x
  BDLines []          -> Just $ BDEmpty
  BDSeq{}             -> Nothing
  BDCols{}            -> Nothing
  BDSeparator         -> Nothing
  BDAddBaseY{}        -> Nothing
  BDSetBaseY{}        -> Nothing
  BDSetIndentLevel{}  -> Nothing
  BDPar{}             -> Nothing
  BDAlt{}             -> Nothing
  BDForceMultiline{}  -> Nothing
  BDForceSingleline{} -> Nothing
  BDForwardLineMode{} -> Nothing
  BDExternal{}        -> Nothing
  BDLines{}           -> Nothing
  BDAnnotationPrior{} -> Nothing
  BDAnnotationPost{}  -> Nothing
  BDEnsureIndent{}    -> Nothing
  BDProhibitMTEL{}    -> Nothing

-- prepare layouting by translating BDPar's, replacing them with Indents and
-- floating those in. This gives a more clear picture of what exactly is
-- affected by what amount of indentation.
transformSimplifyIndent :: BriDoc -> BriDoc
transformSimplifyIndent = Uniplate.rewrite $ \case
  BDPar ind (BDLines lines) indented ->
    Just $ BDEnsureIndent ind $ BDLines $ lines ++ [indented]
  BDPar ind (BDCols sig cols) indented ->
    Just $ BDCols sig (List.init cols ++ [BDPar ind (List.last cols) indented])
  BDPar ind x indented ->
    Just $ BDLines
      [ BDAddBaseY ind x
      , BDEnsureIndent ind indented
      ]
  BDLines lines | any (\case BDLines{} -> True
                             BDEmpty{} -> True
                             _ -> False) lines ->
    Just $ BDLines $ filter isNotEmpty $ lines >>= \case
      BDLines l -> l
      x -> [x]
  BDAddBaseY i (BDAnnotationPost k x)  ->
    Just $ BDAnnotationPost k (BDAddBaseY i x)
  BDAddBaseY i (BDAnnotationPrior k x) ->
    Just $ BDAnnotationPrior k (BDAddBaseY i x)
  BDAddBaseY i (BDSeq l) ->
    Just $ BDSeq $ List.init l ++ [BDAddBaseY i $ List.last l]
  BDAddBaseY i (BDCols sig l) ->
    Just $ BDCols sig $ List.init l ++ [BDAddBaseY i $ List.last l]
  BDAddBaseY _ lit@BDLit{} ->
    Just lit

  _ -> Nothing


briDocLineLength :: BriDoc -> Int
briDocLineLength briDoc = flip StateS.evalState False $ rec briDoc
                          -- the state encodes whether a separate was already
                          -- appended at the current position.
 where
  rec = \case
    BDEmpty -> return $ 0
    BDLit t -> StateS.put False $> Text.length t
    BDSeq    bds -> sum <$> rec `mapM` bds
    BDCols _ bds -> sum <$> rec `mapM` bds
    BDSeparator -> StateS.get >>= \b -> StateS.put True $> if b then 0 else 1
    BDAddBaseY _ bd -> rec bd
    BDSetBaseY bd -> rec bd
    BDSetIndentLevel bd -> rec bd
    BDPar _ line _ -> rec line
    BDAlt{} -> error "briDocLineLength BDAlt"
    BDForceMultiline  bd -> rec bd
    BDForceSingleline bd -> rec bd
    BDForwardLineMode bd -> rec bd
    BDExternal _ _ _ t -> return $ Text.length t
    BDAnnotationPrior _ bd -> rec bd
    BDAnnotationPost  _ bd -> rec bd
    BDLines (l:_) -> rec l
    BDLines [] -> error "briDocLineLength BDLines []"
    BDEnsureIndent _ bd -> rec bd
    BDProhibitMTEL bd -> rec bd

layoutBriDocM
  :: forall w m
   . ( m ~ MultiRWSS.MultiRWST
             '[Config, ExactPrint.Types.Anns]
             w
             '[LayoutState]
             Identity
     , ContainsType Text.Builder.Builder w
     , ContainsType [LayoutError] w
     , ContainsType (Seq String) w
     )
  => BriDoc
  -> m ()
layoutBriDocM = \case
  BDEmpty -> do
    return () -- can it be that simple
  BDLit t -> do
    layoutRemoveIndentLevelLinger
    layoutWriteAppend t
  BDSeq list -> do
    list `forM_` layoutBriDocM
  -- in this situation, there is nothing to do about cols.
  -- i think this one does not happen anymore with the current simplifications.
  -- BDCols cSig list | BDPar sameLine lines <- List.last list ->
  --   alignColsPar $ BDCols cSig (List.init list ++ [sameLine]) : lines
  BDCols _ list -> do
    list `forM_` layoutBriDocM
  BDSeparator -> do
    layoutAddSepSpace
  BDAddBaseY indent bd -> do
    let indentF = case indent of
          BrIndentNone      -> id
          BrIndentRegular   -> layoutWithAddBaseCol
          BrIndentSpecial i -> layoutWithAddBaseColN i
    indentF $ layoutBriDocM bd
  BDSetBaseY bd -> do
    layoutSetBaseColCur $ layoutBriDocM bd
  BDSetIndentLevel bd -> do
    layoutSetIndentLevel $ layoutBriDocM bd
  BDEnsureIndent indent bd -> do
    let indentF = case indent of
          BrIndentNone      -> id
          BrIndentRegular   -> layoutWithAddBaseCol
          BrIndentSpecial i -> layoutWithAddBaseColN i
    indentF $ do
      layoutWriteEnsureBlock
      layoutBriDocM bd
  BDPar indent sameLine indented -> do
    layoutBriDocM sameLine
    let indentF = case indent of
          BrIndentNone      -> id
          BrIndentRegular   -> layoutWithAddBaseCol
          BrIndentSpecial i -> layoutWithAddBaseColN i
    indentF $ do
      layoutWriteNewlineBlock
      layoutBriDocM indented
  BDLines lines ->
    alignColsLines lines
  BDAlt [] -> error "empty BDAlt"
  BDAlt (alt:_) -> layoutBriDocM alt
  BDForceMultiline  bd -> layoutBriDocM bd
  BDForceSingleline bd -> layoutBriDocM bd
  BDForwardLineMode bd -> layoutBriDocM bd
  BDExternal annKey subKeys shouldAddComment t -> do
    let tlines = Text.lines $ t <> Text.pack "\n"
        tlineCount = length tlines
    anns :: ExactPrint.Types.Anns <- mAsk
    when shouldAddComment $ do
      layoutWriteAppend $ Text.pack $ "{-" ++ show (annKey, Map.lookup annKey anns) ++ "-}"
    zip [1..] tlines `forM_` \(i, l) -> do
      layoutWriteAppend $ l
      unless (i==tlineCount) layoutWriteNewlineBlock
    do
      state <- mGet
      let filterF k _ = not $ k `Set.member` subKeys
      mSet $ state
        { _lstate_commentsPrior = Map.filterWithKey filterF
                                $ _lstate_commentsPrior state
        , _lstate_commentsPost  = Map.filterWithKey filterF
                                $ _lstate_commentsPost  state
        }
  BDAnnotationPrior annKey bd -> do
    do
      state <- mGet
      let m   = _lstate_commentsPrior state
      let allowMTEL = not (_lstate_inhibitMTEL state)
                   && _lstate_isNewline state /= NewLineStateNo
      mAnn <- do
        let mAnn = Map.lookup annKey m
        mSet $ state { _lstate_commentsPrior = Map.delete annKey m }
        return mAnn
      case mAnn of
        Nothing -> when allowMTEL $ moveToExactAnn annKey
        Just [] -> when allowMTEL $ moveToExactAnn annKey
        Just priors -> do
          -- layoutResetSepSpace
          layoutSetCommentCol
          priors `forM_` \( ExactPrint.Types.Comment comment _ _
                          , ExactPrint.Types.DP (x, y)
                          ) -> do
            fixedX <- fixMoveToLineByIsNewline x
            replicateM_ fixedX layoutWriteNewline
            layoutMoveToIndentCol y
            -- layoutWriteAppend $ Text.pack $ replicate y ' '
            layoutWriteAppendMultiline $ Text.pack $ comment
          when allowMTEL $ moveToExactAnn annKey
          layoutIndentRestorePostComment
    layoutBriDocM bd
  BDAnnotationPost annKey bd -> do
    layoutBriDocM bd
    do
      mAnn <- do
        state <- mGet
        let m   = _lstate_commentsPost state
        let mAnn = Map.lookup annKey m
        mSet $ state { _lstate_commentsPost = Map.delete annKey m }
        return mAnn
      case mAnn of
        Nothing -> return ()
        Just posts -> do
          when (not $ null posts) $ layoutSetCommentCol
          posts `forM_` \( ExactPrint.Types.Comment comment _ _
                          , ExactPrint.Types.DP (x, y)
                          ) -> do
            fixedX <- fixMoveToLineByIsNewline x
            replicateM_ fixedX layoutWriteNewline
            -- layoutWriteAppend $ Text.pack $ replicate y ' '
            layoutMoveToIndentCol y
            layoutWriteAppendMultiline $ Text.pack $ comment
    layoutIndentRestorePostComment
  BDProhibitMTEL bd -> do
    -- set flag to True for this child, but disable afterwards.
    -- two hard aspects
    -- 1) nesting should be allowed. this means that resetting at the end must
    --    not indiscriminantely set to False, but take into account the
    --    previous value
    -- 2) nonetheless, newlines cancel inhibition. this means that if we ever
    --    find the flag set to False afterwards, we must not return it to
    --    the previous value, which might be True in the case of testing; it
    --    must remain False.
    state <- mGet
    mSet $ state { _lstate_inhibitMTEL = True }
    layoutBriDocM bd
    state' <- mGet
    when (_lstate_inhibitMTEL state') $ do
      mSet $ state' { _lstate_inhibitMTEL = _lstate_inhibitMTEL state }
  where
    -- alignColsPar :: [BriDoc]
    --           -> m ()
    -- alignColsPar l = colInfos `forM_` \colInfo -> do
    --     layoutWriteNewlineBlock
    --     processInfo (_cbs_map finalState) colInfo
    --   where
    --     (colInfos, finalState) = StateS.runState (mergeBriDocs l) (ColBuildState IntMapS.empty 0)
    alignColsLines :: [BriDoc]
              -> m ()
    alignColsLines l = -- colInfos `forM_` \colInfo -> do
        sequence_ $ List.intersperse layoutWriteNewlineBlock $ colInfos <&> processInfo (_cbs_map finalState)
      where
        (colInfos, finalState) = StateS.runState (mergeBriDocs l) (ColBuildState IntMapS.empty 0)
    briDocToColInfo :: BriDoc -> StateS.State ColBuildState ColInfo
    briDocToColInfo = \case
      BDCols sig list -> withAlloc $ \ind -> do
        subInfos <- mapM briDocToColInfo list
        return $ (briDocLineLength <$> list, ColInfo ind sig subInfos) -- TODO: replace 0
      bd -> return $ ColInfoNo bd

    mergeBriDocs :: [BriDoc] -> StateS.State ColBuildState [ColInfo]
    mergeBriDocs bds = mergeBriDocsW ColInfoStart bds

    mergeBriDocsW :: ColInfo -> [BriDoc] -> StateS.State ColBuildState [ColInfo]
    mergeBriDocsW _ [] = return []
    mergeBriDocsW lastInfo (bd:bdr) = do
      info <- mergeInfoBriDoc lastInfo bd
      infor <- mergeBriDocsW info bdr
      return $ info : infor

    mergeInfoBriDoc :: ColInfo
                    -> BriDoc
                    -> StateS.StateT ColBuildState Identity ColInfo
    mergeInfoBriDoc ColInfoStart = briDocToColInfo
    mergeInfoBriDoc ColInfoNo{}  = briDocToColInfo
    mergeInfoBriDoc (ColInfo infoInd infoSig subInfos) = \case
      bd@(BDCols colSig subDocs)
        | infoSig == colSig
        && length subInfos == length subDocs -> do
          infos <- zip subInfos subDocs
            `forM` uncurry mergeInfoBriDoc
          do -- update map
            s <- StateS.get
            let m = _cbs_map s
            let (Just spaces) = IntMapS.lookup infoInd m
            let new = briDocLineLength <$> subDocs
            StateS.put s
              { _cbs_map = IntMapS.insert infoInd
                                          (zipWith max spaces new)
                                          m
              }
          return $ ColInfo infoInd colSig infos
        | otherwise -> briDocToColInfo bd
      bd            -> return $ ColInfoNo bd
    
    withAlloc :: (ColIndex -> StateS.State ColBuildState (ColSpace, ColInfo))
              -> StateS.State ColBuildState ColInfo
    withAlloc f = do
      cbs <- StateS.get
      let ind = _cbs_index cbs
      StateS.put $ cbs { _cbs_index = ind + 1 }
      (space, info) <- f ind
      StateS.get >>= \c -> StateS.put
        $ c { _cbs_map = IntMapS.insert ind space $ _cbs_map c }
      return info

    processInfo :: ColMap -> ColInfo -> m ()
    processInfo m = \case
      ColInfoStart -> error "should not happen (TM)"
      ColInfoNo doc -> layoutBriDocM doc
      ColInfo ind _ list -> do
        curX <- do
          state <- mGet
          return $ _lstate_curY state + fromMaybe 0 (_lstate_addSepSpace state)
        -- tellDebugMess $ show curX
        let Just cols = IntMapS.lookup ind m
        let posXs = snd (mapAccumL (\acc x -> (acc+x,acc)) curX cols)
        zip posXs list `forM_` \(destX, x) -> do
          layoutWriteEnsureAbsoluteN destX
          processInfo m x

type ColIndex = Int
type ColSpace = [Int]
type ColMap = IntMapS.IntMap {- ColIndex -} ColSpace

data ColInfo
  = ColInfoStart -- start value to begin the mapAccumL.
  | ColInfoNo BriDoc
  | ColInfo ColIndex ColSig [ColInfo]

data ColBuildState = ColBuildState
  { _cbs_map :: ColMap
  , _cbs_index :: ColIndex
  }
