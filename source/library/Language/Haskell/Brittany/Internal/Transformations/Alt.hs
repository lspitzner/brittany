{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.Brittany.Internal.Transformations.Alt where

import qualified Control.Monad.Memo as Memo
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import Data.HList.ContainsType
import qualified Data.List.Extra
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import qualified GHC.OldList as List
import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Prelude
import Language.Haskell.Brittany.Internal.PreludeUtils
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils



data AltCurPos = AltCurPos
  { _acp_line :: Int -- chars in the current line
  , _acp_indent :: Int -- current indentation level
  , _acp_indentPrep :: Int -- indentChange affecting the next Par
  , _acp_forceMLFlag :: AltLineModeState
  }
  deriving Show

data AltLineModeState
  = AltLineModeStateNone
  | AltLineModeStateForceML Bool -- true ~ decays on next wrap
  | AltLineModeStateForceSL
  | AltLineModeStateContradiction
  -- i.e. ForceX False -> ForceX True -> None
  deriving (Show)

altLineModeRefresh :: AltLineModeState -> AltLineModeState
altLineModeRefresh AltLineModeStateNone = AltLineModeStateNone
altLineModeRefresh AltLineModeStateForceML{} = AltLineModeStateForceML False
altLineModeRefresh AltLineModeStateForceSL = AltLineModeStateForceSL
altLineModeRefresh AltLineModeStateContradiction =
  AltLineModeStateContradiction

altLineModeDecay :: AltLineModeState -> AltLineModeState
altLineModeDecay AltLineModeStateNone = AltLineModeStateNone
altLineModeDecay (AltLineModeStateForceML False) = AltLineModeStateForceML True
altLineModeDecay (AltLineModeStateForceML True) = AltLineModeStateNone
altLineModeDecay AltLineModeStateForceSL = AltLineModeStateForceSL
altLineModeDecay AltLineModeStateContradiction = AltLineModeStateContradiction

mergeLineMode :: AltCurPos -> AltLineModeState -> AltCurPos
mergeLineMode acp s = case (_acp_forceMLFlag acp, s) of
  (AltLineModeStateContradiction, _) -> acp
  (AltLineModeStateNone, x) -> acp { _acp_forceMLFlag = x }
  (AltLineModeStateForceSL, AltLineModeStateForceSL) -> acp
  (AltLineModeStateForceML{}, AltLineModeStateForceML{}) ->
    acp { _acp_forceMLFlag = s }
  _ -> acp { _acp_forceMLFlag = AltLineModeStateContradiction }


-- removes any BDAlt's from the BriDoc
transformAlts
  :: forall r w s
   . ( Data.HList.ContainsType.ContainsType Config r
     , Data.HList.ContainsType.ContainsType (Seq String) w
     )
  => BriDocNumbered
  -> MultiRWSS.MultiRWS r w s BriDoc
transformAlts =
  MultiRWSS.withMultiStateA (AltCurPos 0 0 0 AltLineModeStateNone)
    . Memo.startEvalMemoT
    . fmap unwrapBriDocNumbered
    . rec
 where
    -- this function is exponential by nature and cannot be improved in any
    -- way i can think of, and i've tried. (stupid StableNames.)
    -- transWrap :: BriDoc -> BriDocNumbered
    -- transWrap brDc = flip StateS.evalState (1::Int)
    --                $ Memo.startEvalMemoT
    --                $ go brDc
    --   where
    --     incGet = StateS.get >>= \i -> StateS.put (i+1) $> i
    --     go :: BriDoc -> Memo.MemoT BriDoc BriDocNumbered (StateS.State Int) BriDocNumbered
    --     go = Memo.memo $ \bdX -> do
    --       i <- lift $ incGet
    --       fmap (\bd' -> (i,bd')) $ case bdX of
    --         BDEmpty           -> return $ BDFEmpty
    --         BDLit t           -> return $ BDFLit t
    --         BDSeq list        -> BDFSeq <$> go `mapM` list
    --         BDCols sig list   -> BDFCols sig <$> go `mapM` list
    --         BDSeparator       -> return $ BDFSeparator
    --         BDAddBaseY ind bd -> BDFAddBaseY ind <$> go bd
    --         BDSetBaseY bd     -> BDFSetBaseY <$> go bd
    --         BDSetIndentLevel bd     -> BDFSetIndentLevel <$> go bd
    --         BDPar ind line indented -> [ BDFPar ind line' indented'
    --                                    | line' <- go line
    --                                    , indented' <- go indented
    --                                    ]
    --         BDAlt alts              -> BDFAlt <$> go `mapM` alts -- not that this will happen
    --         BDForceMultiline  bd    -> BDFForceMultiline <$> go bd
    --         BDForceSingleline bd    -> BDFForceSingleline <$> go bd
    --         BDForwardLineMode bd    -> BDFForwardLineMode <$> go bd
    --         BDExternal k ks c t         -> return $ BDFExternal k ks c t
    --         BDAnnotationPrior annKey bd -> BDFAnnotationPrior annKey <$> go bd
    --         BDAnnotationPost  annKey bd -> BDFAnnotationRest  annKey <$> go bd
    --         BDLines lines         -> BDFLines <$> go `mapM` lines
    --         BDEnsureIndent ind bd -> BDFEnsureIndent ind <$> go bd
    --         BDProhibitMTEL bd     -> BDFProhibitMTEL <$> go bd



  rec
    :: BriDocNumbered
    -> Memo.MemoT
         Int
         [VerticalSpacing]
         (MultiRWSS.MultiRWS r w (AltCurPos ': s))
         BriDocNumbered
  rec bdX@(brDcId, brDc) = do
    let reWrap = (,) brDcId
    -- debugAcp :: AltCurPos <- mGet
    case brDc of
      -- BDWrapAnnKey annKey bd -> do
      --   acp <- mGet
      --   mSet $ acp { _acp_forceMLFlag = altLineModeDecay $ _acp_forceMLFlag acp }
      --   BDWrapAnnKey annKey <$> rec bd
      BDFEmpty{} -> processSpacingSimple bdX $> bdX
      BDFLit{} -> processSpacingSimple bdX $> bdX
      BDFSeq list -> reWrap . BDFSeq <$> list `forM` rec
      BDFCols sig list -> reWrap . BDFCols sig <$> list `forM` rec
      BDFSeparator -> processSpacingSimple bdX $> bdX
      BDFAddBaseY indent bd -> do
        acp <- mGet
        indAdd <- fixIndentationForMultiple acp indent
        mSet $ acp { _acp_indentPrep = max (_acp_indentPrep acp) indAdd }
        r <- rec bd
        acp' <- mGet
        mSet $ acp' { _acp_indent = _acp_indent acp }
        return $ case indent of
          BrIndentNone -> r
          BrIndentRegular -> reWrap $ BDFAddBaseY (BrIndentSpecial indAdd) r
          BrIndentSpecial i -> reWrap $ BDFAddBaseY (BrIndentSpecial i) r
      BDFBaseYPushCur bd -> do
        acp <- mGet
        mSet $ acp { _acp_indent = _acp_line acp }
        r <- rec bd
        return $ reWrap $ BDFBaseYPushCur r
      BDFBaseYPop bd -> do
        acp <- mGet
        r <- rec bd
        acp' <- mGet
        mSet $ acp' { _acp_indent = _acp_indentPrep acp }
        return $ reWrap $ BDFBaseYPop r
      BDFIndentLevelPushCur bd -> do
        reWrap . BDFIndentLevelPushCur <$> rec bd
      BDFIndentLevelPop bd -> do
        reWrap . BDFIndentLevelPop <$> rec bd
      BDFPar indent sameLine indented -> do
        indAmount <-
          mAsk <&> _conf_layout .> _lconfig_indentAmount .> confUnpack
        let
          indAdd = case indent of
            BrIndentNone -> 0
            BrIndentRegular -> indAmount
            BrIndentSpecial i -> i
        acp <- mGet
        let ind = _acp_indent acp + _acp_indentPrep acp + indAdd
        mSet $ acp { _acp_indent = ind, _acp_indentPrep = 0 }
        sameLine' <- rec sameLine
        mModify $ \acp' -> acp' { _acp_line = ind, _acp_indent = ind }
        indented' <- rec indented
        return $ reWrap $ BDFPar indent sameLine' indented'
      BDFAlt [] -> error "empty BDAlt" -- returning BDEmpty instead is a
                                      -- possibility, but i will prefer a
                                      -- fail-early approach; BDEmpty does not
                                      -- make sense semantically for Alt[].
      BDFAlt alts -> do
        altChooser <- mAsk <&> _conf_layout .> _lconfig_altChooser .> confUnpack
        case altChooser of
          AltChooserSimpleQuick -> do
            rec $ head alts
          AltChooserShallowBest -> do
            spacings <- alts `forM` getSpacing
            acp <- mGet
            let
              lineCheck LineModeInvalid = False
              lineCheck (LineModeValid (VerticalSpacing _ p _)) =
                case _acp_forceMLFlag acp of
                  AltLineModeStateNone -> True
                  AltLineModeStateForceSL{} -> p == VerticalSpacingParNone
                  AltLineModeStateForceML{} -> p /= VerticalSpacingParNone
                  AltLineModeStateContradiction -> False
              -- TODO: use COMPLETE pragma instead?
              lineCheck _ = error "ghc exhaustive check is insufficient"
            lconf <- _conf_layout <$> mAsk
            let
              options = -- trace ("considering options:" ++ show (length alts, acp)) $
                (zip spacings alts
                <&> \(vs, bd) -> -- trace ("spacing=" ++ show vs ++ ",hasSpace=" ++ show (hasSpace lconf acp vs) ++ ",lineCheck=" ++ show (lineCheck vs))
                                 (hasSpace1 lconf acp vs && lineCheck vs, bd)
                )
            rec
              $ fromMaybe (-- trace ("choosing last") $
                           List.last alts)
              $ Data.List.Extra.firstJust
                  (\(_i :: Int, (b, x)) ->
                    [ -- traceShow ("choosing option " ++ show i) $
                      x
                    | b
                    ]
                  )
              $ zip [1 ..] options
          AltChooserBoundedSearch limit -> do
            spacings <- alts `forM` getSpacings limit
            acp <- mGet
            let
              lineCheck (VerticalSpacing _ p _) = case _acp_forceMLFlag acp of
                AltLineModeStateNone -> True
                AltLineModeStateForceSL{} -> p == VerticalSpacingParNone
                AltLineModeStateForceML{} -> p /= VerticalSpacingParNone
                AltLineModeStateContradiction -> False
            lconf <- _conf_layout <$> mAsk
            let
              options = -- trace ("considering options:" ++ show (length alts, acp)) $
                (zip spacings alts
                <&> \(vs, bd) -> -- trace ("spacing=" ++ show vs ++ ",hasSpace=" ++ show (hasSpace lconf acp vs) ++ ",lineCheck=" ++ show (lineCheck vs))
                      (any (hasSpace2 lconf acp) vs && any lineCheck vs, bd)
                )
            let
              checkedOptions :: [Maybe (Int, BriDocNumbered)] =
                zip [1 ..] options <&> (\(i, (b, x)) -> [ (i, x) | b ])
            rec
              $ fromMaybe (-- trace ("choosing last") $
                           List.last alts)
              $ Data.List.Extra.firstJust (fmap snd) checkedOptions
      BDFForceMultiline bd -> do
        acp <- mGet
        x <- do
          mSet $ mergeLineMode acp (AltLineModeStateForceML False)
          rec bd
        acp' <- mGet
        mSet $ acp' { _acp_forceMLFlag = _acp_forceMLFlag acp }
        return $ x
      BDFForceSingleline bd -> do
        acp <- mGet
        x <- do
          mSet $ mergeLineMode acp AltLineModeStateForceSL
          rec bd
        acp' <- mGet
        mSet $ acp' { _acp_forceMLFlag = _acp_forceMLFlag acp }
        return $ x
      BDFForwardLineMode bd -> do
        acp <- mGet
        x <- do
          mSet $ acp
            { _acp_forceMLFlag = altLineModeRefresh $ _acp_forceMLFlag acp
            }
          rec bd
        acp' <- mGet
        mSet $ acp' { _acp_forceMLFlag = _acp_forceMLFlag acp }
        return $ x
      BDFExternal{} -> processSpacingSimple bdX $> bdX
      BDFPlain{} -> processSpacingSimple bdX $> bdX
      BDFAnnotationPrior annKey bd -> do
        acp <- mGet
        mSet
          $ acp { _acp_forceMLFlag = altLineModeDecay $ _acp_forceMLFlag acp }
        bd' <- rec bd
        return $ reWrap $ BDFAnnotationPrior annKey bd'
      BDFAnnotationRest annKey bd ->
        reWrap . BDFAnnotationRest annKey <$> rec bd
      BDFAnnotationKW annKey kw bd ->
        reWrap . BDFAnnotationKW annKey kw <$> rec bd
      BDFMoveToKWDP annKey kw b bd ->
        reWrap . BDFMoveToKWDP annKey kw b <$> rec bd
      BDFLines [] -> return $ reWrap BDFEmpty -- evil transformation. or harmless.
      BDFLines (l : lr) -> do
        ind <- _acp_indent <$> mGet
        l' <- rec l
        lr' <- lr `forM` \x -> do
          mModify $ \acp -> acp { _acp_line = ind, _acp_indent = ind }
          rec x
        return $ reWrap $ BDFLines (l' : lr')
      BDFEnsureIndent indent bd -> do
        acp <- mGet
        indAdd <- fixIndentationForMultiple acp indent
        mSet $ acp
          { _acp_indentPrep = 0
            -- TODO: i am not sure this is valid, in general.
          , _acp_indent = _acp_indent acp + indAdd
          , _acp_line = max (_acp_line acp) (_acp_indent acp + indAdd)
            -- we cannot use just _acp_line acp + indAdd because of the case
            -- where there are multiple BDFEnsureIndents in the same line.
            -- Then, the actual indentation is relative to the current
            -- indentation, not the current cursor position.
          }
        r <- rec bd
        acp' <- mGet
        mSet $ acp' { _acp_indent = _acp_indent acp }
        return $ case indent of
          BrIndentNone -> r
          BrIndentRegular ->
            reWrap $ BDFEnsureIndent (BrIndentSpecial indAdd) r
          BrIndentSpecial i -> reWrap $ BDFEnsureIndent (BrIndentSpecial i) r
      BDFNonBottomSpacing _ bd -> rec bd
      BDFSetParSpacing bd -> rec bd
      BDFForceParSpacing bd -> rec bd
      BDFDebug s bd -> do
        acp :: AltCurPos <- mGet
        tellDebugMess
          $ "transformAlts: BDFDEBUG "
          ++ s
          ++ " (node-id="
          ++ show brDcId
          ++ "): acp="
          ++ show acp
        reWrap . BDFDebug s <$> rec bd
  processSpacingSimple
    :: ( MonadMultiReader Config m
       , MonadMultiState AltCurPos m
       , MonadMultiWriter (Seq String) m
       )
    => BriDocNumbered
    -> m ()
  processSpacingSimple bd = getSpacing bd >>= \case
    LineModeInvalid -> error "processSpacingSimple inv"
    LineModeValid (VerticalSpacing i VerticalSpacingParNone _) -> do
      acp <- mGet
      mSet $ acp { _acp_line = _acp_line acp + i }
    LineModeValid VerticalSpacing{} -> error "processSpacingSimple par"
    _ -> error "ghc exhaustive check is insufficient"
  hasSpace1
    :: LayoutConfig -> AltCurPos -> LineModeValidity VerticalSpacing -> Bool
  hasSpace1 _ _ LineModeInvalid = False
  hasSpace1 lconf acp (LineModeValid vs) = hasSpace2 lconf acp vs
  hasSpace1 _ _ _ = error "ghc exhaustive check is insufficient"
  hasSpace2 :: LayoutConfig -> AltCurPos -> VerticalSpacing -> Bool
  hasSpace2 lconf (AltCurPos line _indent _ _) (VerticalSpacing sameLine VerticalSpacingParNone _)
    = line + sameLine <= confUnpack (_lconfig_cols lconf)
  hasSpace2 lconf (AltCurPos line indent indentPrep _) (VerticalSpacing sameLine (VerticalSpacingParSome par) _)
    = line
      + sameLine
      <= confUnpack (_lconfig_cols lconf)
      && indent
      + indentPrep
      + par
      <= confUnpack (_lconfig_cols lconf)
  hasSpace2 lconf (AltCurPos line _indent _ _) (VerticalSpacing sameLine VerticalSpacingParAlways{} _)
    = line + sameLine <= confUnpack (_lconfig_cols lconf)

getSpacing
  :: forall m
   . (MonadMultiReader Config m, MonadMultiWriter (Seq String) m)
  => BriDocNumbered
  -> m (LineModeValidity VerticalSpacing)
getSpacing !bridoc = rec bridoc
 where
  rec :: BriDocNumbered -> m (LineModeValidity VerticalSpacing)
  rec (brDcId, brDc) = do
    config <- mAsk
    let colMax = config & _conf_layout & _lconfig_cols & confUnpack
    result <- case brDc of
      -- BDWrapAnnKey _annKey bd -> rec bd
      BDFEmpty ->
        return $ LineModeValid $ VerticalSpacing 0 VerticalSpacingParNone False
      BDFLit t -> return $ LineModeValid $ VerticalSpacing
        (Text.length t)
        VerticalSpacingParNone
        False
      BDFSeq list -> sumVs <$> rec `mapM` list
      BDFCols _sig list -> sumVs <$> rec `mapM` list
      BDFSeparator ->
        return $ LineModeValid $ VerticalSpacing 1 VerticalSpacingParNone False
      BDFAddBaseY indent bd -> do
        mVs <- rec bd
        return $ mVs <&> \vs -> vs
          { _vs_paragraph = case _vs_paragraph vs of
            VerticalSpacingParNone -> VerticalSpacingParNone
            VerticalSpacingParAlways i ->
              VerticalSpacingParAlways $ case indent of
                BrIndentNone -> i
                BrIndentRegular ->
                  i
                    + (confUnpack
                      $ _lconfig_indentAmount
                      $ _conf_layout
                      $ config
                      )
                BrIndentSpecial j -> i + j
            VerticalSpacingParSome i -> VerticalSpacingParSome $ case indent of
              BrIndentNone -> i
              BrIndentRegular ->
                i + (confUnpack $ _lconfig_indentAmount $ _conf_layout $ config)
              BrIndentSpecial j -> i + j
          }
      BDFBaseYPushCur bd -> do
        mVs <- rec bd
        return $ mVs <&> \vs -> vs
          -- We leave par as-is, even though it technically is not
          -- accurate (in general).
          -- the reason is that we really want to _keep_ it Just if it is
          -- just so we properly communicate the is-multiline fact.
          -- An alternative would be setting to (Just 0).
          { _vs_sameLine = max
            (_vs_sameLine vs)
            (case _vs_paragraph vs of
              VerticalSpacingParNone -> 0
              VerticalSpacingParSome i -> i
              VerticalSpacingParAlways i -> min colMax i
            )
          , _vs_paragraph = VerticalSpacingParSome 0
          }
      BDFBaseYPop bd -> rec bd
      BDFIndentLevelPushCur bd -> rec bd
      BDFIndentLevelPop bd -> rec bd
      BDFPar BrIndentNone sameLine indented -> do
        mVs <- rec sameLine
        mIndSp <- rec indented
        return
          $ [ VerticalSpacing lsp pspResult parFlagResult
            | VerticalSpacing lsp mPsp _ <- mVs
            , indSp <- mIndSp
            , lineMax <- getMaxVS $ mIndSp
            , let
              pspResult = case mPsp of
                VerticalSpacingParSome psp ->
                  VerticalSpacingParSome $ max psp lineMax
                VerticalSpacingParNone -> VerticalSpacingParSome $ lineMax
                VerticalSpacingParAlways psp ->
                  VerticalSpacingParAlways $ max psp lineMax
            , let
              parFlagResult =
                mPsp
                  == VerticalSpacingParNone
                  && _vs_paragraph indSp
                  == VerticalSpacingParNone
                  && _vs_parFlag indSp
            ]
      BDFPar{} -> error "BDPar with indent in getSpacing"
      BDFAlt [] -> error "empty BDAlt"
      BDFAlt (alt : _) -> rec alt
      BDFForceMultiline bd -> do
        mVs <- rec bd
        return $ mVs >>= _vs_paragraph .> \case
          VerticalSpacingParNone -> LineModeInvalid
          _ -> mVs
      BDFForceSingleline bd -> do
        mVs <- rec bd
        return $ mVs >>= _vs_paragraph .> \case
          VerticalSpacingParNone -> mVs
          _ -> LineModeInvalid
      BDFForwardLineMode bd -> rec bd
      BDFExternal _ _ _ txt -> return $ LineModeValid $ case Text.lines txt of
        [t] -> VerticalSpacing (Text.length t) VerticalSpacingParNone False
        _ -> VerticalSpacing 999 VerticalSpacingParNone False
      BDFPlain txt -> return $ LineModeValid $ case Text.lines txt of
        [t] -> VerticalSpacing (Text.length t) VerticalSpacingParNone False
        _ -> VerticalSpacing 999 VerticalSpacingParNone False
      BDFAnnotationPrior _annKey bd -> rec bd
      BDFAnnotationKW _annKey _kw bd -> rec bd
      BDFAnnotationRest _annKey bd -> rec bd
      BDFMoveToKWDP _annKey _kw _b bd -> rec bd
      BDFLines [] ->
        return $ LineModeValid $ VerticalSpacing 0 VerticalSpacingParNone False
      BDFLines ls@(_ : _) -> do
        lSps <- rec `mapM` ls
        let (mVs : _) = lSps -- separated into let to avoid MonadFail
        return
          $ [ VerticalSpacing lsp (VerticalSpacingParSome $ lineMax) False
            | VerticalSpacing lsp _ _ <- mVs
            , lineMax <- getMaxVS $ maxVs $ lSps
            ]
      BDFEnsureIndent indent bd -> do
        mVs <- rec bd
        let
          addInd = case indent of
            BrIndentNone -> 0
            BrIndentRegular ->
              confUnpack $ _lconfig_indentAmount $ _conf_layout $ config
            BrIndentSpecial i -> i
        return $ mVs <&> \(VerticalSpacing lsp psp pf) ->
          VerticalSpacing (lsp + addInd) psp pf
      BDFNonBottomSpacing b bd -> do
        mVs <- rec bd
        return $ mVs <|> LineModeValid
          (VerticalSpacing
            0
            (if b
              then VerticalSpacingParSome 0
              else VerticalSpacingParAlways colMax
            )
            False
          )
      BDFSetParSpacing bd -> do
        mVs <- rec bd
        return $ mVs <&> \vs -> vs { _vs_parFlag = True }
      BDFForceParSpacing bd -> do
        mVs <- rec bd
        return
          $ [ vs
            | vs <- mVs
            , _vs_parFlag vs || _vs_paragraph vs == VerticalSpacingParNone
            ]
      BDFDebug s bd -> do
        r <- rec bd
        tellDebugMess
          $ "getSpacing: BDFDebug "
          ++ show s
          ++ " (node-id="
          ++ show brDcId
          ++ "): mVs="
          ++ show r
        return r
    return result
  maxVs
    :: [LineModeValidity VerticalSpacing] -> LineModeValidity VerticalSpacing
  maxVs = foldl'
    (liftM2
      (\(VerticalSpacing x1 x2 _) (VerticalSpacing y1 y2 _) -> VerticalSpacing
        (max x1 y1)
        (case (x2, y2) of
          (x, VerticalSpacingParNone) -> x
          (VerticalSpacingParNone, x) -> x
          (VerticalSpacingParAlways i, VerticalSpacingParAlways j) ->
            VerticalSpacingParAlways $ max i j
          (VerticalSpacingParAlways i, VerticalSpacingParSome j) ->
            VerticalSpacingParAlways $ max i j
          (VerticalSpacingParSome j, VerticalSpacingParAlways i) ->
            VerticalSpacingParAlways $ max i j
          (VerticalSpacingParSome x, VerticalSpacingParSome y) ->
            VerticalSpacingParSome $ max x y
        )
        False
      )
    )
    (LineModeValid $ VerticalSpacing 0 VerticalSpacingParNone False)
  sumVs
    :: [LineModeValidity VerticalSpacing] -> LineModeValidity VerticalSpacing
  sumVs sps = foldl' (liftM2 go) initial sps
   where
    go (VerticalSpacing x1 x2 x3) (VerticalSpacing y1 y2 _) = VerticalSpacing
      (x1 + y1)
      (case (x2, y2) of
        (x, VerticalSpacingParNone) -> x
        (VerticalSpacingParNone, x) -> x
        (VerticalSpacingParAlways i, VerticalSpacingParAlways j) ->
          VerticalSpacingParAlways $ i + j
        (VerticalSpacingParAlways i, VerticalSpacingParSome j) ->
          VerticalSpacingParAlways $ i + j
        (VerticalSpacingParSome i, VerticalSpacingParAlways j) ->
          VerticalSpacingParAlways $ i + j
        (VerticalSpacingParSome x, VerticalSpacingParSome y) ->
          VerticalSpacingParSome $ x + y
      )
      x3
    singleline (LineModeValid x) = _vs_paragraph x == VerticalSpacingParNone
    singleline _ = False
    isPar (LineModeValid x) = _vs_parFlag x
    isPar _ = False
    parFlag = case sps of
      [] -> True
      _ -> all singleline (List.init sps) && isPar (List.last sps)
    initial = LineModeValid $ VerticalSpacing 0 VerticalSpacingParNone parFlag
  getMaxVS :: LineModeValidity VerticalSpacing -> LineModeValidity Int
  getMaxVS = fmap $ \(VerticalSpacing x1 x2 _) -> x1 `max` case x2 of
    VerticalSpacingParSome i -> i
    VerticalSpacingParNone -> 0
    VerticalSpacingParAlways i -> i

data SpecialCompare = Unequal | Smaller | Bigger

getSpacings
  :: forall m
   . (MonadMultiReader Config m, MonadMultiWriter (Seq String) m)
  => Int
  -> BriDocNumbered
  -> Memo.MemoT Int [VerticalSpacing] m [VerticalSpacing]
getSpacings limit bridoc = preFilterLimit <$> rec bridoc
 where
    -- when we do `take K . filter someCondition` on a list of spacings, we
    -- need to first (also) limit the size of the input list, otherwise a
    -- _large_ input with a similarly _large_ prefix not passing our filtering
    -- process could lead to exponential runtime behaviour.
    -- TODO: 3 is arbitrary.
  preFilterLimit :: [VerticalSpacing] -> [VerticalSpacing]
  preFilterLimit = take (3 * limit)
  memoWithKey :: Memo.MonadMemo k v m1 => k -> m1 v -> m1 v
  memoWithKey k v = Memo.memo (const v) k
  rec :: BriDocNumbered -> Memo.MemoT Int [VerticalSpacing] m [VerticalSpacing]
  rec (brDcId, brdc) = memoWithKey brDcId $ do
    config <- mAsk
    let colMax = config & _conf_layout & _lconfig_cols & confUnpack
    let
      hasOkColCount (VerticalSpacing lsp psp _) = lsp <= colMax && case psp of
        VerticalSpacingParNone -> True
        VerticalSpacingParSome i -> i <= colMax
        VerticalSpacingParAlways{} -> True
    let
      specialCompare vs1 vs2 =
        if ((_vs_sameLine vs1 == _vs_sameLine vs2)
           && (_vs_parFlag vs1 == _vs_parFlag vs2)
           )
          then case (_vs_paragraph vs1, _vs_paragraph vs2) of
            (VerticalSpacingParAlways i1, VerticalSpacingParAlways i2) ->
              if i1 < i2 then Smaller else Bigger
            (p1, p2) -> if p1 == p2 then Smaller else Unequal
          else Unequal
    let
      allowHangingQuasiQuotes =
        config & _conf_layout & _lconfig_allowHangingQuasiQuotes & confUnpack
    let -- this is like List.nub, with one difference: if two elements
        -- are unequal only in _vs_paragraph, with both ParAlways, we
        -- treat them like equals and replace the first occurence with the
        -- smallest member of this "equal group".
      specialNub :: [VerticalSpacing] -> [VerticalSpacing]
      specialNub [] = []
      specialNub (x1 : xr) = case go x1 xr of
        (r, xs') -> r : specialNub xs'
       where
        go y1 [] = (y1, [])
        go y1 (y2 : yr) = case specialCompare y1 y2 of
          Unequal -> let (r, yr') = go y1 yr in (r, y2 : yr')
          Smaller -> go y1 yr
          Bigger -> go y2 yr
    let -- the standard function used to enforce a constant upper bound
        -- on the number of elements returned for each node. Should be
        -- applied whenever in a parent the combination of spacings from
        -- its children might cause excess of the upper bound.
      filterAndLimit :: [VerticalSpacing] -> [VerticalSpacing]
      filterAndLimit =
        take limit
                       -- prune so we always consider a constant
                       -- amount of spacings per node of the BriDoc.
          . specialNub
                       -- In the end we want to know if there is at least
                       -- one valid spacing for any alternative.
                       -- If there are duplicates in the list, then these
                       -- will either all be valid (so having more than the
                       -- first is pointless) or all invalid (in which
                       -- case having any of them is pointless).
                       -- Nonetheless I think the order of spacings should
                       -- be preserved as it provides a deterministic
                       -- choice for which spacings to prune (which is
                       -- an argument against simply using a Set).
                       -- I have also considered `fmap head . group` which
                       -- seems to work similarly well for common cases
                       -- and which might behave even better when it comes
                       -- to determinism of the algorithm. But determinism
                       -- should not be overrated here either - in the end
                       -- this is about deterministic behaviour of the
                       -- pruning we do that potentially results in
                       -- non-optimal layouts, and we'd rather take optimal
                       -- layouts when we can than take non-optimal layouts
                       -- just to be consistent with other cases where
                       -- we'd choose non-optimal layouts.
          . filter hasOkColCount
                       -- throw out any spacings (i.e. children) that
                       -- already use more columns than available in
                       -- total.
          . preFilterLimit
    result <- case brdc of
      -- BDWrapAnnKey _annKey bd -> rec bd
      BDFEmpty -> return $ [VerticalSpacing 0 VerticalSpacingParNone False]
      BDFLit t ->
        return $ [VerticalSpacing (Text.length t) VerticalSpacingParNone False]
      BDFSeq list -> fmap sumVs . mapM filterAndLimit <$> rec `mapM` list
      BDFCols _sig list -> fmap sumVs . mapM filterAndLimit <$> rec `mapM` list
      BDFSeparator -> return $ [VerticalSpacing 1 VerticalSpacingParNone False]
      BDFAddBaseY indent bd -> do
        mVs <- rec bd
        return $ mVs <&> \vs -> vs
          { _vs_paragraph = case _vs_paragraph vs of
            VerticalSpacingParNone -> VerticalSpacingParNone
            VerticalSpacingParAlways i ->
              VerticalSpacingParAlways $ case indent of
                BrIndentNone -> i
                BrIndentRegular ->
                  i
                    + (confUnpack
                      $ _lconfig_indentAmount
                      $ _conf_layout
                      $ config
                      )
                BrIndentSpecial j -> i + j
            VerticalSpacingParSome i -> VerticalSpacingParSome $ case indent of
              BrIndentNone -> i
              BrIndentRegular ->
                i + (confUnpack $ _lconfig_indentAmount $ _conf_layout $ config)
              BrIndentSpecial j -> i + j
          }
      BDFBaseYPushCur bd -> do
        mVs <- rec bd
        return $ mVs <&> \vs -> vs
          -- We leave par as-is, even though it technically is not
          -- accurate (in general).
          -- the reason is that we really want to _keep_ it Just if it is
          -- just so we properly communicate the is-multiline fact.
          -- An alternative would be setting to (Just 0).
          { _vs_sameLine = max
            (_vs_sameLine vs)
            (case _vs_paragraph vs of
              VerticalSpacingParNone -> 0
              VerticalSpacingParSome i -> i
              VerticalSpacingParAlways i -> min colMax i
            )
          , _vs_paragraph = case _vs_paragraph vs of
            VerticalSpacingParNone -> VerticalSpacingParNone
            VerticalSpacingParSome i -> VerticalSpacingParSome i
            VerticalSpacingParAlways i -> VerticalSpacingParAlways i
          }
      BDFBaseYPop bd -> rec bd
      BDFIndentLevelPushCur bd -> rec bd
      BDFIndentLevelPop bd -> rec bd
      BDFPar BrIndentNone sameLine indented -> do
        mVss <- filterAndLimit <$> rec sameLine
        indSps <- filterAndLimit <$> rec indented
        let mVsIndSp = take limit $ [ (x, y) | x <- mVss, y <- indSps ]
        return $ mVsIndSp <&> \(VerticalSpacing lsp mPsp _, indSp) ->
          VerticalSpacing
            lsp
            (case mPsp of
              VerticalSpacingParSome psp ->
                VerticalSpacingParSome $ max psp $ getMaxVS indSp -- TODO
              VerticalSpacingParNone -> spMakePar indSp
              VerticalSpacingParAlways psp ->
                VerticalSpacingParAlways $ max psp $ getMaxVS indSp
            )
            (mPsp
            == VerticalSpacingParNone
            && _vs_paragraph indSp
            == VerticalSpacingParNone
            && _vs_parFlag indSp
            )

      BDFPar{} -> error "BDPar with indent in getSpacing"
      BDFAlt [] -> error "empty BDAlt"
      -- BDAlt (alt:_) -> rec alt
      BDFAlt alts -> do
        r <- rec `mapM` alts
        return $ filterAndLimit =<< r
      BDFForceMultiline bd -> do
        mVs <- filterAndLimit <$> rec bd
        return $ filter ((/= VerticalSpacingParNone) . _vs_paragraph) mVs
      BDFForceSingleline bd -> do
        mVs <- filterAndLimit <$> rec bd
        return $ filter ((== VerticalSpacingParNone) . _vs_paragraph) mVs
      BDFForwardLineMode bd -> rec bd
      BDFExternal _ _ _ txt | [t] <- Text.lines txt ->
        return $ [VerticalSpacing (Text.length t) VerticalSpacingParNone False]
      BDFExternal{} -> return $ [] -- yes, we just assume that we cannot properly layout
                    -- this.
      BDFPlain t -> return
        [ case Text.lines t of
            [] -> VerticalSpacing 0 VerticalSpacingParNone False
            [t1] ->
              VerticalSpacing (Text.length t1) VerticalSpacingParNone False
            (t1 : _) ->
              VerticalSpacing (Text.length t1) (VerticalSpacingParAlways 0) True
        | allowHangingQuasiQuotes
        ]
      BDFAnnotationPrior _annKey bd -> rec bd
      BDFAnnotationKW _annKey _kw bd -> rec bd
      BDFAnnotationRest _annKey bd -> rec bd
      BDFMoveToKWDP _annKey _kw _b bd -> rec bd
      BDFLines [] -> return $ [VerticalSpacing 0 VerticalSpacingParNone False]
      BDFLines ls@(_ : _) -> do
        -- we simply assume that lines is only used "properly", i.e. in
        -- such a way that the first line can be treated "as a part of the
        -- paragraph". That most importantly means that Lines should never
        -- be inserted anywhere but at the start of the line. A
        -- counterexample would be anything like Seq[Lit "foo", Lines].
        lSpss <- map filterAndLimit <$> rec `mapM` ls
        let
          worbled = fmap reverse $ sequence $ reverse $ lSpss
          sumF lSps@(lSp1 : _) =
            VerticalSpacing (_vs_sameLine lSp1) (spMakePar $ maxVs lSps) False
          sumF [] =
            error
              $ "should not happen. if my logic does not fail"
              ++ "me, this follows from not (null ls)."
        return $ sumF <$> worbled
        -- lSpss@(mVs:_) <- rec `mapM` ls
        -- return $ case Control.Lens.transposeOf traverse lSpss of -- TODO: we currently only
        --                      -- consider the first alternative for the
        --                      -- line's spacings.
        --                      -- also i am not sure if always including
        --                      -- the first line length in the paragraph
        --                      -- length gives the desired results.
        --                      -- it is the safe path though, for now.
        --   []       -> []
        --   (lSps:_) -> mVs <&> \(VerticalSpacing lsp _) ->
        --     VerticalSpacing lsp $ VerticalSpacingParSome $ getMaxVS $ maxVs lSps
      BDFEnsureIndent indent bd -> do
        mVs <- rec bd
        let
          addInd = case indent of
            BrIndentNone -> 0
            BrIndentRegular ->
              confUnpack $ _lconfig_indentAmount $ _conf_layout $ config
            BrIndentSpecial i -> i
        return $ mVs <&> \(VerticalSpacing lsp psp parFlag) ->
          VerticalSpacing (lsp + addInd) psp parFlag
      BDFNonBottomSpacing b bd -> do
        -- TODO: the `b` flag is an ugly hack, but I was not able to make
        -- all tests work without it. It should be possible to have
        -- `spMakePar` map VSPAlways{} to VSPSome x1, which fixes this
        -- problem but breaks certain other cases.
        mVs <- rec bd
        return $ if null mVs
          then
            [ VerticalSpacing
                0
                (if b
                  then VerticalSpacingParSome 0
                  else VerticalSpacingParAlways colMax
                )
                False
            ]
          else mVs <&> \vs -> vs
            { _vs_sameLine = min colMax (_vs_sameLine vs)
            , _vs_paragraph = case _vs_paragraph vs of
              VerticalSpacingParNone -> VerticalSpacingParNone
              VerticalSpacingParAlways i
                | b -> VerticalSpacingParSome 0
                | otherwise -> VerticalSpacingParAlways i
              VerticalSpacingParSome i
                | b -> VerticalSpacingParSome 0
                | otherwise -> VerticalSpacingParAlways i
            }
          -- the version below is an alternative idea: fold the input
          -- spacings into a single spacing. This was hoped to improve in
          -- certain cases where non-bottom alternatives took up "too much
          -- explored search space"; the downside is that it also cuts
          -- the search-space short in other cases where it is not necessary,
          -- leading to unnecessary new-lines. Disabled for now. A better
          -- solution would require conditionally folding the search-space
          -- only in appropriate locations (i.e. a new BriDoc node type
          -- for this purpose, perhaps "BDFNonBottomSpacing1").
          -- else
          --   [ Foldable.foldl1
          --     (\(VerticalSpacing x1 x2 _) (VerticalSpacing y1 y2 _) ->
          --       VerticalSpacing
          --         (min x1 y1)
          --         (case (x2, y2) of
          --           (x, VerticalSpacingParNone) -> x
          --           (VerticalSpacingParNone, x) -> x
          --           (VerticalSpacingParAlways i, VerticalSpacingParAlways j) ->
          --             VerticalSpacingParAlways $ min i j
          --           (VerticalSpacingParAlways i, VerticalSpacingParSome j) ->
          --             VerticalSpacingParAlways $ min i j
          --           (VerticalSpacingParSome i, VerticalSpacingParAlways j) ->
          --             VerticalSpacingParAlways $ min i j
          --           (VerticalSpacingParSome x, VerticalSpacingParSome y) ->
          --             VerticalSpacingParSome $ min x y)
          --         False)
          --     mVs
          --   ]
      BDFSetParSpacing bd -> do
        mVs <- rec bd
        return $ mVs <&> \vs -> vs { _vs_parFlag = True }
      BDFForceParSpacing bd -> do
        mVs <- preFilterLimit <$> rec bd
        return
          $ [ vs
            | vs <- mVs
            , _vs_parFlag vs || _vs_paragraph vs == VerticalSpacingParNone
            ]
      BDFDebug s bd -> do
        r <- rec bd
        tellDebugMess
          $ "getSpacings: BDFDebug "
          ++ show s
          ++ " (node-id="
          ++ show brDcId
          ++ "): vs="
          ++ show (take 9 r)
        return r
    return result
  maxVs :: [VerticalSpacing] -> VerticalSpacing
  maxVs = foldl'
    (\(VerticalSpacing x1 x2 _) (VerticalSpacing y1 y2 _) -> VerticalSpacing
      (max x1 y1)
      (case (x2, y2) of
        (x, VerticalSpacingParNone) -> x
        (VerticalSpacingParNone, x) -> x
        (VerticalSpacingParAlways i, VerticalSpacingParAlways j) ->
          VerticalSpacingParAlways $ max i j
        (VerticalSpacingParAlways i, VerticalSpacingParSome j) ->
          VerticalSpacingParAlways $ max i j
        (VerticalSpacingParSome i, VerticalSpacingParAlways j) ->
          VerticalSpacingParAlways $ max i j
        (VerticalSpacingParSome x, VerticalSpacingParSome y) ->
          VerticalSpacingParSome $ max x y
      )
      False
    )
    (VerticalSpacing 0 VerticalSpacingParNone False)
  sumVs :: [VerticalSpacing] -> VerticalSpacing
  sumVs sps = foldl' go initial sps
   where
    go (VerticalSpacing x1 x2 x3) (VerticalSpacing y1 y2 _) = VerticalSpacing
      (x1 + y1)
      (case (x2, y2) of
        (x, VerticalSpacingParNone) -> x
        (VerticalSpacingParNone, x) -> x
        (VerticalSpacingParAlways i, VerticalSpacingParAlways j) ->
          VerticalSpacingParAlways $ i + j
        (VerticalSpacingParAlways i, VerticalSpacingParSome j) ->
          VerticalSpacingParAlways $ i + j
        (VerticalSpacingParSome i, VerticalSpacingParAlways j) ->
          VerticalSpacingParAlways $ i + j
        (VerticalSpacingParSome x, VerticalSpacingParSome y) ->
          VerticalSpacingParSome $ x + y
      )
      x3
    singleline x = _vs_paragraph x == VerticalSpacingParNone
    isPar x = _vs_parFlag x
    parFlag = case sps of
      [] -> True
      _ -> all singleline (List.init sps) && isPar (List.last sps)
    initial = VerticalSpacing 0 VerticalSpacingParNone parFlag
  getMaxVS :: VerticalSpacing -> Int
  getMaxVS (VerticalSpacing x1 x2 _) = x1 `max` case x2 of
    VerticalSpacingParSome i -> i
    VerticalSpacingParNone -> 0
    VerticalSpacingParAlways i -> i
  spMakePar :: VerticalSpacing -> VerticalSpacingPar
  spMakePar (VerticalSpacing x1 x2 _) = case x2 of
    VerticalSpacingParSome i -> VerticalSpacingParSome $ x1 `max` i
    VerticalSpacingParNone -> VerticalSpacingParSome $ x1
    VerticalSpacingParAlways i -> VerticalSpacingParAlways $ x1 `max` i

fixIndentationForMultiple
  :: (MonadMultiReader (CConfig Identity) m) => AltCurPos -> BrIndent -> m Int
fixIndentationForMultiple acp indent = do
  indAmount <- mAsk <&> _conf_layout .> _lconfig_indentAmount .> confUnpack
  let
    indAddRaw = case indent of
      BrIndentNone -> 0
      BrIndentRegular -> indAmount
      BrIndentSpecial i -> i
  -- for IndentPolicyMultiple, we restrict the amount of added
  -- indentation in such a manner that we end up on a multiple of the
  -- base indentation.
  indPolicy <- mAsk <&> _conf_layout .> _lconfig_indentPolicy .> confUnpack
  pure $ if indPolicy == IndentPolicyMultiple
    then
      let
        indAddMultiple1 =
          indAddRaw - ((_acp_indent acp + indAddRaw) `mod` indAmount)
        indAddMultiple2 = if indAddMultiple1 <= 0
          then indAddMultiple1 + indAmount
          else indAddMultiple1
      in indAddMultiple2
    else indAddRaw
