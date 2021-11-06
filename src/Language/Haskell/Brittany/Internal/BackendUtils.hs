{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}

module Language.Haskell.Brittany.Internal.BackendUtils
  ( layoutWriteAppend
  , layoutWriteAppendMultiline
  , layoutWriteNewlineBlock
  , layoutWriteNewline
  , layoutWriteEnsureNewlineBlock
  , layoutWriteEnsureBlock
  , layoutWithAddBaseCol
  , layoutWithAddBaseColBlock
  , layoutWithAddBaseColN
  , layoutWithAddBaseColNBlock
  , layoutBaseYPushCur
  , layoutBaseYPop
  , layoutIndentLevelPushCur
  , layoutIndentLevelPop
  , layoutWriteEnsureAbsoluteN
  , layoutAddSepSpace
  , layoutSetCommentCol
  , layoutMoveToCommentPos
  , layoutIndentRestorePostComment
  , moveToExactAnn
  , moveToY
  , ppmMoveToExactLoc
  , layoutWritePriorComments
  , layoutWritePostComments
  , layoutRemoveIndentLevelLinger
  )
where


#include "prelude.inc"

import           Language.Haskell.Brittany.Internal.Types
import           Language.Haskell.Brittany.Internal.Config.Types
import           Language.Haskell.GHC.ExactPrint.Types ( AnnKey
                                                       , Annotation
                                                       , KeywordId
                                                       )

import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import           Language.Haskell.Brittany.Internal.Utils

import           GHC ( Located, GenLocated(L), moduleNameString )



traceLocal
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m, Show a)
  => a
  -> m ()
traceLocal _ = return ()


layoutWriteAppend
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => Text
  -> m ()
layoutWriteAppend t = do
  traceLocal ("layoutWriteAppend", t)
  state <- mGet
  case _lstate_curYOrAddNewline state of
    Right i -> do
      replicateM_ i $ mTell $ Text.Builder.fromString $ "\n"
    Left{} -> do
      return ()
  let spaces = case _lstate_addSepSpace state of
        Just i -> i
        Nothing -> 0
  mTell $ Text.Builder.fromText $ Text.pack (replicate spaces ' ')
  mTell $ Text.Builder.fromText $ t
  mModify $ \s -> s
    { _lstate_curYOrAddNewline = Left $ case _lstate_curYOrAddNewline s of
        Left c -> c + Text.length t + spaces
        Right{} -> Text.length t + spaces
    , _lstate_addSepSpace = Nothing
    }

layoutWriteAppendSpaces
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => Int
  -> m ()
layoutWriteAppendSpaces i = do
  traceLocal ("layoutWriteAppendSpaces", i)
  unless (i == 0) $ do
    state <- mGet
    mSet $ state
      { _lstate_addSepSpace = Just $ maybe i (+i) $ _lstate_addSepSpace state
      }

layoutWriteAppendMultiline
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => [Text]
  -> m ()
layoutWriteAppendMultiline ts = do
  traceLocal ("layoutWriteAppendMultiline", ts)
  case ts of
    []     -> layoutWriteAppend (Text.pack "") -- need to write empty, too.
    (l:lr) -> do
      layoutWriteAppend l
      lr `forM_` \x -> do
        layoutWriteNewline
        layoutWriteAppend x

-- adds a newline and adds spaces to reach the base column.
layoutWriteNewlineBlock
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => m ()
layoutWriteNewlineBlock = do
  traceLocal ("layoutWriteNewlineBlock")
  state <- mGet
  mSet $ state { _lstate_curYOrAddNewline = Right 1
               , _lstate_addSepSpace      = Just $ lstate_baseY state
               }

-- layoutMoveToIndentCol :: ( MonadMultiState LayoutState m
--                     , MonadMultiWriter (Seq String) m) => Int -> m ()
-- layoutMoveToIndentCol i = do
-- #if INSERTTRACES
--   tellDebugMessShow ("layoutMoveToIndentCol", i)
-- #endif
--   state <- mGet
--   mSet $ state
--     { _lstate_addSepSpace = Just
--                           $ if isJust $ _lstate_addNewline state
--         then i
--         else _lstate_indLevelLinger state + i - _lstate_curY state
--     }

layoutSetCommentCol
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m) => m ()
layoutSetCommentCol = do
  state <- mGet
  let col = case _lstate_curYOrAddNewline state of
        Left i  -> i + fromMaybe 0 (_lstate_addSepSpace state)
        Right{} -> lstate_baseY state
  traceLocal ("layoutSetCommentCol", col)
  unless (Data.Maybe.isJust $ _lstate_commentCol state)
    $ mSet state { _lstate_commentCol = Just col }

-- This is also used to move to non-comments in a couple of places. Seems
-- to be harmless so far..
layoutMoveToCommentPos
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => Int
  -> Int
  -> Int
  -> m ()
layoutMoveToCommentPos y x commentLines = do
  traceLocal ("layoutMoveToCommentPos", y, x, commentLines)
  state <- mGet
  mSet state
    { _lstate_curYOrAddNewline = case _lstate_curYOrAddNewline state of
      Left i  -> if y == 0 then Left i else Right y
      Right{} -> Right y
    , _lstate_addSepSpace      =
      Just $ if Data.Maybe.isJust (_lstate_commentCol state)
        then case _lstate_curYOrAddNewline state of
          Left{}  -> if y == 0 then x else _lstate_indLevelLinger state + x
          Right{} -> _lstate_indLevelLinger state + x
        else if y == 0 then x else _lstate_indLevelLinger state + x
    , _lstate_commentCol       =
      Just $ case _lstate_commentCol state of
        Just existing -> existing
        Nothing -> case _lstate_curYOrAddNewline state of
          Left i  -> i + fromMaybe 0 (_lstate_addSepSpace state)
          Right{} -> lstate_baseY state
    , _lstate_commentNewlines =
        _lstate_commentNewlines state + y + commentLines - 1
    }

-- | does _not_ add spaces to again reach the current base column.
layoutWriteNewline
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => m ()
layoutWriteNewline = do
  traceLocal ("layoutWriteNewline")
  state <- mGet
  mSet $ state
    { _lstate_curYOrAddNewline = case _lstate_curYOrAddNewline state of
      Left{}  -> Right 1
      Right i -> Right (i + 1)
    , _lstate_addSepSpace      = Nothing
    }

_layoutResetCommentNewlines :: MonadMultiState LayoutState m => m ()
_layoutResetCommentNewlines = do
  mModify $ \state -> state { _lstate_commentNewlines = 0 }

layoutWriteEnsureNewlineBlock
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => m ()
layoutWriteEnsureNewlineBlock = do
  traceLocal ("layoutWriteEnsureNewlineBlock")
  state <- mGet
  mSet $ state
    { _lstate_curYOrAddNewline = case _lstate_curYOrAddNewline state of
      Left{}  -> Right 1
      Right i -> Right $ max 1 i
    , _lstate_addSepSpace      = Just $ lstate_baseY state
    , _lstate_commentCol       = Nothing
    }

layoutWriteEnsureAbsoluteN
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => Int
  -> m ()
layoutWriteEnsureAbsoluteN n = do
  state <- mGet
  let diff = case (_lstate_commentCol state, _lstate_curYOrAddNewline state) of
        (Just c , _      ) -> n - c
        (Nothing, Left i ) -> n - i
        (Nothing, Right{}) -> n
  traceLocal ("layoutWriteEnsureAbsoluteN", n, diff)
  when (diff > 0) $ do
    mSet $ state { _lstate_addSepSpace = Just diff -- this always sets to
                                            -- at least (Just 1), so we won't
                                            -- overwrite any old value in any
                                            -- bad way.
                 }

layoutBaseYPushInternal
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m)
  => Int
  -> m ()
layoutBaseYPushInternal i = do
  traceLocal ("layoutBaseYPushInternal", i)
  mModify $ \s -> s { _lstate_baseYs = i : _lstate_baseYs s }

layoutBaseYPopInternal
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m) => m ()
layoutBaseYPopInternal = do
  traceLocal ("layoutBaseYPopInternal")
  mModify $ \s -> s { _lstate_baseYs = List.tail $ _lstate_baseYs s }

layoutIndentLevelPushInternal
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m)
  => Int
  -> m ()
layoutIndentLevelPushInternal i = do
  traceLocal ("layoutIndentLevelPushInternal", i)
  mModify $ \s -> s { _lstate_indLevelLinger = lstate_indLevel s
                    , _lstate_indLevels      = i : _lstate_indLevels s
                    }

layoutIndentLevelPopInternal
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m) => m ()
layoutIndentLevelPopInternal = do
  traceLocal ("layoutIndentLevelPopInternal")
  mModify $ \s -> s { _lstate_indLevelLinger = lstate_indLevel s
                    , _lstate_indLevels      = List.tail $ _lstate_indLevels s
                    }

layoutRemoveIndentLevelLinger :: ( MonadMultiState LayoutState m
             , MonadMultiWriter (Seq String) m
             ) => m ()
layoutRemoveIndentLevelLinger = do
  mModify $ \s -> s { _lstate_indLevelLinger = lstate_indLevel s
                    }

layoutWithAddBaseCol
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiReader Config m
     , MonadMultiWriter (Seq String) m
     )
  => m ()
  -> m ()
layoutWithAddBaseCol m = do
  amount <- mAsk <&> _conf_layout .> _lconfig_indentAmount .> confUnpack
  state <- mGet
  layoutBaseYPushInternal $ lstate_baseY state + amount
  m
  layoutBaseYPopInternal

layoutWithAddBaseColBlock
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiReader Config m
     , MonadMultiWriter (Seq String) m
     )
  => m ()
  -> m ()
layoutWithAddBaseColBlock m = do
  amount <- mAsk <&> _conf_layout .> _lconfig_indentAmount .> confUnpack
  state <- mGet
  layoutBaseYPushInternal $ lstate_baseY state + amount
  layoutWriteEnsureBlock
  m
  layoutBaseYPopInternal

layoutWithAddBaseColNBlock
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => Int
  -> m ()
  -> m ()
layoutWithAddBaseColNBlock amount m = do
  traceLocal ("layoutWithAddBaseColNBlock", amount)
  state <- mGet
  layoutBaseYPushInternal $ lstate_baseY state + amount
  layoutWriteEnsureBlock
  m
  layoutBaseYPopInternal

layoutWriteEnsureBlock
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => m ()
layoutWriteEnsureBlock = do
  traceLocal ("layoutWriteEnsureBlock")
  state <- mGet
  let
    diff = case (_lstate_addSepSpace state, _lstate_curYOrAddNewline state) of
      (Nothing, Left i ) -> lstate_baseY state - i
      (Nothing, Right{}) -> lstate_baseY state
      (Just sp, Left i ) -> max sp (lstate_baseY state - i)
      (Just sp, Right{}) -> max sp (lstate_baseY state)
  -- when (diff>0) $ layoutWriteNewlineBlock
  when (diff > 0) $ do
    mSet $ state { _lstate_addSepSpace = Just $ diff }

layoutWithAddBaseColN
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => Int
  -> m ()
  -> m ()
layoutWithAddBaseColN amount m = do
  state <- mGet
  layoutBaseYPushInternal $ lstate_baseY state + amount
  m
  layoutBaseYPopInternal

layoutBaseYPushCur
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m) => m ()
layoutBaseYPushCur = do
  traceLocal ("layoutBaseYPushCur")
  state <- mGet
  case _lstate_commentCol state of
    Nothing ->
      case (_lstate_curYOrAddNewline state, _lstate_addSepSpace state) of
        (Left i , Just j ) -> layoutBaseYPushInternal (i + j)
        (Left i , Nothing) -> layoutBaseYPushInternal i
        (Right{}, _      ) -> layoutBaseYPushInternal $ lstate_baseY state
    Just cCol -> layoutBaseYPushInternal cCol

layoutBaseYPop
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m) => m ()
layoutBaseYPop = do
  traceLocal ("layoutBaseYPop")
  layoutBaseYPopInternal

layoutIndentLevelPushCur
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m) => m ()
layoutIndentLevelPushCur = do
  traceLocal ("layoutIndentLevelPushCur")
  state <- mGet
  let y = case (_lstate_curYOrAddNewline state, _lstate_addSepSpace state) of
        (Left i , Just j ) -> i + j
        (Left i , Nothing) -> i
        (Right{}, Just j ) -> j
        (Right{}, Nothing) -> 0
  layoutIndentLevelPushInternal y

layoutIndentLevelPop
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m) => m ()
layoutIndentLevelPop = do
  traceLocal ("layoutIndentLevelPop")
  layoutIndentLevelPopInternal
  -- why are comment indentations relative to the previous indentation on
  -- the first node of an additional indentation, and relative to the outer
  -- indentation after the last node of some indented stuff? sure does not
  -- make sense.
  layoutRemoveIndentLevelLinger

layoutAddSepSpace :: (MonadMultiState LayoutState m
  , MonadMultiWriter (Seq String) m)
   => m ()
layoutAddSepSpace = do
  state <- mGet
  mSet $ state
    { _lstate_addSepSpace = Just $ fromMaybe 1 $ _lstate_addSepSpace state }

-- TODO: when refactoring is complete, the other version of this method
-- can probably be removed.
moveToExactAnn
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiReader (Map AnnKey Annotation) m
     , MonadMultiWriter (Seq String) m
     )
  => AnnKey
  -> m ()
moveToExactAnn annKey = do
  traceLocal ("moveToExactAnn", annKey)
  anns <- mAsk
  case Map.lookup annKey anns of
    Nothing  -> return ()
    Just ann -> do
      -- curY <- mGet <&> _lstate_curY
      let ExactPrint.DP (y, _x) = ExactPrint.annEntryDelta ann
      -- mModify $ \state -> state { _lstate_addNewline = Just x }
      moveToY y

moveToY :: MonadMultiState LayoutState m => Int -> m ()
moveToY y = mModify $ \state ->
  let upd = case _lstate_curYOrAddNewline state of
        Left  i -> if y == 0 then Left i else Right y
        Right i -> Right $ max y i
  in  state
        { _lstate_curYOrAddNewline = upd
        , _lstate_addSepSpace      = if Data.Either.isRight upd
                                       then
                                         _lstate_commentCol state
                                         <|> _lstate_addSepSpace state
                                         <|> Just (lstate_baseY state)
                                       else Nothing
        , _lstate_commentCol       = Nothing
        }
-- fixMoveToLineByIsNewline :: MonadMultiState
--                                                   LayoutState m => Int -> m Int
-- fixMoveToLineByIsNewline x = do
--   newLineState <- mGet <&> _lstate_isNewline
--   return $ if newLineState == NewLineStateYes
--     then x-1
--     else x

ppmMoveToExactLoc
  :: MonadMultiWriter Text.Builder.Builder m
  => ExactPrint.DeltaPos
  -> m ()
ppmMoveToExactLoc (ExactPrint.DP (x, y)) = do
  replicateM_ x $ mTell $ Text.Builder.fromString "\n"
  replicateM_ y $ mTell $ Text.Builder.fromString " "

-- TODO: update and use, or clean up. Currently dead code.
layoutWritePriorComments
  :: ( Data.Data.Data ast
     , MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => Located ast
  -> m ()
layoutWritePriorComments ast = do
  mAnn <- do
    state <- mGet
    let key  = ExactPrint.mkAnnKey ast
    let anns = _lstate_comments state
    let mAnn = ExactPrint.annPriorComments <$> Map.lookup key anns
    mSet $ state
      { _lstate_comments =
          Map.adjust (\ann -> ann { ExactPrint.annPriorComments = [] }) key anns
      }
    return mAnn
  case mAnn of
    Nothing -> return ()
    Just priors -> do
      when (not $ null priors) $ layoutSetCommentCol
      priors `forM_` \( ExactPrint.Comment comment _ _
                      , ExactPrint.DP (x, y)
                      ) -> do
        replicateM_ x layoutWriteNewline
        layoutWriteAppendSpaces y
        layoutWriteAppendMultiline $ Text.lines $ Text.pack comment

-- TODO: update and use, or clean up. Currently dead code.
-- this currently only extracs from the `annsDP` field of Annotations.
-- per documentation, this seems sufficient, as the
-- "..`annFollowingComments` are only added by AST transformations ..".
layoutWritePostComments :: (Data.Data.Data ast,
                                               MonadMultiWriter Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                         => Located ast -> m ()
layoutWritePostComments ast = do
  mAnn <- do
    state <- mGet
    let key  = ExactPrint.mkAnnKey ast
    let anns = _lstate_comments state
    let mAnn = ExactPrint.annFollowingComments <$> Map.lookup key anns
    mSet $ state
      { _lstate_comments =
          Map.adjust (\ann -> ann { ExactPrint.annFollowingComments = [] })
                     key
                     anns
      }
    return mAnn
  case mAnn of
    Nothing -> return ()
    Just posts -> do
      when (not $ null posts) $ layoutSetCommentCol
      posts `forM_` \( ExactPrint.Comment comment _ _
                      , ExactPrint.DP (x, y)
                      ) -> do
        replicateM_ x layoutWriteNewline
        layoutWriteAppend $ Text.pack $ replicate y ' '
        mModify $ \s -> s { _lstate_addSepSpace = Nothing }
        layoutWriteAppendMultiline $ Text.lines $ Text.pack $ comment

layoutIndentRestorePostComment
  :: ( MonadMultiState LayoutState m
     , MonadMultiWriter Text.Builder.Builder m
     , MonadMultiWriter (Seq String) m
     )
  => m ()
layoutIndentRestorePostComment = do
  state <- mGet
  let mCommentCol = _lstate_commentCol state
  let eCurYAddNL  = _lstate_curYOrAddNewline state
  mModify $ \s -> s { _lstate_commentCol = Nothing
                    , _lstate_commentNewlines = 0
                    }
  case (mCommentCol, eCurYAddNL) of
    (Just commentCol, Left{}) -> do
      layoutWriteEnsureNewlineBlock
      layoutWriteEnsureAbsoluteN $ commentCol + fromMaybe 0 (_lstate_addSepSpace state)
    _                              -> return ()

-- layoutWritePriorCommentsRestore :: (Data.Data.Data ast,
--                                                MonadMultiWriter Text.Builder.Builder m,
--                                                MonadMultiState LayoutState m
--                                   , MonadMultiWriter (Seq String) m)
--                                 => Located ast -> m ()
-- layoutWritePriorCommentsRestore x = do
--   layoutWritePriorComments x
--   layoutIndentRestorePostComment
--
-- layoutWritePostCommentsRestore :: (Data.Data.Data ast,
--                                                MonadMultiWriter Text.Builder.Builder m,
--                                                MonadMultiState LayoutState m
--                                                , MonadMultiWriter (Seq String) m)
--                                 => Located ast -> m ()
-- layoutWritePostCommentsRestore x = do
--   layoutWritePostComments x
--   layoutIndentRestorePostComment
