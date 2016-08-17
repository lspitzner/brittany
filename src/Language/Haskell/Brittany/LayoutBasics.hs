#define INSERTTRACES 0

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}
#if !INSERTTRACES
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

module Language.Haskell.Brittany.LayoutBasics
  ( processDefault
  , rdrNameToText
  , lrdrNameToText
  , lrdrNameToTextAnn
  , lrdrNameToTextAnnTypeEqualityIsSpecial
  , askIndent
  , layoutWriteAppend
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
  , layoutWritePriorComments
  , layoutWritePostComments
  , layoutRemoveIndentLevelLinger
  , extractAllComments
  , filterAnns
  , ppmMoveToExactLoc
  , docEmpty
  , docLit
  , docAlt
  , docAltFilter
  , docLines
  , docCols
  , docSeq
  , docPar
  , docNodeAnnKW
  , docWrapNode
  , docWrapNodePrior
  , docWrapNodeRest
  , docForceSingleline
  , docForceMultiline
  , docEnsureIndent
  , docAddBaseY
  , docSetBaseY
  , docSetIndentLevel
  , docSeparator
  , docAnnotationPrior
  , docAnnotationKW
  , docAnnotationRest
  , docNonBottomSpacing
  , docSetParSpacing
  , docForceParSpacing
  , docDebug
  , briDocByExact
  , briDocByExactNoComment
  , foldedAnnKeys
  , unknownNodeError
  , appSep
  , docCommaSep
  , docParenLSep
  , spacifyDocs
  , briDocMToPPM
  , allocateNode
  , docSharedWrapper
  , hasAnyCommentsBelow
  )
where



#include "prelude.inc"

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint.Utils

import Language.Haskell.GHC.ExactPrint.Types ( AnnKey, Annotation, KeywordId )

import qualified Data.Text.Lazy.Builder as Text.Builder

import Language.Haskell.Brittany.Config.Types
import Language.Haskell.Brittany.Types
import Language.Haskell.Brittany.Utils

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

import           Data.Data
import           Data.Generics.Schemes
import           Data.Generics.Aliases

import           DataTreePrint

import qualified Text.PrettyPrint as PP

import           Data.Function ( fix )


traceLocal
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m, Show a)
  => a
  -> m ()
#if INSERTTRACES
traceLocal x = do
  mGet >>= tellDebugMessShow @LayoutState
  tellDebugMessShow x
#else
traceLocal _ = return ()
#endif

processDefault :: (ExactPrint.Annotate.Annotate ast, MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiReader ExactPrint.Types.Anns m)
               => GenLocated SrcSpan ast
               -> m ()
processDefault x = do
  anns <- mAsk
  let str = ExactPrint.exactPrint x anns
  -- this hack is here so our print-empty-module trick does not add
  -- a newline at the start if there actually is no module header / imports
  -- / anything.
  -- TODO: instead the appropriate annotation could be removed when "cleaning"
  --       the module (header). This would remove the need for this hack!
  case str of
    "\n" -> return ()
    _    -> mTell $ Text.Builder.fromString $ str

briDocByExact :: (ExactPrint.Annotate.Annotate ast) => GenLocated SrcSpan ast -> ToBriDocM BriDocNumbered
briDocByExact ast = do
  anns <- mAsk
  traceIfDumpConf "ast" _dconf_dump_ast_unknown
    (printTreeWithCustom 100 (customLayouterF anns) ast)
  docExt ast anns True

briDocByExactNoComment :: (ExactPrint.Annotate.Annotate ast) => GenLocated SrcSpan ast -> ToBriDocM BriDocNumbered
briDocByExactNoComment ast = do
  anns <- mAsk
  traceIfDumpConf "ast" _dconf_dump_ast_unknown
    (printTreeWithCustom 100 (customLayouterF anns) ast)
  docExt ast anns False

rdrNameToText :: RdrName -> Text
-- rdrNameToText = Text.pack . show . flip runSDoc unsafeGlobalDynFlags . ppr
rdrNameToText ( Unqual occname     ) = Text.pack $ occNameString occname
rdrNameToText ( Qual mname occname ) = Text.pack $ moduleNameString mname 
                                                ++ "."
                                                ++ occNameString occname
rdrNameToText ( Orig modul occname ) = Text.pack $ moduleNameString (moduleName modul)
                                                ++ occNameString occname
rdrNameToText ( Exact name )         = Text.pack $ getOccString name

lrdrNameToText :: GenLocated l RdrName -> Text
lrdrNameToText (L _ n) = rdrNameToText n

lrdrNameToTextAnn :: ( MonadMultiReader Config m
                     , MonadMultiReader (Map AnnKey Annotation) m
                     )
                  => GenLocated SrcSpan RdrName
                  -> m Text
lrdrNameToTextAnn ast@(L _ n) = do
  anns <- mAsk
  let t = rdrNameToText n
  let hasUni x (ExactPrint.Types.G y, _) = x==y
      hasUni _ _ = False
  -- TODO: in general: we should _always_ process all annotaiton stuff here.
  --       whatever we don't probably should have had some effect on the
  --       output. in such cases, resorting to byExact is probably the safe
  --       choice.
  return $ case Map.lookup (ExactPrint.Types.mkAnnKey ast) anns of
    Nothing -> t
    Just (ExactPrint.Types.Ann _ _ _ aks _ _) -> case n of
      Exact{} | t == Text.pack "()" -> t
      _ | any (hasUni AnnBackquote) aks -> Text.pack "`" <> t <> Text.pack "`"
      _ | any (hasUni AnnCommaTuple) aks -> t
      _ | any (hasUni AnnOpenP)     aks -> Text.pack "(" <> t <> Text.pack ")"
      _ | otherwise -> t

lrdrNameToTextAnnTypeEqualityIsSpecial
  :: (MonadMultiReader Config m, MonadMultiReader (Map AnnKey Annotation) m)
  => GenLocated SrcSpan RdrName
  -> m Text
lrdrNameToTextAnnTypeEqualityIsSpecial ast = do
  x <- lrdrNameToTextAnn ast
  return $ if x == Text.pack "Data.Type.Equality~"
      then Text.pack "~" -- rraaaahhh special casing rraaahhhhhh
      else x

askIndent :: (MonadMultiReader Config m) => m Int
askIndent = confUnpack . _lconfig_indentAmount . _conf_layout <$> mAsk

layoutWriteAppend :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => Text
                  -> m ()
layoutWriteAppend t = do
  traceLocal ("layoutWriteAppend", t)
  state <- mGet
  case _lstate_curYOrAddNewline state of
    Right i -> do
#if INSERTTRACES
      tellDebugMessShow ("  inserted newlines: ", i)
#endif
      replicateM_ i $ mTell $ Text.Builder.fromString $ "\n"
    Left{} -> do
#if INSERTTRACES
      tellDebugMessShow ("  inserted no newlines")
#endif
      return ()
  let spaces = case _lstate_addSepSpace state of
        Just i -> i
        Nothing -> 0
#if INSERTTRACES
  tellDebugMessShow ("  inserted spaces: ", spaces)
#endif
  mTell $ Text.Builder.fromText $ Text.pack (replicate spaces ' ')
  mTell $ Text.Builder.fromText $ t
  mModify $ \s -> s
    { _lstate_curYOrAddNewline = Left $ case _lstate_curYOrAddNewline s of
        Left c -> c + Text.length t + spaces
        Right{} -> Text.length t + spaces
    , _lstate_addSepSpace = Nothing
    }

layoutWriteAppendSpaces :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => Int
                  -> m ()
layoutWriteAppendSpaces i = do
  traceLocal ("layoutWriteAppendSpaces", i)
  unless (i==0) $ do
    state <- mGet
    mSet $ state { _lstate_addSepSpace = Just
                                       $ maybe i (+i)
                                       $ _lstate_addSepSpace state
                 }

layoutWriteAppendMultiline :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => Text
                  -> m ()
layoutWriteAppendMultiline t = do
  traceLocal ("layoutWriteAppendMultiline", t)
  case Text.lines t of
    [] ->
      layoutWriteAppend t -- need to write empty, too.
    (l:lr) -> do
      layoutWriteAppend l
      lr `forM_` \x -> do
        layoutWriteNewline
        layoutWriteAppend x

-- adds a newline and adds spaces to reach the base column.
layoutWriteNewlineBlock :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => m ()
layoutWriteNewlineBlock = do
  traceLocal ("layoutWriteNewlineBlock")
  state <- mGet
  mSet $ state { _lstate_curYOrAddNewline = Right 1
               , _lstate_addSepSpace = Just $ lstate_baseY state
               , _lstate_inhibitMTEL = False
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

layoutSetCommentCol :: ( MonadMultiState LayoutState m
  , MonadMultiWriter (Seq String) m )
   => m ()
layoutSetCommentCol = do
  state <- mGet
  let col = case _lstate_curYOrAddNewline state of
        Left i -> i + fromMaybe 0 (_lstate_addSepSpace state)
        Right{} -> lstate_baseY state
  traceLocal ("layoutSetCommentCol", col)
  unless (Data.Maybe.isJust $ _lstate_commentCol state)
    $ mSet state { _lstate_commentCol = Just col }

layoutMoveToCommentPos
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => Int
  -> Int
  -> m ()
layoutMoveToCommentPos y x = do
  traceLocal ("layoutMoveToCommentPos", y, x)
  state <- mGet
  if Data.Maybe.isJust (_lstate_commentCol state)
    then do
      mSet state
        { _lstate_curYOrAddNewline = case _lstate_curYOrAddNewline state of
          Left i -> if y==0 then Left i else Right y
          Right{} -> Right y
        , _lstate_addSepSpace = Just $ case _lstate_curYOrAddNewline state of
            Left{} -> if y==0
              then x
              else _lstate_indLevelLinger state + x
            Right{} -> _lstate_indLevelLinger state + x
        }
    else do
      mSet state
        { _lstate_curYOrAddNewline = case _lstate_curYOrAddNewline state of
          Left i -> if y==0 then Left i else Right y
          Right{} -> Right y
        , _lstate_addSepSpace = Just $ if y==0
              then x
              else _lstate_indLevelLinger state + x
        , _lstate_commentCol = Just $ case _lstate_curYOrAddNewline state of
            Left i -> i + fromMaybe 0 (_lstate_addSepSpace state)
            Right{} -> lstate_baseY state
        }

-- | does _not_ add spaces to again reach the current base column.
layoutWriteNewline :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => m ()
layoutWriteNewline = do
  traceLocal ("layoutWriteNewline")
  state <- mGet
  mSet $ state { _lstate_curYOrAddNewline = case _lstate_curYOrAddNewline state of
                  Left{} -> Right 1
                  Right i -> Right (i+1)
               , _lstate_addSepSpace = Nothing
               , _lstate_inhibitMTEL = False
               }

layoutWriteEnsureNewlineBlock :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => m ()
layoutWriteEnsureNewlineBlock = do
  traceLocal ("layoutWriteEnsureNewlineBlock")
  state <- mGet
  mSet $ state
    { _lstate_curYOrAddNewline = case _lstate_curYOrAddNewline state of
        Left{} -> Right 1
        Right i -> Right $ max 1 i
    , _lstate_addSepSpace = Just $ lstate_baseY state
    , _lstate_inhibitMTEL = False
    , _lstate_commentCol = Nothing
    }

layoutWriteEnsureBlock :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => m ()
layoutWriteEnsureBlock = do
  traceLocal ("layoutWriteEnsureBlock")
  state <- mGet
  let
    diff = case (_lstate_addSepSpace state, _lstate_curYOrAddNewline state) of
      (Nothing, Left i) -> lstate_baseY state - i
      (Nothing, Right{}) -> lstate_baseY state
      (Just sp, Left i) -> max sp (lstate_baseY state - i)
      (Just sp, Right{}) -> max sp (lstate_baseY state)
  -- when (diff>0) $ layoutWriteNewlineBlock
  when (diff > 0) $ do
    mSet $ state { _lstate_addSepSpace = Just $ diff }

layoutWriteEnsureAbsoluteN :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => Int -> m ()
layoutWriteEnsureAbsoluteN n = do
  state <- mGet
  let diff = case _lstate_curYOrAddNewline state of
        Left i -> n-i
        Right{} -> n
  traceLocal ("layoutWriteEnsureAbsoluteN", n, diff)
  when (diff>0) $ do
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
                    , _lstate_indLevels = i : _lstate_indLevels s
                    }

layoutIndentLevelPopInternal
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m) => m ()
layoutIndentLevelPopInternal = do
  traceLocal ("layoutIndentLevelPopInternal")
  mModify $ \s -> s { _lstate_indLevelLinger = lstate_indLevel s
                    , _lstate_indLevels = List.tail $ _lstate_indLevels s
                    }

layoutRemoveIndentLevelLinger :: ( MonadMultiState LayoutState m
             , MonadMultiWriter (Seq String) m
             ) => m ()
layoutRemoveIndentLevelLinger = do
#if INSERTTRACES
  tellDebugMessShow ("layoutRemoveIndentLevelLinger")
#endif
  mModify $ \s -> s { _lstate_indLevelLinger = lstate_indLevel s
                    }

layoutWithAddBaseCol :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               ,MonadMultiReader Config m
                                               , MonadMultiWriter (Seq String) m)
                  => m ()
                  -> m ()
layoutWithAddBaseCol m = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWithAddBaseCol")
#endif
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
#if INSERTTRACES
  tellDebugMessShow ("layoutWithAddBaseColBlock")
#endif
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

layoutWithAddBaseColN :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => Int
                  -> m ()
                  -> m ()
layoutWithAddBaseColN amount m = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWithAddBaseColN", amount)
#endif
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
    Nothing -> case (_lstate_curYOrAddNewline state, _lstate_addSepSpace state) of
      (Left i, Just j) -> layoutBaseYPushInternal (i+j)
      (Left i, Nothing) -> layoutBaseYPushInternal i
      (Right{}, _) -> layoutBaseYPushInternal $ lstate_baseY state
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
        (Left i, Just j)   -> i + j
        (Left i, Nothing)  -> i
        (Right{}, Just j)  -> j
        (Right{}, Nothing) -> 0
  layoutIndentLevelPushInternal y
  layoutBaseYPushInternal y

layoutIndentLevelPop
  :: (MonadMultiState LayoutState m, MonadMultiWriter (Seq String) m) => m ()
layoutIndentLevelPop = do
  traceLocal ("layoutIndentLevelPop")
  layoutBaseYPopInternal
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
#if INSERTTRACES
  tellDebugMessShow ("layoutAddSepSpace")
#endif
  state <- mGet
  mSet $ state { _lstate_addSepSpace = Just $ fromMaybe 1 $ _lstate_addSepSpace state }

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
    Nothing -> return ()
    Just ann -> do
      -- curY <- mGet <&> _lstate_curY
      let ExactPrint.Types.DP (y, _x) = ExactPrint.Types.annEntryDelta ann
      -- mModify $ \state -> state { _lstate_addNewline = Just x }
      mModify $ \state ->
        let upd = case _lstate_curYOrAddNewline state of
              Left i -> if y==0 then Left i else Right y
              Right i -> Right $ max y i
        in state
          { _lstate_curYOrAddNewline = upd
          , _lstate_addSepSpace = if Data.Either.isRight upd
              then _lstate_commentCol state <|> _lstate_addSepSpace state <|> Just (lstate_baseY state)
              else Nothing
          , _lstate_commentCol = Nothing
          }
-- fixMoveToLineByIsNewline :: MonadMultiState
--                                                   LayoutState m => Int -> m Int
-- fixMoveToLineByIsNewline x = do
--   newLineState <- mGet <&> _lstate_isNewline
--   return $ if newLineState == NewLineStateYes
--     then x-1
--     else x

ppmMoveToExactLoc :: MonadMultiWriter Text.Builder.Builder m
                  => ExactPrint.Types.DeltaPos
                  -> m ()
ppmMoveToExactLoc (ExactPrint.Types.DP (x,y)) = do
  replicateM_ x $ mTell $ Text.Builder.fromString "\n"
  replicateM_ y $ mTell $ Text.Builder.fromString " "

layoutWritePriorComments :: (Data.Data.Data ast,
                                               MonadMultiWriter Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                              , MonadMultiWriter (Seq String) m)
                         => GenLocated SrcSpan ast -> m ()
layoutWritePriorComments ast = do
  mAnn <- do
    state <- mGet
    let key  = ExactPrint.Types.mkAnnKey ast
    let anns = _lstate_comments state
    let mAnn = ExactPrint.annPriorComments <$> Map.lookup key anns
    mSet $ state
      { _lstate_comments =
          Map.adjust (\ann -> ann { ExactPrint.annPriorComments = [] }) key anns
      }
    return mAnn
#if INSERTTRACES
  tellDebugMessShow ("layoutWritePriorComments", ExactPrint.Types.mkAnnKey ast, mAnn)
#endif
  case mAnn of
    Nothing -> return ()
    Just priors -> do
      when (not $ null priors) $ layoutSetCommentCol
      priors `forM_` \( ExactPrint.Types.Comment comment _ _
                      , ExactPrint.Types.DP (x, y)
                      ) -> do
        replicateM_ x layoutWriteNewline
        layoutWriteAppendSpaces y
        layoutWriteAppendMultiline $ Text.pack $ comment

-- this currently only extracs from the `annsDP` field of Annotations.
-- per documentation, this seems sufficient, as the
-- "..`annFollowingComments` are only added by AST transformations ..".
layoutWritePostComments :: (Data.Data.Data ast,
                                               MonadMultiWriter Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                         => GenLocated SrcSpan ast -> m ()
layoutWritePostComments ast = do
  mAnn <- do
    state <- mGet
    let key  = ExactPrint.Types.mkAnnKey ast
    let anns = _lstate_comments state
    let mAnn = ExactPrint.annFollowingComments <$> Map.lookup key anns
    mSet $ state
      { _lstate_comments =
          Map.adjust (\ann -> ann { ExactPrint.annFollowingComments = [] })
                     key
                     anns
      }
    return mAnn
#if INSERTTRACES
  tellDebugMessShow ("layoutWritePostComments", ExactPrint.Types.mkAnnKey ast, mAnn)
#endif
  case mAnn of
    Nothing -> return ()
    Just posts -> do
      when (not $ null posts) $ layoutSetCommentCol
      posts `forM_` \( ExactPrint.Types.Comment comment _ _
                      , ExactPrint.Types.DP (x, y)
                      ) -> do
        replicateM_ x layoutWriteNewline
        layoutWriteAppend $ Text.pack $ replicate y ' '
        layoutWriteAppendMultiline $ Text.pack $ comment

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
#if INSERTTRACES
  tellDebugMessShow ("layoutIndentRestorePostComment", mCommentCol)
#endif
  mModify $ \s -> s { _lstate_commentCol = Nothing }
  case (mCommentCol, eCurYAddNL) of
    (Just commentCol, Left{}) -> do
      layoutWriteEnsureNewlineBlock
      layoutWriteEnsureAbsoluteN $ commentCol + fromMaybe 0 (_lstate_addSepSpace state)
    _                              -> return ()

-- layoutWritePriorCommentsRestore :: (Data.Data.Data ast,
--                                                MonadMultiWriter Text.Builder.Builder m,
--                                                MonadMultiState LayoutState m
--                                   , MonadMultiWriter (Seq String) m)
--                                 => GenLocated SrcSpan ast -> m ()
-- layoutWritePriorCommentsRestore x = do
--   layoutWritePriorComments x
--   layoutIndentRestorePostComment
-- 
-- layoutWritePostCommentsRestore :: (Data.Data.Data ast,
--                                                MonadMultiWriter Text.Builder.Builder m,
--                                                MonadMultiState LayoutState m
--                                                , MonadMultiWriter (Seq String) m)
--                                 => GenLocated SrcSpan ast -> m ()
-- layoutWritePostCommentsRestore x = do
--   layoutWritePostComments x
--   layoutIndentRestorePostComment

extractAllComments
  :: ExactPrint.Annotation -> [(ExactPrint.Comment, ExactPrint.DeltaPos)]
extractAllComments ann =
  ExactPrint.annPriorComments ann
    ++ ExactPrint.annFollowingComments ann
    ++ (ExactPrint.annsDP ann >>= \case
         (ExactPrint.AnnComment com, dp) -> [(com, dp)]
         _ -> []
       )


foldedAnnKeys :: Data.Data.Data ast
              => ast
              -> Set ExactPrint.AnnKey
foldedAnnKeys ast = everything
  Set.union
  (\x -> maybe
         Set.empty
         Set.singleton
         [ gmapQi 1 (\t -> ExactPrint.mkAnnKey $ L l t) x
         | locTyCon == typeRepTyCon (typeOf x)
         , l <- gmapQi 0 cast x
         ]
  )
  ast
 where
  locTyCon = typeRepTyCon (typeOf (L () ()))

filterAnns :: Data.Data.Data ast
           => ast
           -> ExactPrint.Anns
           -> ExactPrint.Anns
filterAnns ast anns =
  Map.filterWithKey (\k _ -> k `Set.member` foldedAnnKeys ast) anns

hasAnyCommentsBelow :: Data ast => GHC.Located ast -> ToBriDocM Bool
hasAnyCommentsBelow ast@(L l _) = do
  anns <- filterAnns ast <$> mAsk
  return $ List.any (\(c, _) -> ExactPrint.commentIdentifier c > l) 
         $ (=<<) extractAllComments
         $ Map.elems
         $ anns

-- new BriDoc stuff

allocateNode :: MonadMultiState NodeAllocIndex m
              => BriDocFInt
             -> m BriDocNumbered
allocateNode bd = do
  i <- allocNodeIndex
  return (i, bd)

allocNodeIndex :: MonadMultiState NodeAllocIndex m => m Int
allocNodeIndex = do
  NodeAllocIndex i <- mGet
  mSet $ NodeAllocIndex (i+1)
  return i

-- docEmpty :: MonadMultiState NodeAllocIndex m => m BriDocNumbered
-- docEmpty = allocateNode BDFEmpty
-- 
-- docLit :: MonadMultiState NodeAllocIndex m => Text -> m BriDocNumbered
-- docLit t = allocateNode $ BDFLit t
-- 
-- docExt :: (ExactPrint.Annotate.Annotate ast, MonadMultiState NodeAllocIndex m)
--        => GenLocated SrcSpan ast -> ExactPrint.Types.Anns -> Bool -> m BriDocNumbered
-- docExt x anns shouldAddComment = allocateNode $ BDFExternal
--                   (ExactPrint.Types.mkAnnKey x)
--                   (foldedAnnKeys x)
--                   shouldAddComment
--                   (Text.pack $ ExactPrint.exactPrint x anns)
-- 
-- docAlt :: MonadMultiState NodeAllocIndex m => [m BriDocNumbered] -> m BriDocNumbered
-- docAlt l = allocateNode . BDFAlt =<< sequence l
-- 
-- 
-- docSeq :: MonadMultiState NodeAllocIndex m => [m BriDocNumbered] -> m BriDocNumbered
-- docSeq l = allocateNode . BDFSeq =<< sequence l
-- 
-- docLines :: MonadMultiState NodeAllocIndex m => [m BriDocNumbered] -> m BriDocNumbered
-- docLines l = allocateNode . BDFLines =<< sequence l
-- 
-- docCols :: MonadMultiState NodeAllocIndex m => ColSig -> [m BriDocNumbered] -> m BriDocNumbered
-- docCols sig l = allocateNode . BDFCols sig =<< sequence l
-- 
-- docAddBaseY :: MonadMultiState NodeAllocIndex m => BrIndent -> m BriDocNumbered -> m BriDocNumbered
-- docAddBaseY ind bdm = allocateNode . BDFAddBaseY ind =<< bdm
-- 
-- docSetBaseY :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- docSetBaseY bdm = allocateNode . BDFSetBaseY =<< bdm
-- 
-- docSetIndentLevel :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- docSetIndentLevel bdm = allocateNode . BDFSetIndentLevel =<< bdm
-- 
-- docSeparator :: MonadMultiState NodeAllocIndex m => m BriDocNumbered
-- docSeparator = allocateNode BDFSeparator
-- 
-- docAnnotationPrior :: MonadMultiState NodeAllocIndex m => AnnKey -> m BriDocNumbered -> m BriDocNumbered
-- docAnnotationPrior annKey bdm = allocateNode . BDFAnnotationPrior annKey =<< bdm
-- 
-- docAnnotationPost :: MonadMultiState NodeAllocIndex m => AnnKey -> m BriDocNumbered -> m BriDocNumbered
-- docAnnotationPost  annKey bdm = allocateNode . BDFAnnotationPost annKey =<< bdm
-- 
-- docNonBottomSpacing :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- docNonBottomSpacing bdm = allocateNode . BDFNonBottomSpacing =<< bdm
-- 
-- appSep :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- appSep x = docSeq [x, docSeparator]
-- 
-- docCommaSep :: MonadMultiState NodeAllocIndex m => m BriDocNumbered
-- docCommaSep = appSep $ docLit $ Text.pack ","
-- 
-- docParenLSep :: MonadMultiState NodeAllocIndex m => m BriDocNumbered
-- docParenLSep = appSep $ docLit $ Text.pack "("
-- 
-- 
-- docPostComment :: (Data.Data.Data ast, MonadMultiState NodeAllocIndex m)
--                => GenLocated SrcSpan ast
--                -> m BriDocNumbered
--                -> m BriDocNumbered
-- docPostComment ast bdm = do
--   bd <- bdm
--   allocateNode $ BDFAnnotationPost (ExactPrint.Types.mkAnnKey ast) bd
-- 
-- docWrapNode :: ( Data.Data.Data ast, MonadMultiState NodeAllocIndex m)
--             => GenLocated SrcSpan ast
--             -> m BriDocNumbered
--             -> m BriDocNumbered
-- docWrapNode ast bdm = do
--   bd <- bdm
--   i1 <- allocNodeIndex
--   i2 <- allocNodeIndex
--   return
--     $ (,) i1
--     $ BDFAnnotationPrior (ExactPrint.Types.mkAnnKey ast)
--     $ (,) i2
--     $ BDFAnnotationPost (ExactPrint.Types.mkAnnKey ast)
--     $ bd
-- 
-- docPar :: MonadMultiState NodeAllocIndex m
--        => m BriDocNumbered
--        -> m BriDocNumbered
--        -> m BriDocNumbered
-- docPar lineM indentedM = do
--   line <- lineM
--   indented <- indentedM
--   allocateNode $ BDFPar BrIndentNone line indented
-- 
-- docForceSingleline :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- docForceSingleline bdm = allocateNode . BDFForceSingleline =<< bdm
-- 
-- docForceMultiline :: MonadMultiState NodeAllocIndex m => m BriDocNumbered -> m BriDocNumbered
-- docForceMultiline bdm = allocateNode . BDFForceMultiline =<< bdm
-- 
-- docEnsureIndent :: MonadMultiState NodeAllocIndex m => BrIndent -> m BriDocNumbered -> m BriDocNumbered
-- docEnsureIndent ind mbd = mbd >>= \bd -> allocateNode $ BDFEnsureIndent ind bd

docEmpty :: ToBriDocM BriDocNumbered
docEmpty = allocateNode BDFEmpty

docLit :: Text -> ToBriDocM BriDocNumbered
docLit t = allocateNode $ BDFLit t

docExt :: (ExactPrint.Annotate.Annotate ast)
       => GenLocated SrcSpan ast -> ExactPrint.Types.Anns -> Bool -> ToBriDocM BriDocNumbered
docExt x anns shouldAddComment = allocateNode $ BDFExternal
                  (ExactPrint.Types.mkAnnKey x)
                  (foldedAnnKeys x)
                  shouldAddComment
                  (Text.pack $ ExactPrint.exactPrint x anns)

docAlt :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docAlt l = allocateNode . BDFAlt =<< sequence l

docAltFilter :: [(Bool, ToBriDocM BriDocNumbered)] -> ToBriDocM BriDocNumbered
docAltFilter = docAlt . map snd . filter fst


docSeq :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docSeq l = allocateNode . BDFSeq =<< sequence l

docLines :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docLines l = allocateNode . BDFLines =<< sequence l

docCols :: ColSig -> [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docCols sig l = allocateNode . BDFCols sig =<< sequence l

docAddBaseY :: BrIndent -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAddBaseY ind bdm = allocateNode . BDFAddBaseY ind =<< bdm

docSetBaseY :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetBaseY bdm = do
  bd <- bdm
  -- the order here is important so that these two nodes can be treated
  -- properly over at `transformAlts`.
  n1 <- allocateNode $ BDFBaseYPushCur bd
  n2 <- allocateNode $ BDFBaseYPop n1
  return n2

docSetIndentLevel :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetIndentLevel bdm = do
  bd <- bdm
  n1 <- allocateNode $ BDFIndentLevelPushCur bd
  n2 <- allocateNode $ BDFIndentLevelPop n1
  return n2

docSeparator :: ToBriDocM BriDocNumbered
docSeparator = allocateNode BDFSeparator

docAnnotationPrior
  :: AnnKey -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAnnotationPrior annKey bdm = allocateNode . BDFAnnotationPrior annKey =<< bdm

docAnnotationKW
  :: AnnKey -> Maybe AnnKeywordId -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAnnotationKW annKey kw bdm = allocateNode . BDFAnnotationKW annKey kw =<< bdm

docAnnotationRest
  :: AnnKey -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAnnotationRest annKey bdm = allocateNode . BDFAnnotationRest annKey =<< bdm

docNonBottomSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docNonBottomSpacing bdm = allocateNode . BDFNonBottomSpacing =<< bdm

docSetParSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetParSpacing bdm = allocateNode . BDFSetParSpacing =<< bdm

docForceParSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceParSpacing bdm = allocateNode . BDFForceParSpacing =<< bdm

docDebug :: String -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docDebug s bdm = allocateNode . BDFDebug s =<< bdm

appSep :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
appSep x = docSeq [x, docSeparator]

docCommaSep :: ToBriDocM BriDocNumbered
docCommaSep = appSep $ docLit $ Text.pack ","

docParenLSep :: ToBriDocM BriDocNumbered
docParenLSep = appSep $ docLit $ Text.pack "("

docNodeAnnKW
  :: Data.Data.Data ast
  => GenLocated SrcSpan ast
  -> Maybe AnnKeywordId
  -> ToBriDocM BriDocNumbered
  -> ToBriDocM BriDocNumbered
docNodeAnnKW ast kw bdm =
  docAnnotationKW (ExactPrint.Types.mkAnnKey ast) kw bdm

class DocWrapable a where
  docWrapNode :: ( Data.Data.Data ast)
              => GenLocated SrcSpan ast
              -> ToBriDocM a
              -> ToBriDocM a
  docWrapNodePrior :: ( Data.Data.Data ast)
                   => GenLocated SrcSpan ast
                   -> ToBriDocM a
                   -> ToBriDocM a
  docWrapNodeRest  :: ( Data.Data.Data ast)
                   => GenLocated SrcSpan ast
                   -> ToBriDocM a
                   -> ToBriDocM a

instance DocWrapable BriDocNumbered where
  docWrapNode ast bdm = do
    bd <- bdm
    i1 <- allocNodeIndex
    i2 <- allocNodeIndex
    return
      $ (,) i1
      $ BDFAnnotationPrior (ExactPrint.Types.mkAnnKey ast)
      $ (,) i2
      $ BDFAnnotationRest (ExactPrint.Types.mkAnnKey ast)
      $ bd
  docWrapNodePrior ast bdm = do
    bd <- bdm
    i1 <- allocNodeIndex
    return
      $ (,) i1
      $ BDFAnnotationPrior (ExactPrint.Types.mkAnnKey ast)
      $ bd
  docWrapNodeRest ast bdm = do
    bd <- bdm
    i2 <- allocNodeIndex
    return
      $ (,) i2
      $ BDFAnnotationRest (ExactPrint.Types.mkAnnKey ast)
      $ bd

instance DocWrapable a => DocWrapable [a] where
  docWrapNode ast bdsm = do
    bds <- bdsm
    case bds of
      [] -> return $ [] -- TODO: this might be bad. maybe. then again, not really. well.
      [bd] -> do
        bd' <- docWrapNode ast (return bd)
        return [bd']
      (bd1:bdR) | (bdN:bdM) <- reverse bdR -> do
        bd1' <- docWrapNodePrior ast (return bd1)
        bdN' <- docWrapNodeRest  ast (return bdN)
        return $ [bd1'] ++ reverse bdM ++ [bdN']
      _ -> error "cannot happen (TM)"
  docWrapNodePrior ast bdsm = do
    bds <- bdsm
    case bds of
      [] -> return $ []
      (bd1:bdR) -> do
        bd1' <- docWrapNodePrior ast (return bd1)
        return $ (bd1':bdR)
  docWrapNodeRest ast bdsm = do
    bds <- bdsm
    case reverse bds of
      [] -> return $ []
      (bdN:bdR) -> do
        bdN' <- docWrapNodeRest ast (return bdN)
        return $ reverse $ (bdN':bdR)

instance DocWrapable ([BriDocNumbered], BriDocNumbered, a) where
  docWrapNode ast stuffM = do
    (bds, bd, x) <- stuffM
    if null bds
      then do
        bd' <- docWrapNode ast (return bd)
        return $ (bds, bd', x)
      else do
        bds' <- docWrapNodePrior ast (return bds)
        bd' <- docWrapNodeRest ast (return bd)
        return $ (bds', bd', x)
  docWrapNodePrior ast stuffM = do
    (bds, bd, x) <- stuffM
    bds' <- docWrapNodePrior ast (return bds)
    return $ (bds', bd, x)
  docWrapNodeRest ast stuffM = do
    (bds, bd, x) <- stuffM
    bd' <- docWrapNodeRest ast (return bd)
    return $ (bds, bd', x)



docPar :: ToBriDocM BriDocNumbered
       -> ToBriDocM BriDocNumbered
       -> ToBriDocM BriDocNumbered
docPar lineM indentedM = do
  line <- lineM
  indented <- indentedM
  allocateNode $ BDFPar BrIndentNone line indented

docForceSingleline :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceSingleline bdm = allocateNode . BDFForceSingleline =<< bdm

docForceMultiline :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docForceMultiline bdm = allocateNode . BDFForceMultiline =<< bdm

docEnsureIndent :: BrIndent -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docEnsureIndent ind mbd = mbd >>= \bd -> allocateNode $ BDFEnsureIndent ind bd

unknownNodeError
  :: Data.Data.Data ast => String -> ast -> ToBriDocM BriDocNumbered
unknownNodeError infoStr ast = do
  mTell $ [LayoutErrorUnknownNode infoStr ast]
  docLit $ Text.pack "{- BRITTANY ERROR UNHANDLED SYNTACTICAL CONSTRUCT -}"

spacifyDocs :: [ToBriDocM BriDocNumbered] -> [ToBriDocM BriDocNumbered]
spacifyDocs [] = []
spacifyDocs ds = fmap appSep (List.init ds) ++ [List.last ds]

briDocMToPPM :: ToBriDocM a -> PPM a
briDocMToPPM m = do
  readers <- MultiRWSS.mGetRawR
  let ((x, errs), debugs) = runIdentity
                          $ MultiRWSS.runMultiRWSTNil
                          $ MultiRWSS.withMultiStateA (NodeAllocIndex 1)
                          $ MultiRWSS.withMultiReaders readers
                          $ MultiRWSS.withMultiWriterAW
                          $ MultiRWSS.withMultiWriterAW
                          $ m
  mTell debugs
  mTell errs
  return x

docSharedWrapper :: Monad m => (x -> m y) -> x -> m (m y)
docSharedWrapper f x = return <$> f x
