#define INSERTTRACES 0

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if !INSERTTRACES
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

module Language.Haskell.Brittany.LayoutBasics
  ( processDefault
  , rdrNameToText
  , lrdrNameToText
  , lrdrNameToTextAnn
  , askIndent
  , getCurRemaining
  , layoutWriteAppend
  , layoutWriteAppendMultiline
  , layoutWriteNewlineBlock
  , layoutWriteNewline
  , layoutWriteEnsureNewlineBlock
  , layoutWriteEnsureBlock
  , layoutWriteEnsureBlockPlusN
  , layoutWithAddBaseCol
  , layoutWithAddBaseColBlock
  , layoutWithAddBaseColN
  , layoutWithAddBaseColNBlock
  , layoutSetBaseColCur
  , layoutSetIndentLevel
  , layoutWriteEnsureAbsoluteN
  , layoutAddSepSpace
  , layoutMoveToIndentCol
  , layoutSetCommentCol
  , moveToExactAnn
  , layoutWritePriorComments
  , layoutWritePostComments
  , layoutIndentRestorePostComment
  , layoutWritePriorCommentsRestore
  , layoutWritePostCommentsRestore
  , layoutRemoveIndentLevelLinger
  , extractCommentsPrior
  , extractCommentsPost
  , fixMoveToLineByIsNewline
  , filterAnns
  , ppmMoveToExactLoc
  , docEmpty
  , docLit
  , docAlt
  , docLines
  , docCols
  , docSeq
  , docPar
  , docPostComment
  , docWrapNode
  , docWrapNodePrior
  , docWrapNodePost
  , docForceSingleline
  , docForceMultiline
  , docEnsureIndent
  , docAddBaseY
  , docSetBaseY
  , docSetIndentLevel
  , docSeparator
  , docAnnotationPrior
  , docAnnotationPost
  , docNonBottomSpacing
  , briDocByExact
  , briDocByExactNoComment
  , fromMaybeIdentity
  , foldedAnnKeys
  , unknownNodeError
  , appSep
  , docCommaSep
  , docParenLSep
  , spacifyDocs
  , briDocMToPPM
  , allocateNode
  , docSharedWrapper
  )
where



#include "prelude.inc"

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint.Utils

import Language.Haskell.GHC.ExactPrint.Types ( AnnKey, Annotation )

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
      Exact{} -> t
      _ | any (hasUni AnnBackquote) aks -> Text.pack "`" <> t <> Text.pack "`"
      _ | any (hasUni AnnOpenP)     aks -> Text.pack "(" <> t <> Text.pack ")"
      _ | otherwise -> t


askIndent :: (MonadMultiReader Config m) => m Int
askIndent = runIdentity . _lconfig_indentAmount . _conf_layout <$> mAsk

getCurRemaining :: ( MonadMultiReader Config m
                  , MonadMultiState LayoutState m
                  )
                => m Int
getCurRemaining = do
  cols <- mAsk <&> _conf_layout .> _lconfig_cols .> runIdentity
  clc <- _lstate_curY <$> mGet
  return $ cols - clc

layoutWriteAppend :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => Text
                  -> m ()
layoutWriteAppend t = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWriteAppend", t)
#endif
  state <- mGet
  case _lstate_addSepSpace state of
    Just i -> do
#if INSERTTRACES
      tellDebugMessShow ("  inserted spaces: ", i)
#endif
      mSet $ state { _lstate_curY = _lstate_curY state + Text.length t + i
                   , _lstate_addSepSpace = Nothing
                   , _lstate_isNewline = NewLineStateNo
                   }
      mTell $ Text.Builder.fromText $ Text.pack (replicate i ' ') <> t
    Nothing -> do
#if INSERTTRACES
      tellDebugMessShow ("  inserted no spaces")
#endif
      mSet $ state { _lstate_curY = _lstate_curY state + Text.length t
                   , _lstate_isNewline = NewLineStateNo
                   }
      mTell $ Text.Builder.fromText t

layoutWriteAppendSpaces :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => Int
                  -> m ()
layoutWriteAppendSpaces i = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWriteAppendSpaces", i)
#endif
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
#if INSERTTRACES
  tellDebugMessShow ("layoutWriteAppendMultiline", t)
#endif
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
#if INSERTTRACES
  tellDebugMessShow ("layoutWriteNewlineBlock")
#endif
  state <- mGet
  mSet $ state { _lstate_curY = 0 -- _lstate_baseY state
               , _lstate_addSepSpace = Just $ _lstate_baseY state
               , _lstate_inhibitMTEL = False
               , _lstate_isNewline = NewLineStateYes
               }
  mTell $ Text.Builder.fromString $ "\n" -- ++ replicate (_lstate_baseY state) ' '

layoutMoveToIndentCol :: ( MonadMultiState LayoutState m
                    , MonadMultiWriter (Seq String) m) => Int -> m ()
layoutMoveToIndentCol i = do
#if INSERTTRACES
  tellDebugMessShow ("layoutMoveToIndentCol", i)
#endif
  state <- mGet
  mSet $ state
    { _lstate_addSepSpace = Just
                          $ if _lstate_isNewline state == NewLineStateNo
        then i 
        else _lstate_indLevelLinger state + i - _lstate_curY state
    }

-- | does _not_ add spaces to again reach the current base column.
layoutWriteNewline :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => m ()
layoutWriteNewline = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWriteNewline")
#endif
  state <- mGet
  mSet $ state { _lstate_curY = 0
               , _lstate_addSepSpace = Nothing
               , _lstate_inhibitMTEL = False
               , _lstate_isNewline = NewLineStateYes
               }
  mTell $ Text.Builder.fromString $ "\n"

layoutWriteEnsureNewlineBlock :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => m ()
layoutWriteEnsureNewlineBlock = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWriteEnsureNewlineBlock")
#endif
  state <- mGet
  mSet $ state { _lstate_addSepSpace = Just $ _lstate_baseY state }
  when (_lstate_isNewline state == NewLineStateNo) $ do
    layoutWriteNewlineBlock


layoutWriteEnsureBlock :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => m ()
layoutWriteEnsureBlock = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWriteEnsureBlock")
#endif
  state <- mGet
  let diff = case _lstate_addSepSpace state of
        Nothing -> _lstate_curY state - _lstate_baseY state
        Just sp -> _lstate_baseY state - sp - _lstate_curY state
  -- when (diff>0) $ layoutWriteNewlineBlock
  when (diff>0) $ do
    mSet $ state { _lstate_addSepSpace = Just
                                       $ _lstate_baseY state
                                       - _lstate_curY state
                 }

layoutWriteEnsureAbsoluteN :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => Int -> m ()
layoutWriteEnsureAbsoluteN n = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWriteEnsureAbsoluteN", n)
#endif
  state <- mGet
  let diff = n - _lstate_curY state
  when (diff>0) $ do
    mSet $ state { _lstate_addSepSpace = Just diff -- this always sets to
                                            -- at least (Just 1), so we won't
                                            -- overwrite any old value in any
                                            -- bad way.
                 }

layoutWriteEnsureBlockPlusN :: (MonadMultiWriter
                                                           Text.Builder.Builder m,
                                                         MonadMultiState LayoutState m
                                                         , MonadMultiWriter (Seq String) m)
                            => Int -> m ()
layoutWriteEnsureBlockPlusN n = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWriteEnsureBlockPlusN", n)
#endif
  state <- mGet
  let diff = _lstate_curY state - _lstate_baseY state - n
  if diff>0
    then layoutWriteNewlineBlock
    else if diff<0
      then do
        layoutWriteAppendSpaces $ negate diff
      else return ()

layoutSetBaseColInternal :: ( MonadMultiState LayoutState m
             , MonadMultiWriter (Seq String) m
             ) => Int -> m ()
layoutSetBaseColInternal i = do
#if INSERTTRACES
  tellDebugMessShow ("layoutSetBaseColInternal", i)
#endif
  mModify $ \s -> s { _lstate_baseY = i }

layoutSetIndentLevelInternal :: ( MonadMultiState LayoutState m
             , MonadMultiWriter (Seq String) m
             ) => Int -> m ()
layoutSetIndentLevelInternal i = do
#if INSERTTRACES
  tellDebugMessShow ("layoutSetIndentLevelInternal", i)
#endif
  mModify $ \s -> s { _lstate_indLevelLinger = _lstate_indLevel s
                    , _lstate_indLevel = i
                    }

layoutRemoveIndentLevelLinger :: ( MonadMultiState LayoutState m
             , MonadMultiWriter (Seq String) m
             ) => m ()
layoutRemoveIndentLevelLinger = do
#if INSERTTRACES
  tellDebugMessShow ("layoutRemoveIndentLevelLinger")
#endif
  mModify $ \s -> s { _lstate_indLevelLinger = _lstate_indLevel s
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
  amount <- mAsk <&> _conf_layout .> _lconfig_indentAmount .> runIdentity
  state <- mGet
  layoutSetBaseColInternal $ _lstate_baseY state + amount
  m
  layoutSetBaseColInternal $ _lstate_baseY state

layoutWithAddBaseColBlock :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               ,MonadMultiReader Config m
                                               , MonadMultiWriter (Seq String) m)
                  => m ()
                  -> m ()
layoutWithAddBaseColBlock m = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWithAddBaseColBlock")
#endif
  amount <- mAsk <&> _conf_layout .> _lconfig_indentAmount .> runIdentity
  state <- mGet
  layoutSetBaseColInternal $ _lstate_baseY state + amount
  layoutWriteEnsureBlock
  m
  layoutSetBaseColInternal $ _lstate_baseY state

layoutWithAddBaseColNBlock :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                  => Int
                  -> m ()
                  -> m ()
layoutWithAddBaseColNBlock amount m = do
#if INSERTTRACES
  tellDebugMessShow ("layoutWithAddBaseColNBlock", amount)
#endif
  state <- mGet
  layoutSetBaseColInternal $ _lstate_baseY state + amount
  layoutWriteEnsureBlock
  m
  layoutSetBaseColInternal $ _lstate_baseY state

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
  layoutSetBaseColInternal $ _lstate_baseY state + amount
  m
  layoutSetBaseColInternal $ _lstate_baseY state

layoutSetBaseColCur :: (MonadMultiState
                                                   LayoutState m,
                                                 MonadMultiWriter (Seq String) m)
                    => m () -> m ()
layoutSetBaseColCur m = do
#if INSERTTRACES
  tellDebugMessShow ("layoutSetBaseColCur")
#endif
  state <- mGet
  layoutSetBaseColInternal $ case _lstate_addSepSpace state of
    Nothing -> _lstate_curY state
    Just i  -> _lstate_curY state + i
  m
  layoutSetBaseColInternal $ _lstate_baseY state

layoutSetIndentLevel :: (MonadMultiState
                                                   LayoutState m,
                                                 MonadMultiWriter (Seq String) m)
                    => m () -> m ()
layoutSetIndentLevel m = do
#if INSERTTRACES
  tellDebugMessShow ("layoutSetIndentLevel")
#endif
  state <- mGet
  layoutSetIndentLevelInternal $ _lstate_curY state + fromMaybe 0 (_lstate_addSepSpace state)
  m
  layoutSetIndentLevelInternal $ _lstate_indLevel state
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
moveToExactAnn :: (MonadMultiWriter Text.Builder.Builder m,
                    MonadMultiState LayoutState m,
                    MonadMultiReader (Map AnnKey Annotation) m
                  , MonadMultiWriter (Seq String) m) => AnnKey -> m ()
moveToExactAnn annKey = do
#if INSERTTRACES
  tellDebugMessShow ("moveToExactAnn'", annKey)
#endif
  anns <- mAsk
  case Map.lookup annKey anns of
    Nothing -> return ()
    Just ann -> do
      -- curY <- mGet <&> _lstate_curY
      let ExactPrint.Types.DP (x, _y) = ExactPrint.Types.annEntryDelta ann
      fixedX <- fixMoveToLineByIsNewline x
      replicateM_ fixedX $ layoutWriteNewlineBlock

fixMoveToLineByIsNewline :: MonadMultiState
                                                  LayoutState m => Int -> m Int
fixMoveToLineByIsNewline x = do
  newLineState <- mGet <&> _lstate_isNewline
  return $ if newLineState == NewLineStateYes
    then x-1
    else x

ppmMoveToExactLoc :: MonadMultiWriter Text.Builder.Builder m
                  => ExactPrint.Types.DeltaPos
                  -> m ()
ppmMoveToExactLoc (ExactPrint.Types.DP (x,y)) = do
  replicateM_ x $ mTell $ Text.Builder.fromString "\n"
  replicateM_ y $ mTell $ Text.Builder.fromString " "

layoutSetCommentCol :: ( MonadMultiState LayoutState m
  , MonadMultiWriter (Seq String) m )
   => m ()
layoutSetCommentCol = do
  state <- mGet
  let col = _lstate_curY state
          + fromMaybe 0 (_lstate_addSepSpace state)
#if INSERTTRACES
  tellDebugMessShow ("layoutSetCommentCol", col)
#endif
  mSet state { _lstate_commentCol = Just col }

layoutWritePriorComments :: (Data.Data.Data ast,
                                               MonadMultiWriter Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                              , MonadMultiWriter (Seq String) m)
                         => GenLocated SrcSpan ast -> m ()
layoutWritePriorComments ast = do
  mAnn <- do
    state <- mGet
    let key = ExactPrint.Types.mkAnnKey ast
    let m   = _lstate_commentsPrior state
    let mAnn = Map.lookup key m
    mSet $ state { _lstate_commentsPrior = Map.delete key m }
    return mAnn
#if INSERTTRACES
  tellDebugMessShow ("layoutWritePriorComments", ExactPrint.Types.mkAnnKey ast, mAnn)
#endif
  case mAnn of
    Nothing -> return ()
    Just priors -> do
      when (not $ null priors) $ do
        state <- mGet
        mSet $ state { _lstate_commentCol = Just $ _lstate_curY state }
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
    let key = ExactPrint.Types.mkAnnKey ast
    let m   = _lstate_commentsPost state
    let mAnn = Map.lookup key m
    mSet $ state { _lstate_commentsPost = Map.delete key m }
    return mAnn
#if INSERTTRACES
  tellDebugMessShow ("layoutWritePostComments", ExactPrint.Types.mkAnnKey ast, mAnn)
#endif
  case mAnn of
    Nothing -> return ()
    Just posts -> do
      when (not $ null posts) $ do
        state <- mGet
        mSet $ state { _lstate_commentCol = Just $ _lstate_curY state }
      posts `forM_` \( ExactPrint.Types.Comment comment _ _
                      , ExactPrint.Types.DP (x, y)
                      ) -> do
        replicateM_ x layoutWriteNewline
        layoutWriteAppend $ Text.pack $ replicate y ' '
        layoutWriteAppendMultiline $ Text.pack $ comment

layoutIndentRestorePostComment :: ( Monad m
           , MonadMultiState LayoutState m
           , MonadMultiWriter Text.Builder.Builder m
           , MonadMultiWriter (Seq String) m
           )
                               => m ()
layoutIndentRestorePostComment = do
  isNotNewline <- mGet <&> _lstate_isNewline .> (==NewLineStateNo)
  mCommentCol <- _lstate_commentCol <$> mGet
#if INSERTTRACES
  tellDebugMessShow ("layoutIndentRestorePostComment", mCommentCol)
#endif
  mModify $ \s -> s { _lstate_commentCol = Nothing }
  case mCommentCol of
    Just commentCol | isNotNewline -> do
      layoutWriteNewline
      layoutWriteAppendSpaces commentCol
    _                              -> return ()

layoutWritePriorCommentsRestore :: (Data.Data.Data ast,
                                               MonadMultiWriter Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                  , MonadMultiWriter (Seq String) m)
                                => GenLocated SrcSpan ast -> m ()
layoutWritePriorCommentsRestore x = do
  layoutWritePriorComments x
  layoutIndentRestorePostComment

layoutWritePostCommentsRestore :: (Data.Data.Data ast,
                                               MonadMultiWriter Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               , MonadMultiWriter (Seq String) m)
                                => GenLocated SrcSpan ast -> m ()
layoutWritePostCommentsRestore x = do
  layoutWritePostComments x
  layoutIndentRestorePostComment

extractCommentsPrior :: ExactPrint.Types.Anns -> PriorMap
extractCommentsPrior anns = flip Map.mapMaybe anns $ \ann ->
  [r | let r = ExactPrint.Types.annPriorComments ann, not (null r)]
extractCommentsPost  :: ExactPrint.Types.Anns -> PostMap
extractCommentsPost  anns = flip Map.mapMaybe anns $ \ann ->
  [r
  | let r = ExactPrint.Types.annsDP ann >>= \case
                 (ExactPrint.Types.AnnComment comment, dp) -> [(comment, dp)]
                 _ -> []
  , not (null r)
  ]


foldedAnnKeys :: Data.Data.Data ast
              => ast
              -> Set ExactPrint.Types.AnnKey
foldedAnnKeys ast = everything
  Set.union
  (\x -> maybe
         Set.empty
         Set.singleton
         [ gmapQi 1 (\t -> ExactPrint.Types.mkAnnKey $ L l t) x
         | locTyCon == typeRepTyCon (typeOf x)
         , l <- gmapQi 0 cast x
         ]
  )
  ast
 where
  locTyCon = typeRepTyCon (typeOf (L () ()))

filterAnns :: Data.Data.Data ast
           => ast
           -> ExactPrint.Types.Anns
           -> ExactPrint.Types.Anns
filterAnns ast anns =
  Map.filterWithKey (\k _ -> k `Set.member` foldedAnnKeys ast) anns

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


docSeq :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docSeq l = allocateNode . BDFSeq =<< sequence l

docLines :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docLines l = allocateNode . BDFLines =<< sequence l

docCols :: ColSig -> [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docCols sig l = allocateNode . BDFCols sig =<< sequence l

docAddBaseY :: BrIndent -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAddBaseY ind bdm = allocateNode . BDFAddBaseY ind =<< bdm

docSetBaseY :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetBaseY bdm = allocateNode . BDFSetBaseY =<< bdm

docSetIndentLevel :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docSetIndentLevel bdm = allocateNode . BDFSetIndentLevel =<< bdm

docSeparator :: ToBriDocM BriDocNumbered
docSeparator = allocateNode BDFSeparator

docAnnotationPrior :: AnnKey -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAnnotationPrior annKey bdm = allocateNode . BDFAnnotationPrior annKey =<< bdm

docAnnotationPost :: AnnKey -> ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docAnnotationPost  annKey bdm = allocateNode . BDFAnnotationPost annKey =<< bdm

docNonBottomSpacing :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
docNonBottomSpacing bdm = allocateNode . BDFNonBottomSpacing =<< bdm

appSep :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
appSep x = docSeq [x, docSeparator]

docCommaSep :: ToBriDocM BriDocNumbered
docCommaSep = appSep $ docLit $ Text.pack ","

docParenLSep :: ToBriDocM BriDocNumbered
docParenLSep = appSep $ docLit $ Text.pack "("


docPostComment :: (Data.Data.Data ast)
               => GenLocated SrcSpan ast
               -> ToBriDocM BriDocNumbered
               -> ToBriDocM BriDocNumbered
docPostComment ast bdm = do
  bd <- bdm
  allocateNode $ BDFAnnotationPost (ExactPrint.Types.mkAnnKey ast) bd

-- docWrapNode :: ( Data.Data.Data ast)
--             => GenLocated SrcSpan ast
--             -> ToBriDocM BriDocNumbered
--             -> ToBriDocM BriDocNumbered
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

class DocWrapable a where
  docWrapNode :: ( Data.Data.Data ast)
              => GenLocated SrcSpan ast
              -> ToBriDocM a
              -> ToBriDocM a
  docWrapNodePrior :: ( Data.Data.Data ast)
                   => GenLocated SrcSpan ast
                   -> ToBriDocM a
                   -> ToBriDocM a
  docWrapNodePost  :: ( Data.Data.Data ast)
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
      $ BDFAnnotationPost (ExactPrint.Types.mkAnnKey ast)
      $ bd
  docWrapNodePrior ast bdm = do
    bd <- bdm
    i1 <- allocNodeIndex
    return
      $ (,) i1
      $ BDFAnnotationPrior (ExactPrint.Types.mkAnnKey ast)
      $ bd
  docWrapNodePost ast bdm = do
    bd <- bdm
    i2 <- allocNodeIndex
    return
      $ (,) i2
      $ BDFAnnotationPost (ExactPrint.Types.mkAnnKey ast)
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
        bdN' <- docWrapNodePost  ast (return bdN)
        return $ [bd1'] ++ reverse bdM ++ [bdN']
      _ -> error "cannot happen (TM)"
  docWrapNodePrior ast bdsm = do
    bds <- bdsm
    case bds of
      [] -> return $ []
      (bd1:bdR) -> do
        bd1' <- docWrapNodePrior ast (return bd1)
        return $ (bd1':bdR)
  docWrapNodePost ast bdsm = do
    bds <- bdsm
    case reverse bds of
      [] -> return $ []
      (bdN:bdR) -> do
        bdN' <- docWrapNodePost ast (return bdN)
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
        bd' <- docWrapNodePost ast (return bd)
        return $ (bds', bd', x)
  docWrapNodePrior ast stuffM = do
    (bds, bd, x) <- stuffM
    bds' <- docWrapNodePrior ast (return bds)
    return $ (bds', bd, x)
  docWrapNodePost ast stuffM = do
    (bds, bd, x) <- stuffM
    bd' <- docWrapNodePost ast (return bd)
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


fromMaybeIdentity :: Identity a -> Maybe a -> Identity a
fromMaybeIdentity x y = Data.Coerce.coerce
                      $ fromMaybe (Data.Coerce.coerce x) y

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
