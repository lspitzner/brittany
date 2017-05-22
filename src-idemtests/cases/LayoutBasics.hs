{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE KindSignatures #-}

module Language.Haskell.Brittany.Internal.LayoutBasics
  ( processDefault
  , layoutByExact
  -- , layoutByExactR
  , descToBlockStart
  , descToBlockMinMax
  , descToMinMax
  , rdrNameToText
  , lrdrNameToText
  , lrdrNameToTextAnn
  , askIndent
  , calcLayoutMin
  , calcLayoutMax
  , getCurRemaining
  , layoutWriteAppend
  , layoutWriteAppendMultiline
  , layoutWriteNewline
  , layoutWriteNewlinePlain
  , layoutWriteEnsureNewline
  , layoutWriteEnsureBlock
  , layoutWriteEnsureBlockPlusN
  , layoutWithAddIndent
  , layoutWithAddIndentBlock
  , layoutWithAddIndentN
  , layoutWithAddIndentNBlock
  , layoutWithNonParamIndent
  , layoutWriteEnsureAbsoluteN
  , layoutAddSepSpace
  , moveToExactAnn
  , moveToExactAnn'
  , setOpIndent
  , stringLayouter
  , layoutWritePriorComments
  , layoutWritePostComments
  , layoutIndentRestorePostComment
  , layoutWritePriorCommentsRestore
  , layoutWritePostCommentsRestore
  , extractCommentsPrior
  , extractCommentsPost
  , applyLayouter
  , applyLayouterRestore
  , filterAnns
  , layouterFToLayouterM
  , ppmMoveToExactLoc
  , customLayouterF
  , docEmpty
  , docLit
  , docAlt
  , docSeq
  , docPar
  -- , docCols
  , docPostComment
  , docWrapNode
  , briDocByExact
  , fromMaybeIdentity
  , foldedAnnKeys
  )
where



-- more imports here..

import qualified Language.Haskell.GHC.ExactPrint as ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Annotate as ExactPrint.Annotate
import qualified Language.Haskell.GHC.ExactPrint.Types as ExactPrint.Types
import qualified Language.Haskell.GHC.ExactPrint.Utils as ExactPrint.Utils

import Language.Haskell.GHC.ExactPrint.Types ( AnnKey, Annotation )

import qualified Data.Text.Lazy.Builder as Text.Builder

import Language.Haskell.Brittany.Internal.Config.Types
import Language.Haskell.Brittany.Internal.Types
import Language.Haskell.Brittany.Internal.Utils

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
    --test
  case str of
    "\n" -> return ()
    _    -> mTell $ Text.Builder.fromString $ str


layoutByExact :: ( MonadMultiReader Config m
                 , MonadMultiReader (ExactPrint.Types.Anns) m
                 , ExactPrint.Annotate.Annotate ast
                  )
               => GenLocated SrcSpan ast -> m Layouter
layoutByExact x = do
  anns <- mAsk
  trace (showTreeWithCustom (customLayouterF anns) x) $ layoutByExactR x
  -- trace (ExactPrint.Utils.showAnnData anns 2 x) $ layoutByExactR x

layoutByExactR :: (MonadMultiReader Config m
                  , MonadMultiReader (ExactPrint.Types.Anns) m
                  , ExactPrint.Annotate.Annotate ast)
               => GenLocated SrcSpan ast -> m Layouter
layoutByExactR x = do
  indent <- askIndent
  anns <- mAsk
  let t = Text.pack $ ExactPrint.exactPrint x anns
  let tlines = Text.lines $ t <> Text.pack "\n"
      tlineCount = length tlines
  let len = indent + maximum (Text.length <$> tlines)
  return $ Layouter
    { _layouter_desc = LayoutDesc Nothing $ Just $ BlockDesc AllSameIndent len len Nothing
    , _layouter_func = \_ -> do
        -- layoutWriteEnsureBlock
        layoutWriteAppend $ Text.pack $ "{-" ++ show (ExactPrint.Types.mkAnnKey x, Map.lookup (ExactPrint.Types.mkAnnKey x) anns) ++ "-}"
        zip [1..] tlines `forM_` \(i, l) -> do
          layoutWriteAppend $ l
          unless (i==tlineCount) layoutWriteNewline
        do
          let subKeys = foldedAnnKeys x
          state <- mGet
          let filterF k _ = not $ k `Set.member` subKeys
          mSet $ state
            { _lstate_commentsPrior = Map.filterWithKey filterF
                                    $ _lstate_commentsPrior state
            , _lstate_commentsPost  = Map.filterWithKey filterF
                                    $ _lstate_commentsPost  state
            }
    , _layouter_ast = x
    }

briDocByExact :: (ExactPrint.Annotate.Annotate ast,
                     MonadMultiReader Config m,
                     MonadMultiReader ExactPrint.Types.Anns m
                     ) => GenLocated SrcSpan ast -> m BriDoc
briDocByExact ast = do
  anns <- mAsk
  traceIfDumpConf "ast" _dconf_dump_ast_unknown
    (printTreeWithCustom 100 (customLayouterF anns) ast)
  return $ docExt ast anns

descToBlockStart :: LayoutDesc -> Maybe BlockStart
descToBlockStart (LayoutDesc _ (Just (BlockDesc bs _ _ _))) = Just bs
descToBlockStart (LayoutDesc (Just line) _)                 = Just $ RestOfLine line
descToBlockStart _                                          = Nothing

descToBlockMinMax :: LayoutDesc -> Maybe (Int, Int)
descToBlockMinMax (LayoutDesc _ (Just (BlockDesc _ bmin bmax _))) = Just (bmin, bmax)
descToBlockMinMax _                                               = Nothing

descToMinMax :: Int -> LayoutDesc -> Maybe (Int, Int)
descToMinMax p (LayoutDesc _ (Just (BlockDesc start bmin bmax _))) =
  Just (max rolMin bmin, max rolMin bmax)
    where
      rolMin = case start of
        RestOfLine rol -> p + _lColumns_min rol
        AllSameIndent -> 0

descToMinMax p (LayoutDesc (Just (LayoutColumns _ _ lmin)) _)    =
  Just (len, len)
    where
      len = p + lmin
descToMinMax _ _                                                 =
  Nothing

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
    Nothing -> traceShow "Nothing" t
    Just (ExactPrint.Types.Ann _ _ _ aks _ _) -> if
        | any (hasUni AnnBackquote) aks -> Text.pack "`" <> t <> Text.pack "`"
        | any (hasUni AnnOpenP)     aks -> Text.pack "(" <> t <> Text.pack ")"
        | otherwise -> t


askIndent :: (MonadMultiReader Config m) => m Int
askIndent = runIdentity . _lconfig_indentAmount . _conf_layout <$> mAsk

-- minimum block width, judged from block info or line, whichever is
-- available.
-- example: calcLayoutMin doBlock ~~~ atomically $ do
--                                      foo
--                                    ##              indent
--                                    #############   linepre
--                                    ############### result (in this case)
calcLayoutMin :: Int -- basic indentation amount
              -> Int -- currently used width in current line (after indent)
                     -- used to accurately calc placing of the current-line
                     -- stuff ("do" in the above example) and its width.
              -> LayoutDesc
              -> Int
calcLayoutMin indent linePre (LayoutDesc line block) = case (line, block) of
  (_, Just (BlockDesc AllSameIndent m _ _))    -> indent + m
  (_, Just (BlockDesc (RestOfLine inl) m _ _)) -> max (linePre + _lColumns_min inl) (indent + m)
  (Just s, _)                                  -> indent + _lColumns_min s
  _ -> error "bad LayoutDesc mnasdoiucxvlkjasd"

-- see 
calcLayoutMax :: Int -- basic indentation amount
              -> Int -- currently used width in current line (after indent)
                     -- used to accurately calc placing of the current-line
                     -- stuff ("do" in the above example) and its width.
              -> LayoutDesc
              -> Int
calcLayoutMax indent linePre (LayoutDesc line block) = case (line, block) of
  (Just s, _)                                  -> linePre + _lColumns_min s
  (_, Just (BlockDesc AllSameIndent _ m _))    -> indent + m
  (_, Just (BlockDesc (RestOfLine inl) _ m _)) -> max (linePre + _lColumns_min inl) (indent + m)
  _ -> error "bad LayoutDesc msdnfgouvadnfoiu"

getCurRemaining :: ( MonadMultiReader Config m
                  , MonadMultiState LayoutState m
                  )
                => m Int
getCurRemaining = do
  cols <- mAsk <&> _conf_layout .> _lconfig_cols .> runIdentity
  clc <- _lstate_curLineCols <$> mGet
  return $ cols - clc

layoutWriteAppend :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                  => Text
                  -> m ()
layoutWriteAppend t = do
  state <- mGet
  if _lstate_addSepSpace state
    then do
      mSet $ state { _lstate_curLineCols = _lstate_curLineCols state + Text.length t + 1
                   , _lstate_addSepSpace = False
                   }
      mTell $ Text.Builder.fromText $ Text.pack " " <> t
    else do
      mSet $ state { _lstate_curLineCols = _lstate_curLineCols state + Text.length t }
      mTell $ Text.Builder.fromText t

layoutWriteAppendMultiline :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                  => Text
                  -> m ()
layoutWriteAppendMultiline t = case Text.lines t of
  [] -> return ()
  (l:lr) -> do
    layoutWriteAppend l
    lr `forM_` \x -> do
      layoutWriteNewlinePlain
      layoutWriteAppend x

-- adds a newline and adds spaces to reach the current indentation level.
-- TODO: rename newline -> newlineBlock and newlinePlain -> newline
layoutWriteNewline :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                  => m ()
layoutWriteNewline = do
  state <- mGet
  mSet $ state { _lstate_curLineCols = _lstate_indent state
                          , _lstate_commentCol = Nothing
                          , _lstate_addSepSpace = False
                          }
  mTell $ Text.Builder.fromString $ "\n" ++ replicate (_lstate_indent state) ' '

-- | does _not_ add spaces to again reach the current indentation levels.
layoutWriteNewlinePlain :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                  => m ()
layoutWriteNewlinePlain = do
  state <- mGet
  mSet $ state { _lstate_curLineCols = 0
                          , _lstate_commentCol = Nothing
                          , _lstate_addSepSpace = False
                          }
  mTell $ Text.Builder.fromString $ "\n"

layoutWriteEnsureNewline :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                  => m ()
layoutWriteEnsureNewline = do
  state <- mGet
  when (_lstate_curLineCols state /= _lstate_indent state)
    $ layoutWriteNewline

layoutWriteEnsureBlock :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                  => m ()
layoutWriteEnsureBlock = do
  state <- mGet
  let diff = _lstate_curLineCols state - _lstate_indent state
  if diff>0
    then layoutWriteNewline
    else if diff<0
      then do
        layoutWriteAppend $ Text.pack $ replicate (negate diff) ' '
        mSet $ state { _lstate_curLineCols = _lstate_indent state
                     , _lstate_addSepSpace = False
                     }
      else return ()

layoutWriteEnsureAbsoluteN :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                  => Int -> m ()
layoutWriteEnsureAbsoluteN n = do
  state <- mGet
  let diff = n - _lstate_curLineCols state
  if diff>0
    then do
      layoutWriteAppend $ Text.pack $ replicate diff ' '
      mSet $ state { _lstate_curLineCols = n
                   , _lstate_addSepSpace = False
                   }
    else return ()

layoutWriteEnsureBlockPlusN :: (MonadMultiWriter
                                                           Text.Builder.Builder m,
                                                         MonadMultiState LayoutState m)
                            => Int -> m ()
layoutWriteEnsureBlockPlusN n = do
  state <- mGet
  let diff = _lstate_curLineCols state - _lstate_indent state - n
  if diff>0
    then layoutWriteNewline
    else if diff<0
      then do
        layoutWriteAppend $ Text.pack $ replicate (negate diff) ' '
        mSet $ state { _lstate_addSepSpace = False }
      else return ()

layoutWithAddIndent :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               ,MonadMultiReader Config m)
                  => m ()
                  -> m ()
layoutWithAddIndent m = do
  amount <- mAsk <&> _conf_layout .> _lconfig_indentAmount .> runIdentity
  state <- mGet
  mSet state { _lstate_indent = _lstate_indent state + amount }
  m
  do
    s <- mGet
    mSet $ s { _lstate_indent = _lstate_indent state }

layoutWithAddIndentBlock :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m
                                               ,MonadMultiReader Config m)
                  => m ()
                  -> m ()
layoutWithAddIndentBlock m = do
  amount <- mAsk <&> _conf_layout .> _lconfig_indentAmount .> runIdentity
  state <- mGet
  mSet state { _lstate_indent = _lstate_indent state + amount }
  layoutWriteEnsureBlock
  m
  do
    s <- mGet
    mSet $ s { _lstate_indent = _lstate_indent state }

layoutWithAddIndentNBlock :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                  => Int
                  -> m ()
                  -> m ()
layoutWithAddIndentNBlock amount m = do
  state <- mGet
  mSet state { _lstate_indent = _lstate_indent state + amount }
  layoutWriteEnsureBlock
  m
  do
    s <- mGet
    mSet $ s { _lstate_indent = _lstate_indent state }

layoutWithAddIndentN :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                  => Int
                  -> m ()
                  -> m ()
layoutWithAddIndentN amount m = do
  state <- mGet
  mSet state { _lstate_indent = _lstate_indent state + amount }
  m
  do
    s <- mGet
    mSet $ s { _lstate_indent = _lstate_indent state }

layoutAddSepSpace :: MonadMultiState LayoutState m => m ()
layoutAddSepSpace = do
  state <- mGet
  mSet $ state { _lstate_addSepSpace = True }

moveToExactAnn :: (Data.Data.Data x,
                    MonadMultiWriter Text.Builder.Builder m,
                    MonadMultiState LayoutState m,
                    MonadMultiReader (Map AnnKey Annotation) m) => GenLocated SrcSpan x -> m ()
moveToExactAnn ast = do
  anns <- mAsk
  case Map.lookup (ExactPrint.Types.mkAnnKey ast) anns of
    Nothing -> return ()
    Just ann -> do
      let ExactPrint.Types.DP (x, _y) = ExactPrint.Types.annEntryDelta ann
      replicateM_ x $ layoutWriteNewline

-- TODO: when refactoring is complete, the other version of this method
-- can probably be removed.
moveToExactAnn' :: (MonadMultiWriter Text.Builder.Builder m,
                    MonadMultiState LayoutState m,
                    MonadMultiReader (Map AnnKey Annotation) m) => AnnKey -> m ()
moveToExactAnn' annKey = do
  anns <- mAsk
  case Map.lookup annKey anns of
    Nothing -> return ()
    Just ann -> do
      -- curY <- mGet <&> _lstate_curLineCols
      let ExactPrint.Types.DP (x, _y) = ExactPrint.Types.annEntryDelta ann
      replicateM_ x $ layoutWriteNewline
      -- when (x/=0) $ do
      --   replicateM_ x $ layoutWriteNewlinePlain
      --   mModify $ \s -> s { _lstate_curLineCols = curY }
      --   mTell $ Text.Builder.fromString $ replicate curY ' '

ppmMoveToExactLoc :: MonadMultiWriter Text.Builder.Builder m
                  => ExactPrint.Types.DeltaPos
                  -> m ()
ppmMoveToExactLoc (ExactPrint.Types.DP (x,y)) = do
  replicateM_ x $ mTell $ Text.Builder.fromString "\n"
  replicateM_ y $ mTell $ Text.Builder.fromString " "

layoutWithNonParamIndent :: (MonadMultiWriter
                                                 Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
    => LayoutFuncParams -> m () -> m ()
layoutWithNonParamIndent params m = do
  case _params_opIndent params of
    Nothing -> m
    Just x -> layoutWithAddIndentN x m

setOpIndent :: Int -> LayoutDesc -> LayoutFuncParams -> LayoutFuncParams
setOpIndent i desc p = p
  { _params_opIndent = Just $ case _bdesc_opIndentFloatUp =<< _ldesc_block desc of
      Nothing -> i
      Just j -> max i j
  }

stringLayouter :: Data.Data.Data ast
               => GenLocated SrcSpan ast -> Text -> Layouter
stringLayouter ast t = Layouter
  { _layouter_desc = LayoutDesc
    { _ldesc_line = Just $ LayoutColumns
      { _lColumns_key = ColumnKeyUnique
      , _lColumns_lengths = [Text.length t]
      , _lColumns_min = Text.length t
      }
    , _ldesc_block = Nothing
    }
  , _layouter_func = \_ -> do
      layoutWritePriorCommentsRestore ast
      layoutWriteAppend t
      layoutWritePostComments ast
  , _layouter_ast = ast
  }

layoutWritePriorComments :: (Data.Data.Data ast,
                                               MonadMultiWriter Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                         => GenLocated SrcSpan ast -> m ()
layoutWritePriorComments ast = do
  mAnn <- do
    state <- mGet
    let key = ExactPrint.Types.mkAnnKey ast
    let m   = _lstate_commentsPrior state
    let mAnn = Map.lookup key m
    mSet $ state { _lstate_commentsPrior = Map.delete key m }
    return mAnn
  case mAnn of
    Nothing -> return ()
    Just priors -> do
      when (not $ null priors) $ do
        state <- mGet
        mSet $ state { _lstate_commentCol = Just $ _lstate_curLineCols state }
      priors `forM_` \( ExactPrint.Types.Comment comment _ _
                      , ExactPrint.Types.DP (x, y)
                      ) -> do
        replicateM_ x layoutWriteNewlinePlain
        layoutWriteAppend $ Text.pack $ replicate y ' '
        layoutWriteAppendMultiline $ Text.pack $ comment

-- this currently only extracs from the `annsDP` field of Annotations.
-- per documentation, this seems sufficient, as the
-- "..`annFollowingComments` are only added by AST transformations ..".
layoutWritePostComments :: (Data.Data.Data ast,
                                               MonadMultiWriter Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                         => GenLocated SrcSpan ast -> m ()
layoutWritePostComments ast = do
  mAnn <- do
    state <- mGet
    let key = ExactPrint.Types.mkAnnKey ast
    let m   = _lstate_commentsPost state
    let mAnn = Map.lookup key m
    mSet $ state { _lstate_commentsPost = Map.delete key m }
    return mAnn
  case mAnn of
    Nothing -> return ()
    Just posts -> do
      when (not $ null posts) $ do
        state <- mGet
        mSet $ state { _lstate_commentCol = Just $ _lstate_curLineCols state }
      posts `forM_` \( ExactPrint.Types.Comment comment _ _
                      , ExactPrint.Types.DP (x, y)
                      ) -> do
        replicateM_ x layoutWriteNewlinePlain
        layoutWriteAppend $ Text.pack $ replicate y ' '
        layoutWriteAppendMultiline $ Text.pack $ comment

layoutIndentRestorePostComment :: ( Monad m
           , MonadMultiState LayoutState m
           , MonadMultiWriter Text.Builder.Builder m
           )
                               => m ()
layoutIndentRestorePostComment = do
  mCommentCol <- _lstate_commentCol <$> mGet
  case mCommentCol of
    Nothing -> return ()
    Just commentCol -> do
      layoutWriteNewlinePlain
      layoutWriteAppend $ Text.pack $ replicate commentCol ' '

layoutWritePriorCommentsRestore :: (Data.Data.Data ast,
                                               MonadMultiWriter Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
                                => GenLocated SrcSpan ast -> m ()
layoutWritePriorCommentsRestore x = do
  layoutWritePriorComments x
  layoutIndentRestorePostComment

layoutWritePostCommentsRestore :: (Data.Data.Data ast,
                                               MonadMultiWriter Text.Builder.Builder m,
                                               MonadMultiState LayoutState m)
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
  | let
    r = ExactPrint.Types.annsDP ann
      >>= \case
                 (ExactPrint.Types.AnnComment comment, dp) -> [(comment, dp)]
                 _ -> []
  , not (null r)
  ]


applyLayouter :: Layouter -> LayoutFuncParams -> LayoutM ()
applyLayouter l@(Layouter _ _ ast) params = do
  -- (always) write the prior comments at this point.
  layoutWritePriorCommentsRestore ast
  -- run the real stuff.
  _layouter_func l params
  -- if the _layouter_func has not done so already at some point
  -- (there are nodes for which this makes sense),
  -- write the post comments.
  -- effect is `return ()` if there are no postComments.
  layoutWritePostComments ast

applyLayouterRestore :: Layouter -> LayoutFuncParams -> LayoutM ()
applyLayouterRestore l@(Layouter _ _ ast) params = do
  -- (always) write the prior comments at this point.
  layoutWritePriorCommentsRestore ast
  -- run the real stuff.
  _layouter_func l params
  -- if the _layouter_func has not done so already at some point
  -- (there are nodes for which this makes sense),
  -- write the post comments.
  -- effect is `return ()` if there are no postComments.
  layoutWritePostCommentsRestore ast

foldedAnnKeys :: Data.Data.Data ast
              => ast
              -> Set ExactPrint.Types.AnnKey
foldedAnnKeys ast = everything
  Set.union
  (\x -> maybe
         Set.empty
         Set.singleton
         [ gmapQi 1 (\t -> ExactPrint.Types.mkAnnKey $ L l t) x
         | typeRepTyCon (typeOf (L () ())) == (typeRepTyCon (typeOf x))
         , l <- gmapQi 0 cast x
         ]
  )
  ast

filterAnns :: Data.Data.Data ast
           => ast
           -> ExactPrint.Types.Anns
           -> ExactPrint.Types.Anns
filterAnns ast anns =
  Map.filterWithKey (\k _ -> k `Set.member` foldedAnnKeys ast) anns

layouterFToLayouterM :: MultiReader '[Config, ExactPrint.Types.Anns] a -> LayoutM a
layouterFToLayouterM m = do
  settings <- mAsk
  anns <- mAsk
  return $ runIdentity
         $ runMultiReaderTNil
         $ Control.Monad.Trans.MultiReader.Lazy.withMultiReader anns
         $ Control.Monad.Trans.MultiReader.Lazy.withMultiReader settings
         $ m

-- new BriDoc stuff

docEmpty :: BriDoc
docEmpty = BDEmpty

docLit :: Text -> BriDoc
docLit t = BDLit t

docExt :: ExactPrint.Annotate.Annotate ast
       => GenLocated SrcSpan ast -> ExactPrint.Types.Anns -> BriDoc
docExt x anns = BDExternal
                  (ExactPrint.Types.mkAnnKey x)
                  (foldedAnnKeys x)
                  (Text.pack $ ExactPrint.exactPrint x anns)

docAlt :: [BriDoc] -> BriDoc
docAlt = BDAlt


docSeq :: [BriDoc] -> BriDoc
docSeq = BDSeq


docPostComment :: Data.Data.Data ast
               => GenLocated SrcSpan ast
               -> BriDoc
               -> BriDoc
docPostComment ast bd = BDAnnotationPost (ExactPrint.Types.mkAnnKey ast) bd

docWrapNode :: Data.Data.Data ast
            => GenLocated SrcSpan ast
            -> BriDoc
            -> BriDoc
docWrapNode ast bd = BDAnnotationPrior (ExactPrint.Types.mkAnnKey ast)
                   $ BDAnnotationPost (ExactPrint.Types.mkAnnKey ast)
                   $ bd

docPar :: BriDoc
       -> BriDoc
       -> BriDoc
docPar line indented = BDPar BrIndentNone line indented

-- docPar :: BriDoc
--        -> BrIndent
--        -> [BriDoc]
--        -> BriDoc
-- docPar = BDPar

-- docCols :: ColSig
--         -> [BriDoc]
--         -> BriDoc
-- docCols = BDCols


fromMaybeIdentity :: Identity a -> Maybe a -> Identity a
fromMaybeIdentity x y = Data.Coerce.coerce
                      $ fromMaybe (Data.Coerce.coerce x) y
