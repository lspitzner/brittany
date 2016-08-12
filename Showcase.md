# brittany showcase

We will try to take the following module and try to fit it into 80 columns.

## input

~~~~ .hs
--        10        20        30        40        50        60        70        80
module Language.Haskell.Brittany.BriLayouter
  ( layoutBriDoc
  )
where



layoutBriDoc :: Data.Data.Data ast => ast -> BriDocNumbered -> PPM ()
layoutBriDoc ast briDoc = do
  -- first step: transform the briDoc.
  briDoc' <- MultiRWSS.withMultiStateS BDEmpty $ do
    traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw $ briDocToDoc $ unwrapBriDocNumbered $ briDoc
    -- bridoc transformation: remove alts
    transformAlts briDoc >>= mSet
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
        { _lstate_baseY = 0
        , _lstate_curY = 0
        , _lstate_indLevel = 0
        , _lstate_indLevelLinger = 0
        , _lstate_commentsPrior = extractCommentsPrior filteredAnns
        , _lstate_commentsPost = extractCommentsPost  filteredAnns
        , _lstate_commentCol = Nothing
        , _lstate_addSepSpace = Nothing
        , _lstate_inhibitMTEL = False
        , _lstate_isNewline = NewLineStateInit
        }

  state' <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'
  
  let remainingComments = Map.elems (_lstate_commentsPrior state') ++ Map.elems (_lstate_commentsPost  state')
  remainingComments `forM_` (mTell . (:[]) . LayoutErrorUnusedComment . show . fmap fst)
  
  return $ ()
~~~~

## brittany output

~~~~ .hs
--        10        20        30        40        50        60        70        80
module Language.Haskell.Brittany.BriLayouter
  ( layoutBriDoc
  )
where



layoutBriDoc :: Data.Data.Data ast => ast -> BriDocNumbered -> PPM ()
layoutBriDoc ast briDoc = do
  -- first step: transform the briDoc.
  briDoc'                       <- MultiRWSS.withMultiStateS BDEmpty $ do
    traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw
      $ briDocToDoc
      $ unwrapBriDocNumbered
      $ briDoc
    -- bridoc transformation: remove alts
    transformAlts briDoc >>= mSet
    mGet
      >>= traceIfDumpConf "bridoc post-alt" _dconf_dump_bridoc_simpl_alt
      .   briDocToDoc
    -- bridoc transformation: float stuff in
    mGet <&> transformSimplifyFloating >>= mSet
    mGet
      >>= traceIfDumpConf "bridoc post-floating"
                          _dconf_dump_bridoc_simpl_floating
      .   briDocToDoc
    -- bridoc transformation: par removal
    mGet <&> transformSimplifyPar >>= mSet
    mGet
      >>= traceIfDumpConf "bridoc post-par" _dconf_dump_bridoc_simpl_par
      .   briDocToDoc
    -- bridoc transformation: float stuff in
    mGet <&> transformSimplifyColumns >>= mSet
    mGet
      >>= traceIfDumpConf "bridoc post-columns" _dconf_dump_bridoc_simpl_columns
      .   briDocToDoc
    -- -- bridoc transformation: indent
    mGet <&> transformSimplifyIndent >>= mSet
    mGet
      >>= traceIfDumpConf "bridoc post-indent" _dconf_dump_bridoc_simpl_indent
      .   briDocToDoc
    mGet
      >>= traceIfDumpConf "bridoc final" _dconf_dump_bridoc_final
      .   briDocToDoc
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
        , _lstate_commentsPrior  = extractCommentsPrior filteredAnns
        , _lstate_commentsPost   = extractCommentsPost filteredAnns
        , _lstate_commentCol     = Nothing
        , _lstate_addSepSpace    = Nothing
        , _lstate_inhibitMTEL    = False
        , _lstate_isNewline      = NewLineStateInit
        }

  state' <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'

  let remainingComments = Map.elems (_lstate_commentsPrior state')
        ++ Map.elems (_lstate_commentsPost state')
  remainingComments
    `forM_` (mTell . (:[]) . LayoutErrorUnusedComment . show . fmap fst)

  return $ ()
~~~~

In contrast, let us look at Chris Done's `hindent` re-formatting results for the same input, with two different styles:

## hindent --style chris-done output

~~~~ .hs
--        10        20        30        40        50        60        70        80
module Language.Haskell.Brittany.BriLayouter
  (layoutBriDoc)
  where

layoutBriDoc :: Data.Data.Data ast
             => ast -> BriDocNumbered -> PPM ()
layoutBriDoc ast briDoc = 
  do 
     -- first step: transform the briDoc.
     briDoc' <- 
       MultiRWSS.withMultiStateS BDEmpty $
       do traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw $
            briDocToDoc $ unwrapBriDocNumbered $ briDoc
          -- bridoc transformation: remove alts
          transformAlts briDoc >>=
            mSet
          mGet >>=
            traceIfDumpConf "bridoc post-alt" _dconf_dump_bridoc_simpl_alt .
            briDocToDoc
          -- bridoc transformation: float stuff in
          mGet <&>
            transformSimplifyFloating >>=
            mSet
          mGet >>=
            traceIfDumpConf "bridoc post-floating" _dconf_dump_bridoc_simpl_floating .
            briDocToDoc
          -- bridoc transformation: par removal
          mGet <&>
            transformSimplifyPar >>=
            mSet
          mGet >>=
            traceIfDumpConf "bridoc post-par" _dconf_dump_bridoc_simpl_par .
            briDocToDoc
          -- bridoc transformation: float stuff in
          mGet <&>
            transformSimplifyColumns >>=
            mSet
          mGet >>=
            traceIfDumpConf "bridoc post-columns" _dconf_dump_bridoc_simpl_columns .
            briDocToDoc
          -- -- bridoc transformation: indent
          mGet <&>
            transformSimplifyIndent >>=
            mSet
          mGet >>=
            traceIfDumpConf "bridoc post-indent" _dconf_dump_bridoc_simpl_indent .
            briDocToDoc
          mGet >>=
            traceIfDumpConf "bridoc final" _dconf_dump_bridoc_final .
            briDocToDoc
     -- -- convert to Simple type
     -- simpl <- mGet <&> transformToSimple
     -- return simpl
     anns :: ExactPrint.Types.Anns <- mAsk
     let filteredAnns = filterAnns ast anns
     let state = 
           LayoutState {_lstate_baseY = 0
                       ,_lstate_curY = 0
                       ,_lstate_indLevel = 0
                       ,_lstate_indLevelLinger = 0
                       ,_lstate_commentsPrior = 
                          extractCommentsPrior filteredAnns
                       ,_lstate_commentsPost = extractCommentsPost filteredAnns
                       ,_lstate_commentCol = Nothing
                       ,_lstate_addSepSpace = Nothing
                       ,_lstate_inhibitMTEL = False
                       ,_lstate_isNewline = NewLineStateInit}
     state' <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'
     let remainingComments = 
           Map.elems (_lstate_commentsPrior state') ++
           Map.elems (_lstate_commentsPost state')
     remainingComments `forM_`
       (mTell . (: []) . LayoutErrorUnusedComment . show . fmap fst)
     return $ ()
~~~~

## hindent --style gibiansky output

note that it managed to garbage the first comment to `-- 10 20 30 40 50 60 70 80` which i replaced again afterwards.

~~~~ .hs
--        10        20        30        40        50        60        70        80
module Language.Haskell.Brittany.BriLayouter (layoutBriDoc) where

layoutBriDoc :: Data.Data.Data ast => ast -> BriDocNumbered -> PPM ()
layoutBriDoc ast briDoc = do
  -- first step: transform the briDoc.
  briDoc' <- MultiRWSS.withMultiStateS BDEmpty $ do
               traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw $ briDocToDoc $ unwrapBriDocNumbered $ briDoc
               -- bridoc transformation: remove alts
               transformAlts briDoc >>= mSet
               mGet >>= traceIfDumpConf "bridoc post-alt"
                          _dconf_dump_bridoc_simpl_alt . briDocToDoc
               -- bridoc transformation: float stuff in
               mGet <&> transformSimplifyFloating >>= mSet
               mGet >>= traceIfDumpConf "bridoc post-floating"
                          _dconf_dump_bridoc_simpl_floating . briDocToDoc
               -- bridoc transformation: par removal
               mGet <&> transformSimplifyPar >>= mSet
               mGet >>= traceIfDumpConf "bridoc post-par"
                          _dconf_dump_bridoc_simpl_par . briDocToDoc
               -- bridoc transformation: float stuff in
               mGet <&> transformSimplifyColumns >>= mSet
               mGet >>= traceIfDumpConf "bridoc post-columns"
                          _dconf_dump_bridoc_simpl_columns . briDocToDoc
               -- -- bridoc transformation: indent
               mGet <&> transformSimplifyIndent >>= mSet
               mGet >>= traceIfDumpConf "bridoc post-indent"
                          _dconf_dump_bridoc_simpl_indent . briDocToDoc
               mGet >>= traceIfDumpConf "bridoc final" _dconf_dump_bridoc_final . briDocToDoc
  -- -- convert to Simple type simpl <- mGet <&> transformToSimple return simpl
  anns :: ExactPrint.Types.Anns <- mAsk
  let filteredAnns = filterAnns ast anns

  let state = LayoutState
        { _lstate_baseY = 0
        , _lstate_curY = 0
        , _lstate_indLevel = 0
        , _lstate_indLevelLinger = 0
        , _lstate_commentsPrior = extractCommentsPrior filteredAnns
        , _lstate_commentsPost = extractCommentsPost filteredAnns
        , _lstate_commentCol = Nothing
        , _lstate_addSepSpace = Nothing
        , _lstate_inhibitMTEL = False
        , _lstate_isNewline = NewLineStateInit
        }

  state' <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'

  let remainingComments = Map.elems (_lstate_commentsPrior state') ++ Map.elems
                                                                        (_lstate_commentsPost
                                                                           state')
  remainingComments `forM_` (mTell .
                             (: []) .
                             LayoutErrorUnusedComment .
                             show .
                             fmap fst)

  return $ ()
~~~~
