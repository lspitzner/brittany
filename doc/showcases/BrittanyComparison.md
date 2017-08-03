
versions used:

- brittany-0.8.0.1
- hindent-5.2.3
- haskell-formatter-1.0.0

## simple nested ifs

~~~~.hs
--------------------------------------------------------------- vvv brittany vvv
mybinding = if condition1
  then if condition2
    then if condition3 then 0 else 1
    else if condition3 then 2 else 3
  else 4
---------------------------------------------------------------- vvv hindent vvv
mybinding =
  if condition1
    then if condition2
           then if condition3
                  then 0
                  else 1
           else if condition3
                  then 2
                  else 3
    else 4
------------------------------------------------------ vvv haskell-formatter vvv
mybinding
  = if condition1 then
      if condition2 then if condition3 then 0 else 1 else
        if condition3 then 2 else 3
      else 4
--------------------------------------------------------------------------------
~~~~

## monad comprehension + alignment

~~~~.hs
--        10        20        30        40        50        60        70        80
--------------------------------------------------------------- vvv brittany vvv
myBinding =
  [ [ [ LeguEosb r1 (n2 - r1) fxvoNz    ymuSreje v
      , LeguEosb n2 (f3 - n2) oyphEmedn ymuSreje v
      , LeguEosb f3 (i4 - f3) fxvoNz    ymuSreje v
      , LeguEosb i4 (v5 - i4) oieha     ymuSreje v
      , LeguEosb v5 (j6 - v5) fxvoNz    ymuSreje v
      ]
    | oyphEmedn <- sdliWmguje
    , oieha     <- ohzvIp
    ]
  | v5 < j6
  , sdliWmguje <- zedoaregeuKilb tua1
  , ohzvIp     <- zedoaregeuKilb (0 - loy2)
  ]
---------------------------------------------------------------- vvv hindent vvv
myBinding =
  [ [ [ LeguEosb r1 (n2 - r1) fxvoNz ymuSreje v
      , LeguEosb n2 (f3 - n2) oyphEmedn ymuSreje v
      , LeguEosb f3 (i4 - f3) fxvoNz ymuSreje v
      , LeguEosb i4 (v5 - i4) oieha ymuSreje v
      , LeguEosb v5 (j6 - v5) fxvoNz ymuSreje v
  ]
  | oyphEmedn <- sdliWmguje
  , oieha <- ohzvIp
  ]
  | v5 < j6
  , sdliWmguje <- zedoaregeuKilb tua1
  , ohzvIp <- zedoaregeuKilb (0 - loy2)
  ]
------------------------------------------------------ vvv haskell-formatter vvv
myBinding
  = [[[LeguEosb r1 (n2 - r1) fxvoNz ymuSreje v,
       LeguEosb n2 (f3 - n2) oyphEmedn ymuSreje v,
       LeguEosb f3 (i4 - f3) fxvoNz ymuSreje v,
       LeguEosb i4 (v5 - i4) oieha ymuSreje v,
       LeguEosb v5 (j6 - v5) fxvoNz ymuSreje v]

      | oyphEmedn <- sdliWmguje, oieha <- ohzvIp]

     | v5 < j6, sdliWmguje <- zedoaregeuKilb tua1,
     ohzvIp <- zedoaregeuKilb (0 - loy2)]
--------------------------------------------------------------------------------
~~~~

## tricky full line usage

~~~~.hs
--        10        20        30        40        50        60        70        80
--------------------------------------------------------------- vvv brittany vvv
mybinding = RH.performEvent_ $ postBuild <&> \() -> liftIO $ do
  runMaybeT postCliInit >>= \case
    Nothing -> return ()
    Just () -> do
      _ <- forkIO $ postCliInitAsync `catch` \(e :: SomeException) ->
        writeLogS LogLevelError (show e)
      return ()
---------------------------------------------------------------- vvv hindent vvv
mybinding =
  RH.performEvent_ $
  postBuild <&> \() ->
    liftIO $ do
      runMaybeT postCliInit >>= \case
        Nothing -> return ()
        Just () -> do
          _ <-
            forkIO $
            postCliInitAsync `catch` \(e :: SomeException) ->
              writeLogS LogLevelError (show e)
          return ()
------------------------------------------------------ vvv haskell-formatter vvv
mybinding
  = RH.performEvent_ $
      postBuild <&>
        \ () ->
          liftIO $
            do runMaybeT postCliInit >>=
                 \case
                     Nothing -> return ()
                     Just () -> do _ <- forkIO $
                                          postCliInitAsync `catch`
                                            \ (e :: SomeException) ->
                                              writeLogS LogLevelError (show e)
                                   return ()
--------------------------------------------------------------------------------
~~~~

## long type signature

~~~~.hs
--        10        20        30        40        50        60        70        80
--------------------------------------------------------------- vvv brittany vvv
linewise
  :: forall n t
   . (Ord n, R.ReflexHost t, MonadIO (R.PushM t), MonadIO (R.HostFrame t))
  => (  R.Event t Text -- command string executed by user
     -> R.Dynamic t (Maybe Text, Int, Text)
     -> R.Behavior t (Seq Text) -- history
     -> R.Event t ()   -- post-shutdown
     -> RH.AppHost
          t
          ( R.Event t () -- shutdown trigger
          , R.Behavior t String -- tab-completion value
          , R.Dynamic t (Widget n)
          )
     )
  -> RH.AppHost t ()
---------------------------------------------------------------- vvv hindent vvv
--                                                         (overflowing columns)
linewise ::
     forall n t.
     (Ord n, R.ReflexHost t, MonadIO (R.PushM t), MonadIO (R.HostFrame t))
  => (R.Event t Text -- command string executed by user
       -> R.Dynamic t (Maybe Text, Int, Text) -> R.Behavior t (Seq Text) -- history
                                                  -> R.Event t () -- post-shutdown
                                                      -> RH.AppHost t ( R.Event t () -- shutdown trigger
                                                                      , R.Behavior t String -- tab-completion value
                                                                      , R.Dynamic t (Widget n)))
  -> RH.AppHost t ()
------------------------------------------------------ vvv haskell-formatter vvv
linewise ::
         forall n t .
           (Ord n, R.ReflexHost t, MonadIO (R.PushM t),
            MonadIO (R.HostFrame t)) =>
           (R.Event t Text ->
              -- command string executed by user
              R.Dynamic t (Maybe Text, Int, Text) ->
                R.Behavior t (Seq Text) ->
                  -- history
                  R.Event t () ->
                    -- post-shutdown
                    RH.AppHost t
                      -- shutdown trigger
                      (R.Event t (), R.Behavior t String,
                       -- tab-completion value
                       R.Dynamic t (Widget n)))

             -> RH.AppHost t ()
--------------------------------------------------------------------------------
~~~~

## slighly longer mix of different constructs

~~~~.hs
--        10        20        30        40        50        60        70        80
--------------------------------------------------------------- vvv brittany vvv
completion :: String -> CommandDesc a -> String -> String
completion cmdline desc pcRest =
  List.drop (List.length lastWord) $ case choices of
    [] -> ""
    (c1:cr) ->
      headDef ""
        $ filter (\s -> List.all (s`isPrefixOf`) cr)
        $ reverse
        $ List.inits c1
 where
  nameDesc = case _cmd_mParent desc of
    Nothing                        -> desc
    Just (_, parent) | null pcRest -> parent
    Just{}                         -> desc
  lastWord = reverse $ takeWhile (not . isSpace) $ reverse $ cmdline
  choices  = join
    [ [ r
      | Just r <- Foldable.toList (_cmd_children nameDesc)
        <&> \(s, _) -> [ s | lastWord `isPrefixOf` s ]
      ]
    , [ s
      | s <- transPartDesc =<< _cmd_parts nameDesc
      , lastWord `isPrefixOf` s
      ]
    ]
  transPartDesc :: PartDesc -> [String]
  transPartDesc = \case
    PartLiteral  s      -> [s]
    PartVariable _      -> []
    PartOptional x      -> transPartDesc x
    PartAlts     alts   -> alts >>= transPartDesc
    PartSeq      []     -> []
    PartSeq      (x:_)  -> transPartDesc x
    PartDefault    _  x -> transPartDesc x
    PartSuggestion ss x -> ss ++ transPartDesc x
    PartRedirect   _  x -> transPartDesc x
    PartReorder xs      -> xs >>= transPartDesc
    PartMany    x       -> transPartDesc x
    PartWithHelp _h x   -> transPartDesc x
---------------------------------------------------------------- vvv hindent vvv
completion :: String -> CommandDesc a -> String -> String
completion cmdline desc pcRest =
  List.drop (List.length lastWord) $
  case choices of
    [] -> ""
    (c1:cr) ->
      headDef "" $
      filter (\s -> List.all (s `isPrefixOf`) cr) $ reverse $ List.inits c1
  where
    nameDesc =
      case _cmd_mParent desc of
        Nothing -> desc
        Just (_, parent)
          | null pcRest -> parent
        Just {} -> desc
    lastWord = reverse $ takeWhile (not . isSpace) $ reverse $ cmdline
    choices =
      join
        [ [ r
          | Just r <-
              Foldable.toList (_cmd_children nameDesc) <&> \(s, _) ->
                [s | lastWord `isPrefixOf` s]
          ]
        , [ s
          | s <- transPartDesc =<< _cmd_parts nameDesc
          , lastWord `isPrefixOf` s
          ]
        ]
    transPartDesc :: PartDesc -> [String]
    transPartDesc =
      \case
        PartLiteral s -> [s]
        PartVariable _ -> []
        PartOptional x -> transPartDesc x
        PartAlts alts -> alts >>= transPartDesc
        PartSeq [] -> []
        PartSeq (x:_) -> transPartDesc x
        PartDefault _ x -> transPartDesc x
        PartSuggestion ss x -> ss ++ transPartDesc x
        PartRedirect _ x -> transPartDesc x
        PartReorder xs -> xs >>= transPartDesc
        PartMany x -> transPartDesc x
        PartWithHelp _h x -> transPartDesc x
------------------------------------------------------ vvv haskell-formatter vvv
completion :: String -> CommandDesc a -> String -> String
completion cmdline desc pcRest
  = List.drop (List.length lastWord) $
      case choices of
          [] -> ""
          (c1 : cr) -> headDef "" $
                         filter (\ s -> List.all (s `isPrefixOf`) cr) $
                           reverse $ List.inits c1

  where nameDesc
          = case _cmd_mParent desc of
                Nothing -> desc
                Just (_, parent) | null pcRest -> parent
                Just{} -> desc
        lastWord = reverse $ takeWhile (not . isSpace) $ reverse $ cmdline
        choices
          = join
              [[r |
                Just r <- Foldable.toList (_cmd_children nameDesc) <&>
                            \ (s, _) -> [s | lastWord `isPrefixOf` s]],

               [s | s <- transPartDesc =<< _cmd_parts nameDesc,
                lastWord `isPrefixOf` s]]

        transPartDesc :: PartDesc -> [String]
        transPartDesc
          = \case
                PartLiteral s -> [s]
                PartVariable _ -> []
                PartOptional x -> transPartDesc x
                PartAlts alts -> alts >>= transPartDesc
                PartSeq [] -> []
                PartSeq (x : _) -> transPartDesc x
                PartDefault _ x -> transPartDesc x
                PartSuggestion ss x -> ss ++ transPartDesc x
                PartRedirect _ x -> transPartDesc x
                PartReorder xs -> xs >>= transPartDesc
                PartMany x -> transPartDesc x
                PartWithHelp _h x -> transPartDesc x
--------------------------------------------------------------------------------
~~~~

## another long example; full module

~~~~.hs
--        10        20        30        40        50        60        70        80
--------------------------------------------------------------- vvv brittany vvv
module Language.Haskell.Brittany.BriLayouter
  ( layoutBriDoc
  )
where



layoutBriDoc :: Data.Data.Data ast => ast -> BriDocNumbered -> PPM ()
layoutBriDoc ast briDoc = do
  -- first step: transform the briDoc.
  briDoc' <- MultiRWSS.withMultiStateS BDEmpty $ do
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
      . briDocToDoc
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
---------------------------------------------------------------- vvv hindent vvv
--                                                         (overflowing columns)
module Language.Haskell.Brittany.BriLayouter
  ( layoutBriDoc
  ) where

layoutBriDoc :: Data.Data.Data ast => ast -> BriDocNumbered -> PPM ()
layoutBriDoc ast briDoc
                 -- first step: transform the briDoc.
 = do
  briDoc' <-
    MultiRWSS.withMultiStateS BDEmpty $ do
      traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw $
        briDocToDoc $ unwrapBriDocNumbered $ briDoc
      -- bridoc transformation: remove alts
      transformAlts briDoc >>= mSet
      mGet >>=
        traceIfDumpConf "bridoc post-alt" _dconf_dump_bridoc_simpl_alt .
        briDocToDoc
      -- bridoc transformation: float stuff in
      mGet <&> transformSimplifyFloating >>= mSet
      mGet >>=
        traceIfDumpConf "bridoc post-floating" _dconf_dump_bridoc_simpl_floating .
        briDocToDoc
      -- bridoc transformation: par removal
      mGet <&> transformSimplifyPar >>= mSet
      mGet >>=
        traceIfDumpConf "bridoc post-par" _dconf_dump_bridoc_simpl_par .
        briDocToDoc
      -- bridoc transformation: float stuff in
      mGet <&> transformSimplifyColumns >>= mSet
      mGet >>=
        traceIfDumpConf "bridoc post-columns" _dconf_dump_bridoc_simpl_columns .
        briDocToDoc
      -- -- bridoc transformation: indent
      mGet <&> transformSimplifyIndent >>= mSet
      mGet >>=
        traceIfDumpConf "bridoc post-indent" _dconf_dump_bridoc_simpl_indent .
        briDocToDoc
      mGet >>=
        traceIfDumpConf "bridoc final" _dconf_dump_bridoc_final . briDocToDoc
  -- -- convert to Simple type
  -- simpl <- mGet <&> transformToSimple
  -- return simpl
  anns :: ExactPrint.Types.Anns <- mAsk
  let filteredAnns = filterAnns ast anns
  let state =
        LayoutState
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
  let remainingComments =
        Map.elems (_lstate_commentsPrior state') ++
        Map.elems (_lstate_commentsPost state')
  remainingComments `forM_`
    (mTell . (: []) . LayoutErrorUnusedComment . show . fmap fst)
  return $ ()
------------------------------------------------------ vvv haskell-formatter vvv
module Language.Haskell.Brittany.BriLayouter (layoutBriDoc) where

layoutBriDoc :: Data.Data.Data ast => ast -> BriDocNumbered -> PPM ()
layoutBriDoc ast briDoc
  -- first step: transform the briDoc.
  = do briDoc' <- MultiRWSS.withMultiStateS BDEmpty $
                    do traceIfDumpConf "bridoc raw" _dconf_dump_bridoc_raw $
                         briDocToDoc $ unwrapBriDocNumbered $ briDoc
                       -- bridoc transformation: remove alts
                       transformAlts briDoc >>= mSet
                       mGet >>=
                         traceIfDumpConf "bridoc post-alt"
                           _dconf_dump_bridoc_simpl_alt
                           . briDocToDoc
                       -- bridoc transformation: float stuff in
                       mGet <&> transformSimplifyFloating >>= mSet
                       mGet >>=
                         traceIfDumpConf "bridoc post-floating"
                           _dconf_dump_bridoc_simpl_floating
                           . briDocToDoc
                       -- bridoc transformation: par removal
                       mGet <&> transformSimplifyPar >>= mSet
                       mGet >>=
                         traceIfDumpConf "bridoc post-par"
                           _dconf_dump_bridoc_simpl_par
                           . briDocToDoc
                       -- bridoc transformation: float stuff in
                       mGet <&> transformSimplifyColumns >>= mSet
                       mGet >>=
                         traceIfDumpConf "bridoc post-columns"
                           _dconf_dump_bridoc_simpl_columns
                           . briDocToDoc
                       -- -- bridoc transformation: indent
                       mGet <&> transformSimplifyIndent >>= mSet
                       mGet >>=
                         traceIfDumpConf "bridoc post-indent"
                           _dconf_dump_bridoc_simpl_indent
                           . briDocToDoc
                       mGet >>=
                         traceIfDumpConf "bridoc final" _dconf_dump_bridoc_final
                           . briDocToDoc
       -- -- convert to Simple type
       -- simpl <- mGet <&> transformToSimple
       -- return simpl
       anns :: ExactPrint.Types.Anns <- mAsk
       let filteredAnns = filterAnns ast anns
       let state
             = LayoutState{_lstate_baseY = 0, _lstate_curY = 0,
                           _lstate_indLevel = 0, _lstate_indLevelLinger = 0,
                           _lstate_commentsPrior =
                             extractCommentsPrior filteredAnns,
                           _lstate_commentsPost =
                             extractCommentsPost filteredAnns,
                           _lstate_commentCol = Nothing,
                           _lstate_addSepSpace = Nothing,
                           _lstate_inhibitMTEL = False,
                           _lstate_isNewline = NewLineStateInit}

       state' <- MultiRWSS.withMultiStateS state $ layoutBriDocM briDoc'
       let remainingComments
             = Map.elems (_lstate_commentsPrior state') ++
                 Map.elems (_lstate_commentsPost state')
       remainingComments `forM_`
         (mTell . (: []) . LayoutErrorUnusedComment . show . fmap fst)
       return $ ()
~~~~
