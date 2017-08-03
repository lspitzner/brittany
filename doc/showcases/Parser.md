
### Input

contains some very long lines and strange formatting of the
list/monad-comprehension:

~~~~.hs
parseCalUnits :: forall m . (MonadMultiReader ParserWrapperGuard m, MonadMultiState ParserFileCache m, MonadIO m) => [GenericParser] -> [FilePath] -> m (Either Text [CalUnit])
parseCalUnits parsers inputs = fmap (fmap join . sequence) $ inputs `forM` \path -> do
  readCachedFile path (\raw -> parseChunks $ createChunks $ (Text.pack path, raw))
 where

  createChunks :: (Text, Text) -> [(Text, [(Int, InputLine)])]
  createChunks (file, input) = fmap ((,) file) $ groupBy (\a b -> grouper (snd a) (snd b)) $ dropWhile (lineIsSpace . snd) $ zip [1 ..] $ lineMapper <$> Text.lines input
   where
    headerParser :: Parser (Text, Bool, Text)
    headerParser =
      [ (Text.pack name, isMain, Text.pack typ)
        | _ <- string "node", _ <- many1 $ oneOf " \t",
          isMain <- fmap isJust $ optionMaybe (string "*" *> many1 (oneOf " \t")),
          name <- many1 $ noneOf " \t\r\n:",
          _ <- many $ oneOf " \t",
          _ <- char ':',
          _ <- many $ oneOf " \t",
          typ <- many1 $ satisfy $ not . isSpace,
          _ <- many $ oneOf " \t",
          _ <- eof ]
    lineMapper :: Text -> InputLine
    lineMapper line = case runParser headerParser () "" line of
      Left  _e -> NormalLine line
      Right (n, m, t) -> HeaderLine n m t
    lineIsSpace :: InputLine -> Bool
    lineIsSpace (NormalLine x) = Text.null $ Text.strip x
    lineIsSpace _ = False
    grouper :: InputLine -> InputLine -> Bool
    grouper _ HeaderLine{} = False
    grouper _ _ = True

  parseChunks :: [(Text, [(Int, InputLine)])] -> m (Either Text [CalUnit])
  parseChunks nodes = runEitherT $ sequence (uncurry parseNode <$> nodes)
   where
    findParser :: Text -> Maybe GenericParser
    findParser pid = find (\(GenericParser p) -> parser_ident p == pid) parsers
    parseNode :: Text -> [(Int, InputLine)] -> EitherT Text m CalUnit
    parseNode file ((n, HeaderLine name isMain typ):rlines) = do
      (GenericParser p) <- case findParser typ of
        Nothing -> left $ Text.pack "could not find parser for node \"" <> name <> Text.pack "\" of type \"" <> typ <> Text.pack "\" at " <> file <> Text.pack (": " ++ show n ++ ".")
        Just x -> return x
      let firstLine = fromMaybe 0 $ fst <$> listToMaybe rlines
      ParserDataStore dstore version <- _pfc_store <$> mGet
      let postfix = Text.pack "\n(When parsing node \"" <> name <> Text.pack "\")"
      parsed <- bimapEitherT (<>postfix) id $ hoistEither $ parser_parse p file firstLine $ Text.unlines $ [ fst $ Text.breakOn (Text.pack "--") line | (_, NormalLine line) <- rlines ]
      let oldDat = case M.lookup name dstore of
            Nothing -> parser_zero p $> version
            Just s -> case decodeOrFail s of
              Right (rest, _, decoded) | BSL.null rest -> decoded
              _ -> parser_zero p $> version
      let (newDat, delta) = parser_diff p version oldDat (parsed $> version)
      pfc <- mGet
      mSet $ pfc { _pfc_store = ParserDataStore (M.insert name (encode newDat) dstore) version }
      let calUnit = parser_build p version name (newDat, delta)
      return $ CalUnit isMain calUnit
    parseNode file ((n, _):_) = left $ Text.pack "expected node definition at " <> file <> Text.pack (": " ++ show n ++ ".")
    parseNode file _ = left $ Text.pack "expected node definition at " <> file <> Text.pack "."
~~~~.hs

### Brittany 0.8.0.1 output on default settings

~~~~.hs
parseCalUnits
  :: forall m
   . ( MonadMultiReader ParserWrapperGuard m
     , MonadMultiState ParserFileCache m
     , MonadIO m
     )
  => [GenericParser]
  -> [FilePath]
  -> m (Either Text [CalUnit])
parseCalUnits parsers inputs =
  fmap (fmap join . sequence) $ inputs `forM` \path -> do
    readCachedFile
      path
      (\raw -> parseChunks $ createChunks $ (Text.pack path, raw))
 where

  createChunks :: (Text, Text) -> [(Text, [(Int, InputLine)])]
  createChunks (file, input) =
    fmap ((,) file)
      $   groupBy (\a b -> grouper (snd a) (snd b))
      $   dropWhile (lineIsSpace . snd)
      $   zip [1 ..]
      $   lineMapper
      <$> Text.lines input
   where
    headerParser :: Parser (Text, Bool, Text)
    headerParser =
      [ (Text.pack name, isMain, Text.pack typ)
      | _      <- string "node"
      , _      <- many1 $ oneOf " \t"
      , isMain <- fmap isJust $ optionMaybe (string "*" *> many1 (oneOf " \t"))
      , name   <- many1 $ noneOf " \t\r\n:"
      , _      <- many $ oneOf " \t"
      , _      <- char ':'
      , _      <- many $ oneOf " \t"
      , typ    <- many1 $ satisfy $ not . isSpace
      , _      <- many $ oneOf " \t"
      , _      <- eof
      ]
    lineMapper :: Text -> InputLine
    lineMapper line = case runParser headerParser () "" line of
      Left  _e        -> NormalLine line
      Right (n, m, t) -> HeaderLine n m t
    lineIsSpace :: InputLine -> Bool
    lineIsSpace (NormalLine x) = Text.null $ Text.strip x
    lineIsSpace _              = False
    grouper :: InputLine -> InputLine -> Bool
    grouper _ HeaderLine{} = False
    grouper _ _            = True

  parseChunks :: [(Text, [(Int, InputLine)])] -> m (Either Text [CalUnit])
  parseChunks nodes = runEitherT $ sequence (uncurry parseNode <$> nodes)
   where
    findParser :: Text -> Maybe GenericParser
    findParser pid = find (\(GenericParser p) -> parser_ident p == pid) parsers
    parseNode :: Text -> [(Int, InputLine)] -> EitherT Text m CalUnit
    parseNode file ((n, HeaderLine name isMain typ):rlines) = do
      (GenericParser p) <- case findParser typ of
        Nothing ->
          left
            $  Text.pack "could not find parser for node \""
            <> name
            <> Text.pack "\" of type \""
            <> typ
            <> Text.pack "\" at "
            <> file
            <> Text.pack (": " ++ show n ++ ".")
        Just x -> return x
      let firstLine = fromMaybe 0 $ fst <$> listToMaybe rlines
      ParserDataStore dstore version <- _pfc_store <$> mGet
      let postfix =
            Text.pack "\n(When parsing node \"" <> name <> Text.pack "\")"
      parsed <-
        bimapEitherT (<>postfix) id
        $ hoistEither
        $ parser_parse p file firstLine
        $ Text.unlines
        $ [ fst $ Text.breakOn (Text.pack "--") line
          | (_, NormalLine line) <- rlines
          ]
      let oldDat = case M.lookup name dstore of
            Nothing -> parser_zero p $> version
            Just s  -> case decodeOrFail s of
              Right (rest, _, decoded) | BSL.null rest -> decoded
              _ -> parser_zero p $> version
      let (newDat, delta) = parser_diff p version oldDat (parsed $> version)
      pfc <- mGet
      mSet $ pfc
        { _pfc_store = ParserDataStore (M.insert name (encode newDat) dstore)
                                       version
        }
      let calUnit = parser_build p version name (newDat, delta)
      return $ CalUnit isMain calUnit
    parseNode file ((n, _):_) =
      left $ Text.pack "expected node definition at " <> file <> Text.pack
        (": " ++ show n ++ ".")
    parseNode file _ =
      left $ Text.pack "expected node definition at " <> file <> Text.pack "."
~~~~
