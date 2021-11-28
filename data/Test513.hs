-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
{-# LANGUAGE MultiWayIf #-}
readMergePersConfig path shouldCreate conf = do
  exists <- liftIO $ System.Directory.doesFileExist path
  if
    | exists -> do
      contents <- liftIO $ ByteString.readFile path -- no lazy IO, tyvm.
      fileConf <- case Data.Yaml.decodeEither contents of
        Left e -> do
          liftIO
            $ putStrErrLn
            $ "error reading in brittany config from "
            ++ path
            ++ ":"
          liftIO $ putStrErrLn e
          mzero
        Right x -> return x
      return $ fileConf Semigroup.<> conf
    | shouldCreate -> do
      liftIO $ ByteString.writeFile path $ Data.Yaml.encode $ cMap
        (Option . Just . runIdentity)
        staticDefaultConfig
      return $ conf
    | otherwise -> do
      return conf
