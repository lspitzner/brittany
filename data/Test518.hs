-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
foo = Reflex.runSpiderHost $ ReflexHost.hostApp $ do
  (inputEvent :: Reflex.Event Reflex.Spider String, inputFire :: String
    -> IO Bool) <-
    ReflexHost.newExternalEvent
  liftIO . forkIO . forever $ getLine >>= inputFire
  ReflexHost.performEvent_ $ fmap (liftIO . putStrLn) inputEvent
