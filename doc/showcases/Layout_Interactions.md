# Syntactical element interaction example layouts

Last updated for brittany version `0.8.0.1`.

Brittany would layout the following bindings as displayed here. If you change
only the layout of these bindings in some way (e.g. if some lines overflowed
80 columns) and pass it through brittany, you would again get the below
versions.


#### Nested ifs

~~~~.hs
mybinding = if condition1
  then if condition2
    then if condition3 then 0 else 1
    else if condition3 then 2 else 3
  else 4
~~~~

#### if -> case -> do

~~~~.hs
mybinding = if GHC.xopt GHC.Cpp dynFlags
  then case cppMode of
    CPPModeAbort -> do
      return $ Left "Encountered -XCPP. Aborting."
    CPPModeWarn -> do
      putStrErrLn
        $  "Warning: Encountered -XCPP."
        ++ " Be warned that -XCPP is not supported and that"
        ++ " brittany cannot check that its output is syntactically"
        ++ " valid in its presence."
      return $ Right True
    CPPModeNowarn -> return $ Right True
  else return $ Right False
~~~~

#### single line ending with start of do-block

~~~~.hs
mybinding = RH.performEvent_ $ postBuild <&> \() -> liftIO $ do
  runMaybeT postCliInit >>= \case
    Nothing -> return ()
    Just () -> do
      _ <- forkIO $ postCliInitAsync `catch` \(e :: SomeException) ->
        writeLogS LogLevelError (show e)
      return ()
~~~~

#### record-syntax + do-block

~~~~.hs
myBinding = Booh
  { booh_id     = name
  , booh_parser = name
  , booh_query  = someLongFunction name ["thingy"] $ do
    cps            <- zu (Text.pack "thingy")
    SampleRate sri <- askConfig' conf_defaultSampleRate
    buildLinearState myBinding [cps]
  }
~~~~

