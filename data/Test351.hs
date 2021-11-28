createRedirectedProcess processConfig = do
  let redirectedProc = (_processConfig_inner processConfig)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  foo
