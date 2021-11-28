data Foo = Bar
  { foo  :: Baz
  , bars :: Bizzz
  }
  -- a
  deriving --b
           ( -- c
            ToJSON -- d
                  , -- e
                    FromJSON --f
                            ) -- g
