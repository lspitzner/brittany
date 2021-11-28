data Foo = Bar
  { foo  :: Baz
  , bars :: Bizzz
  }
  -- a
  deriving --a
           ToJSON --b
                  via  -- c
                      ( -- d
                       SomeType --e
                               , -- f
                                 ABC --g
                                    )
