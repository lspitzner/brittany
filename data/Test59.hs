data Foo = Bar
  { foo  :: Baz
  , bars :: Bizzz
  }
  deriving ToJSON via (SomeType)
  deriving (ToJSON, FromJSON) via (SomeType)
