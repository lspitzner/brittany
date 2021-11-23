instance Foo Int where
  newtype Bar Int = BarInt
    { unBarInt :: Int
    }
