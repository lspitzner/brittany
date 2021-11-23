instance MyClass Int where
  -- | This data is very important
  data MyData = IntData
    { intData  :: String
    , intData2 :: Int
    }
  myMethod :: MyData -> Int
  myMethod = intData2
