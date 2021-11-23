instance MyClass Int where
  -- | This data is important
  data MyData = Test Int Int
  myMethod :: MyData -> Int
  myMethod = intData2
  -- | This data is also important
  data MyData2 = IntData
    { intData  :: String
    -- ^ Interesting field
    , intData2 :: Int
    }
