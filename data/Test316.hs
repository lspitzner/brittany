-- The spaces before commas are undesirable
test :: Proxy '[ 'True ]
test :: Proxy '[True]
test :: Proxy '[ 'True , False ]
test :: Proxy '[True , False]
test :: Proxy '[True , 'False]
test :: Proxy '[ 'True , 'False ]
test :: Proxy '[ 'Just 'True , False ]
test :: Proxy '[Just True , False]
test :: Proxy '[('True)]
test :: Proxy ('Just 'True)
test :: Proxy ('True)

test = Proxy @'[ 'True ]
test = Proxy @'[True]
test = Proxy @'[ 'True , False ]
test = Proxy @'[True , False]
test = Proxy @'[True , 'False]
test = Proxy @'[ 'True , 'False ]
test = Proxy @'[ 'Just 'True , False ]
test = Proxy @'[Just True , False]
test = Proxy @'[('True)]
test = Proxy @('Just 'True)
test = Proxy @('True)

test
  :: Proxy '[-- comment
              'True ]
test
  :: Proxy '[-- comment
             True]
test
  :: Proxy '[{- comment -}
              'True ]
test
  :: Proxy '[{- comment -}
             True]

test =
  Proxy @'[-- comment
            'True ]
test =
  Proxy @'[-- comment
           True]
test =
  Proxy @'[{- comment -}
            'True ]
test =
  Proxy @'[{- comment -}
           True]
