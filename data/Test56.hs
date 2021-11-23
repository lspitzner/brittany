{-# LANGUAGE ScopedTypeVariables #-}
data MyRecord
  = forall a b
  . ( Loooooooooooooooooooooooooooooooong a
    , Loooooooooooooooooooooooooooooooong b
    ) =>
    MyConstructor
      { foo, foo2
          :: loooooooooooooooooooooooooooooooong
          -> loooooooooooooooooooooooooooooooong
      , bar  :: a
      , bazz :: b
      }
  deriving Show
