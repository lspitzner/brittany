{-# LANGUAGE ScopedTypeVariables #-}
data MyRecord
  = forall a b
  . ( Loooooooooooooooooooooooooooooooong a
    , Loooooooooooooooooooooooooooooooong b
    ) =>
    MyConstructor
      { a :: a
      , b :: b
      }
