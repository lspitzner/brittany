{-# LANGUAGE ExistentialQuantification #-}
data MyRecord
  -- test comment
  = forall a b
  . ( Loooooooooooooooooooooooooooooooong a
    , Loooooooooooooooooooooooooooooooong b
    ) =>
    MyConstructor a b
