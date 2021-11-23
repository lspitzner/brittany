{-# LANGUAGE DatatypeContexts #-}
data
  ( LooooooooooooooooooooongConstraint a
  , LooooooooooooooooooooongConstraint b
  ) =>
  MyRecord a b
  = MyConstructor
    { foo1, foo2
        :: loooooooooooooooooooooooooooooooong
        -> loooooooooooooooooooooooooooooooong
    , bar  :: a
    , bazz :: b
    }
