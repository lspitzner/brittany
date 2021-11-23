{-# LANGUAGE ExistentialQuantification #-}
data Foo = forall a . Show a => Bar
  { foo :: a
  }
