{-# language TypeFamilies #-}
class C a where
  type family F a
instance C Int where
  type F Int = IO Int -- x
