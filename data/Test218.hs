{-# language TypeFamilies #-}
class C a where
  data family F a
instance C Int where
  newtype F Int = N Int
