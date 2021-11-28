{-# language TypeFamilies #-}
class C a where
  data family F a
instance C Int where
  data F Int = D Int
