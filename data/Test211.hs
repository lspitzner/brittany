{-# language TypeFamilies #-}
module M where
data family F a
newtype instance F Int = N Int
