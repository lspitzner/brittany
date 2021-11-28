{-# language TypeFamilies #-}
data family F a
newtype instance F Int = N Int
