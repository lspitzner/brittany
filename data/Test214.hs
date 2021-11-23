{-# language TypeFamilies #-}
module M where
data family F a
data instance F Int = D Int
