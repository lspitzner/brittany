{-# language TypeFamilies #-}
module M where
type family F a
type instance F Int = IO Int
