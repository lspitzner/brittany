{-# language TypeFamilies #-}
type family F a
type instance F Int = IO Int -- x
