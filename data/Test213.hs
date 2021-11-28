{-# language TypeFamilies #-}
data family F a
data instance F Int = D Int -- x
