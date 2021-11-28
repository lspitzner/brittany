{-# LANGUAGE TypeFamilies #-}
f :: ((~) a b) => a -> b
f = id
