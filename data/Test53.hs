{-# LANGUAGE ScopedTypeVariables #-}
data MyRecord
  = forall a
  . LooooooooooooooooooooongConstraint a =>
    LoooooooooooongConstructor
      { foo :: abittoolongbutnotvery -> abittoolongbutnotvery
      }
