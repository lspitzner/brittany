{-# LANGUAGE ScopedTypeVariables #-}
-- brittany { lconfig_allowSinglelineRecord: true }
data MyRecord = forall a . Show a => Bar
  { foo :: abittoolongbutnotvery -> abittoolongbutnotvery
  }
