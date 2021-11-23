{-# LANGUAGE ScopedTypeVariables #-}
func
  :: forall m
   . Foo
  => ColMap2
  -> ColInfo
  -> ColInfo
  -> ColInfo
  -> ColInfo
  -> m ()
