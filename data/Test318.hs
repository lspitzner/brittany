{-# LANGUAGE RankNTypes, KindSignatures #-}
func
  :: forall m str
   . (Str str, Monad m)
  => Int
  -> Proxy (str :: [*])
  -> m (Tagged str String)
