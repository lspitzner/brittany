{-# LANGUAGE BangPatterns #-}
func = do
  let !forced = some
  pure ()
