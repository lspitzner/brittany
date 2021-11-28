{-# LANGUAGE RecursiveDo #-}
foo = do
  rec a <- f b
      b <- g a
  return (a, b)
