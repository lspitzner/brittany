{-# LANGUAGE RecursiveDo #-}
foo = do
  rec -- comment
      a <- f b
      b <- g a
  return (a, b)
