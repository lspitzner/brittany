{-# LANGUAGE PatternSynonyms #-}
pattern Head2 x y <- x : y : xs where
  Head2 x y = [x, y]
