{-# LANGUAGE PatternSynonyms #-}
pattern HeadC x <- x : xs where
  HeadC x = [x]
