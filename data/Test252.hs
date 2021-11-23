{-# LANGUAGE PatternSynonyms #-}
pattern LongMatcher
  :: longlongtypevar
  -> longlongtypevar
  -> longlongtypevar
  -> Maybe [longlongtypevar]
pattern LongMatcher x y z = Just [x, y, z]
