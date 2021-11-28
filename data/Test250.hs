{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
pattern Signed x <- (asSigned -> x) where
  Signed (Neg x) = -x -- negative comment
  Signed Zero    = 0  -- zero comment
  Signed (Pos x) = x  -- positive comment
