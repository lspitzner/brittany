{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
pattern Signed x <- (asSigned -> x) where
  Signed (Neg x) = -x
  Signed Zero    = 0
  Signed (Pos x) = x
