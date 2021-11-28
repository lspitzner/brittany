{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
pattern Signed xxxxxxxxxxxxxxxxxxxxxxxx <-
  (asSigned -> xxxxxxxxxxxxxxxxxxxxxxxx) where
  Signed (Neg x) = -x
  Signed Zero    = 0
  Signed (Pos x) = x
