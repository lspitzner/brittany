{-# LANGUAGE MagicHash, UnboxedTuples #-}
spanKey :: (# Int#, Int# #) -> (# Int#, Int# #)
spanKey = case foo of
  (# bar#, baz# #) -> (# baz# +# bar#, bar# #)
