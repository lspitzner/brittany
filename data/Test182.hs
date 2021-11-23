{-# LANGUAGE StarIsType #-}
type MySynonym (a :: * -> *)
  =  MySynonym a b
  -> MySynonym a b
  -> MyParamType a b
  -> MyParamType a b
  -> MySynonym2 b a
