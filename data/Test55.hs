{-# LANGUAGE ScopedTypeVariables #-}
data MyStruct
  = forall a b
  . ( Loooooooooooooooooooooooooooooooong a
    , Loooooooooooooooooooooooooooooooong b
    ) =>
    MyConstructor (ToBriDocM BriDocNumbered)
                  (ToBriDocM BriDocNumbered)
                  (ToBriDocM BriDocNumbered)
