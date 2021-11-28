{-# LANGUAGE TypeFamilies #-}
module Lib where
instance Foo () where
  newtype Bar () = Baz ()
    deriving (Eq, Ord, Show)
  bar = Baz
