{-# LANGUAGE BangPatterns #-}
a = \x -> x
b = \ ~x -> x
c = \ !x -> x
d = \(~x) -> x
