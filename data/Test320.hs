fmapuv :: U.Unbox a => (a -> b) -> U.Vector a -> V.Vector b
fmapuv f xs = G.generate (G.length xs) (f . (xs G.!))
