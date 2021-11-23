data Foo = Bar
  {  -- a
    foo  -- b
         :: -- c
            Baz -- d
  , -- e
    bars :: Bizzz
  }
  deriving (Show, Eq, Monad, Functor, Traversable, Foldable)
