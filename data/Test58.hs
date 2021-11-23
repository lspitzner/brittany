data Foo = Bar
  { foo  :: Baz
  , bars :: Bizzz
  }
  deriving Show
  deriving (Eq, Ord)
  deriving stock Show
  deriving stock (Eq, Ord)
  deriving anyclass Show
  deriving anyclass (Show, Eq, Monad, Functor)
  deriving newtype Show
  deriving newtype (Traversable, Foldable)
