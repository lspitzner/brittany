instance MyClass Int where
  type MyType = String
  myMethod :: MyType -> Int
  myMethod x = x + 1
  type MyType = Int
