testMethod foo bar baz qux =
  let x = undefined foo bar baz qux qux baz bar :: String
  -- some comment explaining the in expression
  in  undefined foo x :: String
