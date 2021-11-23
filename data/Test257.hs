{-# LANGUAGE QuasiQuotes #-}
func = do
  let body = [json|
  hello
  |]
  pure True
