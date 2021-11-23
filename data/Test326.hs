spanKey p q = case minViewWithKey q of
  Just ((k, _), q') | p k ->
    let (kas, q'') = spanKey p q' in ((k, a) : kas, q'')
  _ -> ([], q)
