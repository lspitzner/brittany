foldrDesc f z = unSwitchQueue $ \q ->
  switch (Min.foldrDesc (f unTaggedF) z q) (Min.foldrAsc (f unTaggedF) z q)
