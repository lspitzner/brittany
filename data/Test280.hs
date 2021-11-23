func =
  [ (thing, take 10 alts) --TODO: select best ones
  | (thing, _got, alts@(_ : _)) <- nosuchFooThing
  , gast                        <- award
  ]
