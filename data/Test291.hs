autocheckCases =
  [ ("Never Deadlocks"  , representative deadlocksNever)
  , ("No Exceptions"    , representative exceptionsNever)
  , ("Consistent Result", alwaysSame) -- already representative
  ]
