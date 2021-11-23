{-# LANGUAGE PatternSynonyms #-}
pattern myLeftVariableName `MyInfixPatternMatcher` myRightVariableName <-
  [myLongLeftVariableName, myLongRightVariableName] where
  MyInfixPatternMatcher x y = [x, x, y]
