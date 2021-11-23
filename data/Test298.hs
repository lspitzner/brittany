parserCompactLocation =
  [ try
      $ [ ParseRelAbs (Text.Read.read digits) _ _
        | digits <- many1 digit
        , rel1 :: Maybe (Either Int (Ratio Int)) <- optionMaybe
          [ case divPart of
              Nothing -> Left $ Text.Read.read digits
              Just ddigits ->
                Right $ Text.Read.read digits % Text.Read.read ddigits
          | digits  <- many1 digit
          , divPart <- optionMaybe (string "/" *> many1 digit)
          ]
        ]
  ]
