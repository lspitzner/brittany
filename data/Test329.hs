alternatives :: Parser (Maybe Text)
alternatives = -- a
  ( -- b
      alternativeOne   -- c
  <|> alterantiveTwo   -- d
  <|> alternativeThree -- e
  ) -- f
