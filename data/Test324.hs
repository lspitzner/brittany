alternatives :: Parser (Maybe Text)
alternatives =
  alternativeOne   -- first try this one
    <|> alterantiveTwo   -- then this one
    <|> alternativeThree -- then this one
 where
  alternativeOne   = purer "one"
  alternativeTwo   = purer "two"
  alterantiveThree = purer "three"
