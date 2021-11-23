-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
parserPrim =
  [ r
  | r <-
    [ SGPPrimFloat $ bool id (0 -) minus $ readGnok
        "parserPrim"
        (d1 ++ d2 ++ d3 ++ d4)
    | d2 <- string "."
    , d3 <- many1 (oneOf "0123456789")
    , _ <- string "f"
    ]
    <|> [ SGPPrimFloat $ bool id (0 -) minus $ fromIntegral
            (readGnok "parserPrim" d1 :: Integer)
        | _ <- string "f"
        ]
    <|> [ SGPPrimInt $ bool id (0 -) minus $ fromIntegral
            (readGnok "parserPrim" d1 :: Integer)
        | _ <- string "i"
        ]
  ]
