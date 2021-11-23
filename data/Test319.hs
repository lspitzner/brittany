widgetsDyn =
  [ [ vBox
        [ padTop Max outputLinesWidget
        , padRight Max wid1 <+> flowWidget  -- alignment here is strange/buggy
        , padBottom (Pad 5) help
        ]
    ]
  | wid1              <- promptDyn
  , (flowWidget, _)   <- flowResultD
  , outputLinesWidget <- outputLinesWidgetD
  , help              <- suggestionHelpBox
  , parser            <- cmdParserD
  ]
