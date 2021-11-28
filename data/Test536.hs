-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
{-# LANGUAGE TypeApplications #-}
layoutPatternBindFinal alignmentToken binderDoc mPatDoc clauseDocs = do
  docAlt
    $  -- one-line solution
      [ docCols
          (ColBindingLine alignmentToken)
          [ docSeq (patPartInline ++ [guardPart])
          , docSeq
            [ appSep $ return binderDoc
            , docForceSingleline $ return body
            , wherePart
            ]
          ]
      | not hasComments
      , [(guards, body, _bodyRaw)] <- [clauseDocs]
      , let guardPart = singleLineGuardsDoc guards
      , wherePart <- case mWhereDocs of
        Nothing -> return @[] $ docEmpty
        Just [w] -> return @[] $ docSeq
          [ docSeparator
          , appSep $ docLit $ Text.pack "where"
          , docSetIndentLevel $ docForceSingleline $ return w
          ]
        _ -> []
      ]
    ++ -- one-line solution + where in next line(s)
       [ docLines
         $ [ docCols
               (ColBindingLine alignmentToken)
               [ docSeq (patPartInline ++ [guardPart])
               , docSeq
                 [appSep $ return binderDoc, docForceParSpacing $ return body]
               ]
           ]
         ++ wherePartMultiLine
       | [(guards, body, _bodyRaw)] <- [clauseDocs]
       , let guardPart = singleLineGuardsDoc guards
       , Data.Maybe.isJust mWhereDocs
       ]
    ++ -- two-line solution + where in next line(s)
       [ docLines
         $ [ docForceSingleline
             $ docSeq (patPartInline ++ [guardPart, return binderDoc])
           , docEnsureIndent BrIndentRegular $ docForceSingleline $ return
             body
           ]
         ++ wherePartMultiLine
       | [(guards, body, _bodyRaw)] <- [clauseDocs]
       , let guardPart = singleLineGuardsDoc guards
       ]
