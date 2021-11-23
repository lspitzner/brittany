-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
isValidPosition position
  | validX && validY = Just position
  | otherwise = Nothing
