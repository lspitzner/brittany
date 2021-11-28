-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
func
  :: Int -- basic indentation amount
  -> Int -- currently used width in current line (after indent)
         -- used to accurately calc placing of the current-line
  -> LayoutDesc
  -> Int
