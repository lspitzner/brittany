-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
func = do
  s <- mGet
  mSet $ s { _lstate_indent = _lstate_indent state }
