-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
layoutWriteNewlineBlock
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => m ()
