layoutWriteNewlineBlock
  :: ( MonadMultiWriter Text.Builder.Builder m
     , MonadMultiState LayoutState m
     , MonadMultiWriter (Seq String) m
     )
  => m ()
