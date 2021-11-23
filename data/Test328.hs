func = do
  createDirectoryIfMissing True path
  openFile fileName AppendMode
