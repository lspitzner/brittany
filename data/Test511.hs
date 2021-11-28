-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
wrapPatPrepend pat prepElem = do
  patDocs <- layoutPat pat
  case Seq.viewl patDocs of
    Seq.EmptyL -> return $ Seq.empty
    x1 Seq.:< xR -> do
      x1' <- docSeq [prepElem, return x1]
      return $ x1' Seq.<| xR
