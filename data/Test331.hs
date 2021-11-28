go l [] = Right l
go l ((IRType, _a) : eqr) = go l eqr
go l ((_, IRType) : eqr) = go l eqr
go _ ((IRTypeError ps t1 t2, _) : _) = Left $ makeError ps t1 t2
go _ ((_, IRTypeError ps t1 t2) : _) = Left $ makeError ps t1 t2
