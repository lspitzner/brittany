# Horizontal alignment example layouts

Last updated for brittany version `0.8.0.1`.

Brittany would layout the following bindings as displayed here. If you change
only the layout of these bindings in some way (e.g. if you initially entered
these without any alignment) and pass it through brittany, you would (again)
get the below versions.


#### basic nested alignment example

~~~~.hs
func (MyLongFoo abc def) = 1
func (Bar       a   d  ) = 2
func _                   = 3
~~~~

#### alignment of function args and monadcomp bindings

~~~~.hs
myBinding =
  [ [ [ LeguEosb r1 (n2 - r1) fxvoNz    ymuSreje v
      , LeguEosb n2 (f3 - n2) oyphEmedn ymuSreje v
      , LeguEosb f3 (i4 - f3) fxvoNz    ymuSreje v
      , LeguEosb i4 (v5 - i4) oieha     ymuSreje v
      , LeguEosb v5 (j6 - v5) fxvoNz    ymuSreje v
      ]
    | oyphEmedn <- sdliWmguje
    , oieha     <- ohzvIp
    ]
  | v5 < j6
  , sdliWmguje <- zedoaregeuKilb tua1
  , ohzvIp     <- zedoaregeuKilb (0 - loy2)
  ]
~~~~

#### pattern matching

If same number of pattern args, there is sub-alignment.
Types are not inspected in any way for this.

~~~~.hs
transPartDesc = \case
  PartLiteral  s      -> [s]
  PartVariable _      -> []
  PartOptional x      -> transPartDesc x
  PartAlts     alts   -> alts >>= transPartDesc
  PartSeq      []     -> []
  PartSeq      (x:_)  -> transPartDesc x
  PartDefault    _  x -> transPartDesc x
  PartSuggestion ss x -> ss ++ transPartDesc x
  PartRedirect   _  x -> transPartDesc x
  PartReorder xs      -> xs >>= transPartDesc
  PartMany    x       -> transPartDesc x
  PartWithHelp _h x   -> transPartDesc x
~~~~

#### record syntax, field alignment

~~~~.hs
action = do
  startA <- transRelPos start
  s1     <- get
  modify $ \s2 -> s2 { _ts_sequencePos = Nothing
                     , _ts_curNote     = _ts_curNote s1
                     , _ts_curTrans    = _ts_curTrans s1
                     , _ts_curBar      = _ts_curBar s1
                     , _ts_curBase     = _ts_curBase s1
                     }
~~~~

#### items that are not single-line break up alignment

~~~~.hs
action = do
  _       <- string "set"
  bindId  <- onlySpaces *> ((,) <$> getPosition <*> parserIdent)
  marg    <- onlySpaces *> optionMaybe (char '(' *> parserParamDef <* char ')')
  localId <-
    onlySpaces *> (optionMaybe $ string "in" *> onlySpaces *> parserIdent)
  _    <- onlySpaces *> string "="
  expr <- spaces *> parserExpr
  pure $ CompNotParseItemBind bindId marg expr localId
~~~~

consequently these `<-` are not aligned at all:

~~~~.hs
action = do
  normVal :: Float <- expectParamE
    =<< expectDynamic (Text.pack "foldInNormalize")
  ResponseNode folderC folderN <- uncurry callBExpectS
    =<< getKalinAndParams (Text.pack "foldInFolder")
  ResponseNode foldeeC foldeeN <- uncurry callBExpectS
    =<< getKalinAndParams (Text.pack "foldInFoldee")
  pure $ calc normVal folderC folderN foldeeC foldeeN
~~~~
