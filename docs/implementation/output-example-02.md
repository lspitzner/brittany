invocation:

~~~~
> brittany --dump-bridoc-raw
~~~~

input (via stdin, remember ctrl-d to end-of-file):

~~~~
x :: Maybe Int
~~~~

output (all but the last line is stderr):

~~~~
---- bridoc raw ----
BDAlt
  [ BDSeq
      [ BDSeq [BDLit (pack "x"),BDSeparator]
      , BDSeq [BDLit (pack "::"),BDSeparator]
      , BDForceSingleline
          BDAlt
            [ BDSeq [BDForceSingleline (BDLit (pack "Maybe")),BDLit (pack " "),BDForceSingleline (BDLit (pack "Int"))]
            , BDPar BrIndentNone (BDLit (pack "Maybe")) (BDLines [BDEnsureIndent BrIndentRegular (BDLit (pack "Int"))])
            ]
      ]
  , BDAddBaseY
      BrIndentRegular
      BDPar
        BrIndentNone
        BDLit (pack "x")
        BDCols
          ColTyOpPrefix
          [ BDLit (pack ":: ")
          , BDAddBaseY
              BrIndentSpecial 3
              BDAlt
                [ BDSeq [BDForceSingleline (BDLit (pack "Maybe")),BDLit (pack " "),BDForceSingleline (BDLit (pack "Int"))]
                , BDPar BrIndentNone (BDLit (pack "Maybe")) (BDLines [BDEnsureIndent BrIndentRegular (BDLit (pack "Int"))])
                ]
          ]
  ]
----
x :: Maybe Int
~~~~

