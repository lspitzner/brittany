invocation:

~~~~
> brittany --dump-bridoc-alt --dump-bridoc-final
~~~~

input (via stdin, remember ctrl-d to end-of-file):

~~~~
x :: Maybe Int
~~~~

output (all but the last line is stderr):

~~~~
---- bridoc post-alt ----
BDSeq [BDSeq [BDLit (pack "x"),BDSeparator],BDSeq [BDLit (pack "::"),BDSeparator],BDSeq [BDLit (pack "Maybe"),BDLit (pack " "),BDLit (pack "Int")]]
---- bridoc final ----
BDSeq [BDLit (pack "x"),BDSeparator,BDLit (pack "::"),BDSeparator,BDLit (pack "Maybe"),BDLit (pack " "),BDLit (pack "Int")]
----
x :: Maybe Int
~~~~

