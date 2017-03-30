invocation:

~~~~
> brittany --dump-ast-full
~~~~

input (via stdin, remember ctrl-d to end-of-file):

~~~~
id :: a -> a
~~~~

output (all but the last line is stderr):

~~~~
---- ast ----
A Just (Ann (DP (0,0)) [] [] [((G AnnEofPos),DP (1,0))] Nothing Nothing)
  HsModule
    Nothing
    Nothing
    []
    [ A Just (Ann (DP (0,0)) [] [] [((G AnnDcolon),DP (0,1))] Nothing Nothing)
        SigD
          TypeSig
            [ A Just (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
                Unqual {OccName: id}
            ]
            HsIB
              PlaceHolder
              HsWC
                PlaceHolder
                Nothing
                A Just (Ann (DP (0,1)) [] [] [((G AnnRarrow),DP (0,1))] Nothing Nothing)
                  HsFunTy
                    A Just (Ann (DP (0,0)) [] [] [] Nothing Nothing)
                      HsAppsTy
                        [ A Just (Ann (DP (0,0)) [] [] [] Nothing Nothing)
                            HsAppPrefix
                              A Just (Ann (DP (0,0)) [] [] [] Nothing Nothing)
                                HsTyVar
                                  A Just (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
                                    Unqual {OccName: a}
                        ]
                    A Just (Ann (DP (0,1)) [] [] [] Nothing Nothing)
                      HsAppsTy
                        [ A Just (Ann (DP (0,0)) [] [] [] Nothing Nothing)
                            HsAppPrefix
                              A Just (Ann (DP (0,0)) [] [] [] Nothing Nothing)
                                HsTyVar
                                  A Just (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
                                    Unqual {OccName: a}
                        ]
    ]
    Nothing
    Nothing
----
id :: a -> a
~~~~

