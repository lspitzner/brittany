# Example type signature layouts

Last updated for brittany version `0.8.0.1`.

Brittany would layout the following signatures as displayed here. If you change
only the layout of these signatures in some way (e.g. if you initially entered
them as one-liners) and pass it through brittany, you would (again) get the
below versions.

~~~~.hs
docExt
  :: (ExactPrint.Annotate.Annotate ast)
  => Located ast
  -> ExactPrint.Types.Anns
  -> Bool
  -> ToBriDocM BriDocNumbered
~~~~

~~~~.hs
processDefault
  :: ( ExactPrint.Annotate.Annotate ast
     , MonadMultiWriter Text.Builder.Builder m
     , MonadMultiReader ExactPrint.Types.Anns m
     )
  => Located ast
  -> m ()
~~~~

~~~~.hs
linewise
  :: forall n t
   . (Ord n, R.ReflexHost t, MonadIO (R.PushM t), MonadIO (R.HostFrame t))
  => (  R.Event t Text -- command string executed by user
     -> R.Dynamic t (Maybe Text, Int, Text)
     -> R.Behavior t (Seq Text) -- history
     -> R.Event t ()   -- post-shutdown
     -> RH.AppHost
          t
          ( R.Event t () -- shutdown trigger
          , R.Behavior t String -- tab-completion value
          , R.Dynamic t (Widget n)
          )
     )
  -> RH.AppHost t ()
~~~~

linewise ::
     forall n t.
     (Ord n, R.ReflexHost t, MonadIO (R.PushM t), MonadIO (R.HostFrame t))
  => (R.Event t Text -- command string executed by user
       -> R.Dynamic t (Maybe Text, Int, Text) -> R.Behavior t (Seq Text) -- history
                                                  -> R.Event t () -- post-shutdown
                                                      -> RH.AppHost t ( R.Event t () -- shutdown trigger
                                                                      , R.Behavior t String -- tab-completion value
                                                                      , R.Dynamic t (Widget n)))
  -> RH.AppHost t ()


processDefault ::
     ( ExactPrint.Annotate.Annotate ast
     , MonadMultiWriter Text.Builder.Builder m
     , MonadMultiReader ExactPrint.Types.Anns m
     )
  => Located ast
  -> m ()
