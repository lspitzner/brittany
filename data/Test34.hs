{-# LANGUAGE RankNTypes #-}
addFlagStringParam
  :: forall f out
   . (Applicative f)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag String -- ^ properties
  -> CmdParser f out String
