downloadRepoPackage = case repo of
  RepoLocal {..}    -> return ()
  RepoLocal { abc } -> return ()
  RepoLocal{}       -> return ()
