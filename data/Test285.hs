func = BuildReport
 where
  convertInstallOutcome = case result of
    Left  BR.PlanningFailed      -> PlanningFailed
    Left  (BR.DependentFailed p) -> DependencyFailed p
    Left  (BR.DownloadFailed  _) -> DownloadFailed
    Left  (BR.UnpackFailed    _) -> UnpackFailed
    Left  (BR.ConfigureFailed _) -> ConfigureFailed
    Left  (BR.BuildFailed     _) -> BuildFailed
    Left  (BR.TestsFailed     _) -> TestsFailed
    Left  (BR.InstallFailed   _) -> InstallFailed
    Right (BR.BuildOk _ _ _    ) -> InstallOk
