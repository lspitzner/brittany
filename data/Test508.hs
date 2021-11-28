-- brittany { lconfig_columnAlignMode: { tag: ColumnAlignModeDisabled }, lconfig_indentPolicy: IndentPolicyLeft }
func = do
  let
    (primaryPkg, otherPkgs) = selectPrimaryLocalPackage pwd pkgs'
    (bproblems, x) = resolveBuildTargets primaryPkg otherPkgs utargets''
    -- default local dir target if there's no given target
    utargets'' = "foo"
  return ()
