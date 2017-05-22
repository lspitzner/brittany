# Revision history for brittany

## 0.8.0.1  -- May 2017

* Document the high-level design of the program
* Improve layouting for many different cases, too many to list here. Brittany
  still does only reformat top-level type signatures and bindings.
* Publish all dependencies on hackage; `ghc-exactprint ` adaptions got merged
  upstream as well.
* Reduce the aggressiveness of horizontal alignment; this is configurable
  via the `lconfig_alignmentLimit` and `lconfig_alignmentBreakOnMultiline`
  values (config file only for now).
* (!) Breaking change to the config file format: The keys previously contained
  underscore (e.g. `_econf_Werror`) but do not anymore (`econf_Werror`).
  Add config version; also
* Move config value `conf_errorHandling.econf_CPPMode` to
  `conf_preprocessor.ppconf_CPPMode`.
* Cope with unhandled syntactical constructs more gracefully by falling back
  on the ghc-exactprint output; Brittany simply won't touch certain
  subexpressions instead of aborting. This is further configurable via the
  `econf_omit_output_valid_check` config value.
* Due to improvements to the `butcher` library: Accept `--key=value` on
  commandline (only `--key value` was supported previously).
* Improve testsuite setup: The `tests.blt` file contains many different
  testcases that allow quick addition - feel free to report bugs directly
  by making PRs with additions to that file.

## 0.7.1.0  -- 2016-09-06

* Support stack
* Fix --ghc-options handling
* Add commandline param to allow shortcut `brittany Foo.hs`
  meaning `brittany -i Foo.hs`

## 0.7.0.0  -- 2016-09-04

* First official alpha release
* Fix commandline flag parsing
* Implement/Improve horizontal aligning
* Various minor fixes and layouting improvements

## 0.6.0.0  -- 2016-08-12

* Add check that output is syntactically valid
* (!) Change config format, breaking previous configs
* Various layouting additions/imporements
* Various minor fixes

## 0.5.0.0  -- 2016-08-08

* Support --ghc-options
* Support user and local config files: Local config file is not mandatory
  anymore.

## 0.4.0.0  -- 2016-08-06

* Make comment handling a bit more clever
* Various layouting additions/imporements

## 0.3.0.0  -- 2016-08-04

* Various layouting additions/imporements

## 0.2.0.0  -- 2016-07-30

* Basic and partial implementation for
  expression/decl/statement/pattern layouting
* Prevent exponential behaviour using manual stablenames

## 0.1.0.0  -- 2016-06-05

* First working code for type signature layouting
