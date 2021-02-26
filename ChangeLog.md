# Revision history for brittany

## 0.13.1.1 -- February 2021

* #333: Allowed random 1.2. Thanks @felixonmars!
* #334: Updated Arch install instructions. Thanks @ahstro!
* #343: Allowed ghc-exactprint 0.6.4. Thanks @maralorn!

## 0.13.1.0 -- December 2020

* #330: Started sorting imports. Thanks @expipiplus1!

## 0.13.0.0 -- December 2020

* #324: Added support for GHC 8.10.
  * Dropped support for GHC 8.4, 8.2, and 8.0.
  * Thanks @jneira, @bubba, @infinity0, and @expipiplus1!

## 0.12.2.0 -- November 2020

* #207: Fix newtype indent in associated type family.
* #231: Improve comments-affecting-layout behaviour for tuples.
* #259: Data declaration for newtype and records. Thanks @eborden!
* #263: Fix non-idempotent newlines with comment + where.
* #273: Error handling.
* #281: Fix moving comment in export list (haddock header).
* #286: Fix comments in instance/type instances.
* #287: Add support for pattern synonyms. Thanks @RaoulHC!
* #293: Expose main function as a module. Thanks @soareschen!
* #303: Readme: Supports 8.8. Thanks @andys8!
* #311: Allows aeson-1.5.*. Thanks @jkachmar!
* #313: Nondecreasing export list formatting. Thanks @expipiplus1!

## 0.12.1.1 -- December 2019

* Bugfixes:
    - Fix layouting regression of record update for many/large fields
    - Fix whitespace regression on ExplicitForall notation
      (`foo :: forall  a . Show a => a -> a`, note the double space)
      introduced in 0.12. (#264)
    - Fix roundtripping of type equality constraint
      `f :: ((~) a b) => a -> b` (#267)
* One experimental feature addition: Turning brace notation semicolons into
  newlines when formatting (see #270)

## 0.12.1.0 -- September 2019

* Support ghc-8.8
* Support for OverloadedLabels extension
  (thanks to Evan Rutledge Borden @eborden)
* Support for Implicit Params extension (thanks to pepe iborra @pepeiborra)
* Add flag `--no-user-config` to enable only using manually passed config
* Disable the performance test suite by default to prevent spurious failures
  on certain CI setups. The github/travis brittany CI still has all tests
  enabled. See the `brittany-test-perf` flag in the cabal file.
* Bugfixes:
    - Fix one wandering-comment bug for let-in expressions
    - Fix invalid result for prefix operator pattern matches
    - Fix lambda expression with laziness/strictness annotation
    - Fix parenthesis handling for infix pattern matches with 3+ arguments
* Changes to layouting behaviour:
    - For pattern matching and data/instance definitions, the usage of
      parenthesis is now "normalized", i.e. superfluous parens are removed by
      brittany.

## 0.12.0.0 -- June 2019

* Support for ghc-8.6 (basic support, not necessarily all new syntactic
  extensions)
* Support -XExplicitNamespaces and -XPatternSynonyms
* Allow a --dry-run sort of operation via flag "-c/--check-mode"
  (thanks to Doug Beardsley @mightybyte)
* Include file name in errors about unsupported syntax nodes (thanks to @5outh)
* Partially implement layouting class instances: Layouts children, but
  falls back on ghc-exactprint for the instance head
  (thanks to Rupert Horlick @ruhatch)
* Implement layouting for type synonyms (thanks to Rupert Horlick @ruhatch)
* Support -XMagicHash, -XUnboxedTuples (thanks to Sergey Vinokurov @sergv)
* Support -XQuasiQuotes (no formatting applied to the splices; they are simply
  retained without causing the dreaded "Unknown construct: HsSpliceE{}")
    - `lconfig_allowHangingQuasiQuotes` controls whether multi-line
      QuasiQuotes are allowed to start at the end of the current line, or
      whether they are always placed in new lines.
* Bugfixes:
    - Fix rare-case alignment bug with IndentPolicyMultiple (#144)
    - Make inline layout config apply to module header (#151)
    - Fix unaligned import-hiding layout (#150)
    - Fix idempotence violation for comments around if-then-else (#167)
    - Fix comments having an effect on far-away parent node's layout (#159)
    - Fix imports of type operators ("symbolic data types")
      (thanks to Phil Hazelden @ChickenProp)
    - Work around GHC and cabal-install misfeature ".ghc.environment files"
      that could break brittany in unexpected and hard-to-understand ways
    - Stop removing empty lines before `where` keyword in a couple of cases
    - Fix functions with mixing prefix/infix style causing error (#234)
* Changes to layout:
    - Align usage of spaces for record update vs record construction (#126)
    - More indentation to import-hiding-paragraph (follow-up to #150 fix)
    - Record construction and update now are layouted in the same way
      (thanks to Evan Rutledge Borden @eborden)
    - Stop allowing single-line layout when there are comments between
      arguments (#214) (thanks to @matt-noonan)
* Various build-instructions and editor integrations

## 0.11.0.0 -- May 2018

* Support for ghc-8.4
* Implement inline-config
    e.g. "-- brittany --indent=4"

    respects the following comment forms as input:

    ~~~~
    source comment                       affected target
    ======================================================
    "-- brittany CONFIG"                 whole module
    "-- brittany-next-binding CONFIG"    next binding
    "-- brittany-disable-next-binding"   next binding
    "-- brittany @ myExampleFunc CONFIG" `myExampleFunc`
    ~~~~

    multiline-comments are supported too, although
    the specification must still be a single line. E.g.

    > "{- brittany --columns 50 -}"

    CONFIG is either:

    1) one or more flags in the form of what brittany accepts
       on the commandline, e.g. "--columns 50", or
    2) one or more specifications in the form of what brittany
       accepts in its config files for the layouting config
       (a one-line yaml document), e.g. "{ lconfig_cols: 50 }"
* Implement `IndentPolicyMultiple` (thanks to Bryan Richter @chreekat)
    Restrict indentation amounts to `n * indentAmount`
* Implement `--obfuscate` that replaces non-keyword identifiers with random
  names
* Do not write files unless there are changes (don't update modtime)
  (`--write-mode=inplace`) (#93)
* Bugfixes:
    - Fix empty function constraints (`() => IO ()`) (#133)
    - Fix overflowing columns caused by aligning with surrounding lines
      for certain complex cases
    - Implement hacky workaround for `type instance`s (`-XTypeFamilies`) (#89)
* Layouting changes:
    - On default settings, allow single-line module header
      `module MyModule where` when no exports
    - Fix one case of non-optimal layouting for if-then-else
    - Allow same-line let binding inside do-notation with
      `IndentPolicyLeft/Multiple` and `indentAmount>=4`

## 0.10.0.0 -- March 2018

* Implement module/exports/imports layouting (thanks to sniperrifle2004)
* Expose config paths/parsing functions (thanks to Alexey Raga)
* Bugfixes:
    - Fix layouting of `NOINLINE` pragma
    - Fix ticked type operator (e.g. `':-`) losing tick (#125)
    - Fix alignment issue with cases involving operators (#65)
    - Fix comments in tuples being dropped (#37)
    - Fix comment placements with let-in (#110)
* Layouting changes:
    - Align arguments only if it is the same function being called (#128)
    - Do not use single-line layout when infix operator expression contains
      comments (#111)
* New layouting config items:
    - `lconfig_importColumn`/`--import-col`: column for import items
    - `lconfig_importAsColumn`/`--import-as-col`: column for the "as" name of
      a module
    - `lconfig_reformatModulePreamble`: controls module/export/import layouting
      (default True)
    - `lconfig_allowSingleLineExportList`: permit one-line module header, e.g.
      `module Main (main)` (default False)

## 0.9.0.1  -- February 2018

* Support `TupleSections` (thanks to Matthew Piziak)
* Bugfixes:
    - Fix Shebang handling with stdin input (#92)
    - Fix bug that effectively deleted strict/lazy matches (BangPatterns) (#116)
    - Fix infix operator whitespace bug (#101, #114)
    - Fix help command output and its layouting (#103)
    - Fix crash when config dir does not exist yet (#115)
* Layouting changes:
    - no space after opening non-tuple parenthesis even for multi-line case
    - use spaces around infix operators (applies to sections and in pattern
      matches)
    - Let-in is layouted more flexibly in fewer lines, if possible
      (thanks to Evan Borden)

## 0.9.0.0  -- December 2017

* Change default global config path (use XDG spec)
    Existing config should still be respected, so this should not break
    compatibility
* Support per-project config
* ! Slight rework of the commandline interface:
    - Support multiple inputs and outputs
    - Support inplace-transformation for multiple files via
      `--write-mode=inplace`
* Implement `IndentPolicyLeft` - the indentation mode that never adds more
  than the base indentation for nested parts (no hanging indentation)

    (thanks to Evan Borden)
* Fix bug that manifested in bad output for (top-level) template haskell splices
* Extension support:
    - RecordWildCards
    - RecursiveDo (was only partially supported previously)
* Layouting Bugfixes:
    - Properly reproduce parentheses around kind signatures
    - Fix issue around promoted lists
      (example good: `'[ 'True]` bad: `'['True]`)
    - Trim text from exactprint used as workaround for unknown nodes
      (unsupported extension workaround)
* Layouting changes
    - Insert spaces around operator in sections

## 0.8.0.3  -- September 2017

* Support for ghc-8.2.1
* Bugfixes:
    - Fix quadratic performance issue
    - Fix special "where" indentation with indentAmount /= 2
    - Fix negative literals in patterns
    - Support type applications
* Accept `-h` for `--help` and improve help layouting (via butcher-1.1.0.2)
* Add continuous integration via travis (cabal, cabal-new, stack)
  (brittle due compilation time limit)
* Reduce compilation memory usage a bit

## 0.8.0.2  -- August 2017

* Add library interface, to be used by `haskell-ide-engine`.
* Publish to hackage.

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
* Release under the terms of the AGPLv3

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
