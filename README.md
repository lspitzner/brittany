# brittany [![Hackage version](https://img.shields.io/hackage/v/brittany.svg?label=Hackage)](https://hackage.haskell.org/package/brittany) [![Stackage version](https://www.stackage.org/package/brittany/badge/lts?label=Stackage)](https://www.stackage.org/package/brittany) [![Build Status](https://secure.travis-ci.org/lspitzner/brittany.svg?branch=master)](http://travis-ci.org/lspitzner/brittany) 
haskell source code formatter

![Output sample](https://github.com/lspitzner/brittany/raw/master/brittany-sample.gif)

(see [more examples and comparisons](/doc/showcases))

This project's goals roughly are to:

- Always retain the semantics of the source being transformed;
- Be idempotent (this also directly ensures that only valid haskell is
  produced);
- Support the full GHC-haskell syntax including syntactic extensions
  (but excluding `-XCPP` which is too hard);
- Retain newlines and comments unmodified;
- Be clever about using the available horizontal space while not overflowing
  it if it cannot be avoided;
- Be clever about aligning things horizontally (this can be turned off
  completely however);
- Have linear complexity in the size of the input.

In theory, the core algorithm inside brittany reaches these goals. It is rather
clever about making use of horizontal space while still being linear in the
size of the input (although the constant factor is not small). See
[these examples of clever layouting](/doc/showcases/Layout_Interactions.md).

But brittany is not finished yet, and there are some open issues that yet
require fixing:

- **only type-signatures and function/value bindings** are processed;
  other module elements (data-decls, classes, instances, imports/exports etc.)
  are not transformed in any way; this extends to e.g. **bindings inside class
  instance definitions** - they **won't be touched** (yet).
- By using `ghc-exactprint` as the parser, brittany supports full GHC 
  including extensions, but **some of the less common syntactic elements
  (even of 2010 haskell) are not handled**.
- **There are some known issues regarding handling of in-source comments.**
  There are cases where comments are not copied to the output (this will
  be detected and the user will get an error); there are other cases where
  comments are moved slightly; there are also cases where comments result in
  wonky newline insertion (although this should be a purely aesthetic issue.)
- There is an **open performance issue on large inputs** (due to an
  accidentally quadratic sub-algorithm); noticable for inputs with >1k loc.

# Other usage notes

- Requires `GHC-8.0.*`; support for 8.2 is on the list, but I haven't even
  looked at how much the `GHC` API changes.
- config (file) documentation is lacking.
- some config values can not be configured via commandline yet.
- uses/creates user config file in `~/.brittany/config.yaml`;
  also reads `brittany.yaml` in current dir if present.

# Installation

- via `cabal` "old-build"

    ~~~~.sh
    # optionally:
    # mkdir brittany
    # cd brittany
    # cabal sandbox init
    cabal install brittany --bindir=$HOME/.cabal/bin # -w $PATH_TO_GHC_8_0
    ~~~~

- via `cabal new-build`

    ~~~~.sh
    cabal unpack brittany
    cd brittany-0.8.0.2
    # cabal new-configure -w $PATH_TO_GHC_8_0
    cabal new-build exe:brittany
    # and it should be safe to just copy the executable, e.g.
    cp `./find dist-newstyle/build/ -type f -name brittany` $HOME/.cabal/bin/
    ~~~~

- via `stack`

    ~~~~.sh
    git clone https://github.com/lspitzner/brittany.git
    cd brittany
    stack install
    ~~~~


# Usage

- Currently one mode of operation: Transform a single module. By default read
  from `stdin` and written to `stdout`, but commandline arguments allow to
  read/write from/to files.
- For stdin/stdout usage it makes sense to enable certain syntactic extensions
  by default, i.e. to add something like this to your
  `~/.brittany/config.yaml` (execute `brittany` once to create default):

    ~~~~
    conf_forward:
      options_ghc:
      - -XLambdaCase
      - -XMultiWayIf
      - -XGADTs
      - -XPatternGuards
      - -XViewPatterns
      - -XRecursiveDo
      - -XTupleSections
      - -XExplicitForAll
      - -XImplicitParams
      - -XQuasiQuotes
      - -XTemplateHaskell
      - -XBangPatterns
    ~~~~

# Implementation/High-level Documentation

[See the documentation index](doc/implementation/index.md)

# License

Copyright (C) 2016-2017 Lennart Spitzner

This program is free software: you can redistribute it and/or modify
it under the terms of the
[GNU Affero General Public License, version 3](http://www.gnu.org/licenses/agpl-3.0.html),
as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
