# brittany
haskell source code formatter

![Output sample](https://github.com/lspitzner/brittany/raw/master/brittany-sample.gif)

(see another demonstration in [Showcase.md](Showcase.md))

This project's goals roughly are to:

- Be idempotent (this also directly ensures that only valid haskell is
  produced);
- Support the full ghc-haskell syntax including syntactic extensions;
- Retain newlines and comments unmodified;
- Be clever about using the available horizontal space while not overflowing
  it if it cannot be avoided;
- Be clever about aligning things horizontally (this can be turned off
  completely however);
- Have linear complexity in the size of the input.

At this point, these goals are completed to different degrees.
Currently, only type-signatures and function/value bindings are processed;
other module elements (data-decls, classes, instances, imports/exports etc.) are not
transformed in any way; this extends to e.g. bindings inside class instance
definitions - they won't be touched (yet).
By using ghc-exactprint as the parser, brittany supports full ghc syntax including
extensions, but many of the less common syntactic elements (even of 2010 haskell) are
not handled.
There are cases where comments are not re-inserted properly (leading to an error or
to affected comments being placed at a different location than in the input).

The current algorithm is rather clever about horizontal space while still being
linear in the size of the input (although the constant factor is not small).

# Important notes

- Requires `ghc`>=8
- `-XCPP` is not officially supported (yet).
- ~~Some commandline flags mentioned in the help don't work yet (and won't even
  be parsed correctly.)~~ (fixed in 0.4.0.0)
- some config values can not be configured via commandline yet.
- ~~Creates a `brittany.yaml` config file _in the current directory_.~~
  (fixed in 0.5.0.0; now creates user config file in `~/.brittany`;
  still reads `brittany.yaml` in current dir if present.)
- ~~Currently some unhandled syntactical constructs don't raise errors~~
  (fixed in 0.6.0.0)
- There are cases where comments are not copied to the output (this will
  be detected and the user will get an error); there are other cases where
  comments are moved slightly; there are also cases where comments result in
  wonky newline insertion (although this should be a purely aesthetic issue.)

# Installation

- via `cabal` "old-build"

  ~~~~.sh
  git clone https://github.com/lspitzner/brittany.git
  cd brittany
  cabal sandbox init
  cabal install --bindir=$HOME/.cabal/bin # -w $PATH_TO_GHC_8
  ~~~~

- via `cabal new-build`

  ~~~~.sh
  git clone https://github.com/lspitzner/brittany.git
  cd brittany
  # cabal new-configure -w $PATH_TO_GHC_8
  cabal new-build brittany
  # and it should be safe to just copy the executable, e.g.
  cp ./dist-newstyle/build/brittany-0.8.0.0/build/brittany/brittany $HOME/.cabal/bin/
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
