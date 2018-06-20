# brittany [![Hackage version](https://img.shields.io/hackage/v/brittany.svg?label=Hackage)](https://hackage.haskell.org/package/brittany) [![Stackage version](https://www.stackage.org/package/brittany/badge/lts?label=Stackage)](https://www.stackage.org/package/brittany) [![Build Status](https://secure.travis-ci.org/lspitzner/brittany.svg?branch=master)](http://travis-ci.org/lspitzner/brittany) 
haskell source code formatter

![Output sample](https://github.com/lspitzner/brittany/raw/master/brittany-sample.gif)

(see [more examples and comparisons](/doc/showcases))

This project's goals roughly are to:

- Always retain the semantics of the source being transformed;
- Be idempotent;
- Support the full GHC-haskell syntax including syntactic extensions
  (but excluding `-XCPP` which is too hard);
- Retain newlines and comments unmodified;
- Be clever about using the available horizontal space while not overflowing
  the column maximum if it cannot be avoided;
- Be clever about aligning things horizontally (this can be turned off
  completely however);
- Have linear complexity in the size of the input.

In theory, the core algorithm inside brittany reaches these goals. It is rather
clever about making use of horizontal space while still being linear in the
size of the input (although the constant factor is not small). See
[these examples of clever layouting](/doc/showcases/Layout_Interactions.md).

But brittany is not finished yet, and there are some open issues that yet
require fixing:

- **only the module header (imports/exports), type-signatures and
  function/value bindings** are processed;
  other module elements (data-decls, classes, instances, etc.)
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

## Try without Installing

You can [paste haskell code over here](https://hexagoxel.de/brittany/)
to test how it gets formatted by brittany. (Rg. privacy: the server does
log the size of the input, but _not_ the full input/output of requests.)

# Other usage notes

- Supports GHC versions `8.0`, `8.2` and `8.4`.
- included in stackage with lts>=10.0 (or nightlies dating to >=2017-11-15)
- config (file) documentation is lacking.
- some config values can not be configured via commandline yet.
- uses/creates user config file in `~/.config/brittany/config.yaml`;
  also reads (the first) `brittany.yaml` found in current or parent
  directories.

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
    cd brittany-0.11.0.0
    # cabal new-configure -w $PATH_TO_GHC_8_0
    cabal new-build exe:brittany
    # and it should be safe to just copy the executable, e.g.
    cp `find dist-newstyle/ -name brittany -type f | xargs -x ls -t | head -n1` $HOME/.cabal/bin/
    ~~~~

- via `stack` using a sufficiently recent stackage snapshot (dated to >= 2017-11-15)

    ~~~~.sh
    stack install brittany # --resolver lts-10.0
    ~~~~

    (earlier ltss did not include `brittany` yet, but the repo should contain a
    `stack.yaml` that works with ghc-8.0.)

- on ArchLinux via [the britanny AUR package](https://aur.archlinux.org/packages/brittany/)
  using `aura`:
    ~~~~.sh
    aura -A brittany
    ~~~~

# Editor Integration

#### Sublime text
  [In this gist](https://gist.github.com/lspitzner/097c33177248a65e7657f0c6d0d12075)
  I have described a haskell setup that includes a shortcut to run brittany formatting.
#### VSCode
  [This extension](https://marketplace.visualstudio.com/items?itemName=MaxGabriel.brittany)
  connects commandline `brittany` to VSCode formatting API. Thanks to @MaxGabriel.
#### Via HIE
  [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)
  includes a `brittany` plugin that directly uses the brittany library.
  Relevant for any editors that properly support the language-server-protocol.
#### Neovim / Vim 8
  The [Neoformat](https://github.com/sbdchd/neoformat) plugin comes with support for
  brittany built in.

# Usage

- Default mode of operation: Transform a single module, from `stdin` to `stdout`.
  Can pass one or multiple files as input, and there is a flag to override them
  in place instead of using `stdout` (since 0.9.0.0). So:
  
    ~~~~ .sh
    brittany                           # stdin -> stdout
    brittany mysource.hs               # ./mysource.hs -> stdout
    brittany --write-mode=inplace *.hs # apply formatting to all ./*.hs inplace
    ~~~~
    
- For stdin/stdout usage it makes sense to enable certain syntactic extensions
  by default, i.e. to add something like this to your
  `~/.config/brittany/config.yaml` (execute `brittany` once to create default):

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

# Feature Requests, Contribution, Documentation

This currently is a one-person project in the sense that 90% of the code is
written by one person. And (unfortunately) it is not my job to keep improving
this project. Please forgive that as a consequence my time to invest on new
features is rather limited.

Nonetheless I consider it "in active development" :)

One way of speeding things up is to make your own contributions. There is
a good amount of high-level documentation at

[the documentation index](doc/implementation/index.md)

# License

Copyright (C) 2016-2018 Lennart Spitzner

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
