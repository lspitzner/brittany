- [theory](theory.md)

  Explains the core idea of the formatter that makes it so cool.

- [dataflow](dataflow.md)

  Looking at how the data is tranformed should give the reader a good
  idea of the high-level design, given that Brittany essentially
  performs a `Text -> Text` transformation.

- [bridoc-design](bridoc-design.md)

  An explanation of the `BriDoc` datatype focussed on (potential) contributors
  that wish to add support for more syntactical constructs.

- [bridoc-api](bridoc-api.md)

  Specifying the semantics of the different (smart) constructors of the
  `BriDoc` type.

- Brittany uses the following (randomly deemed noteworthy) libraries:

  - [`ghc-exactprint`](https://hackage.haskell.org/package/ghc-exactprint)
    (and [`ghc`](https://hackage.haskell.org/package/ghc)) for parsing of haskell source;
  - [`uniplate`](https://hackage.haskell.org/package/uniplate)
    for efficient transformations on the recursive `BriDoc` datatype;
    this powers the main computational work done by Brittany;
  - [`monad-memo`](https://hackage.haskell.org/package/monad-memo)
    for explicit function memoization;
  - [`multistate`](https://hackage.haskell.org/package/multistate)
    as an alternative to an unwieldly transformer stack;
  - [`butcher`](https://github.com/lspitzner/butcher)
    for parsing commandline arguments (as an alternative to
    [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative))
  - [`yaml`](https://hackage.haskell.org/package/yaml)
    to handle config file;
  - [`safe`](https://hackage.haskell.org/package/safe)
    and
    [`unsafe`](https://hackage.haskell.org/package/unsafe)
    (heh).

