# Dataflow

From the program design standpoint, Brittany performes a
`Config -> Text -> Text` transformation; it is not interactive in any way and
it processes the whole input at once (no streaming going on). This makes for
a very simple design with nice separation of IO and non-IO.

Brittany makes heavy usage of mtl-on-steroids-style transformers, mostly
limited to Reader, Writer and State. For this kind of task it makes a lot of
sense; we do a pure transformation involving multiple steps
that each requires certain local state during traversals of recursive data
structures. By using MultiRWS we can even entirely avoid using lens without
inducing too much boilerplate.

Firstly, the topmost layer, the IO bits:

<img src="https://cdn.rawgit.com/lspitzner/brittany/7775812cfdc7d2596883f87b5ba9207fbf61f2b3/doc-svg-gen/generated/periphery.svg">

The corresponding code is in these modules:

- `Main`
- `Language.Haskell.Brittany`

The latter [contains the code to run our Reader/Writer/State stack](https://github.com/lspitzner/brittany/blob/7775812cfdc7d2596883f87b5ba9207fbf61f2b3/src/Language/Haskell/Brittany.hs#L64-L75) (well, no state yet).

Note that `MultiRWS` here behaves like a nicer version of a stack like
`ReaderT x (ReaderT y (WriterT w1 (WriterT2 w2 (Writer w3)..)`.
The next graph zooms in on that transformation:

<img src="https://cdn.rawgit.com/lspitzner/brittany/7775812cfdc7d2596883f87b5ba9207fbf61f2b3/doc-svg-gen/generated/ppm.svg">

Two places (The `BriDoc` generation and the backend) have additional local
state (added to the monadic context).
The following is a very simplified description of the BriDoc generation:

<img src="https://cdn.rawgit.com/lspitzner/brittany/7775812cfdc7d2596883f87b5ba9207fbf61f2b3/doc-svg-gen/generated/bridocgen.svg">


For the `BriDoc` generation, the relevant modules are
- `Language.Haskell.Brittany.Layouters.*`
- `Language.Haskell.Brittany.LayouterBasics`

For the `BriDoc` tree transformations, the relevant modules are
- `Language.Haskell.Brittany.Transformations.*`

Finally, for the backend, the relevant modules are
- `Language.Haskell.Brittany.Backend`
- `Language.Haskell.Brittany.BackendUtils`

