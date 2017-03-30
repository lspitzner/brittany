# The BriDoc type and the to-BriDoc transformation

The `BriDoc` type is the brittany equivalent of the `Doc` type from
general-purpose formatting libraries such as the `pretty` package.
It is specialized for this usecase: Representing a formatted
haskell source code document. As a consequence, it is a good amount
more complex than the `Doc` type (which has 8, not directly exposed,
constructors): The `BriDoc` type has ~25 constructors.
(26, but one for debugging, two deprecated and so on.)
Examples are `BDEmpty`, `BDSeq [BriDoc]` (inline sequence),
and `BDAddBaseY BrIndent BriDoc` (add a certain type of indentation
to the inner doc).

The main bulk of code that makes brittany work is the translation
of different syntactical constructs into a raw `BriDoc` value.
(technically a `BriDocF` value, we'll explain soon.)

The input of this translation is the syntax tree produced by
GHC/ExactPrint. The GHC API exposes the syntax tree nodes, and
ExactPrint adds certain annotations (e.g. information about
in-source comments). The main thing that you will be looking
at here is the GHC API documentation, for example
https://downloads.haskell.org/~ghc/8.0.2/docs/html/libraries/ghc-8.0.2/HsDecls.html

Brittany has several flags for dumping intermediate values of the
transformation process; relevant for the "input" syntax tree are the flags
`--dump-ast-unknown` and `--dump-ast-full`, where the latter will print the
whole ast of the input to stderr, the former will only do so for nodes where
brittany falls back on the ghc-exactprint output (i.e. in those cases where
we don't transform, but do a mere copy).
See [this example ast output](output-example-01.md)
(yeah, raw ASTs are ~~annoying large~~ fun!)

## Two examples of the process producing raw BriDoc

1. For example, `Brittany.hs` contains the following code (shortened a bit):

  ~~~~.hs
  ppDecl d@(L loc decl) = case decl of
    SigD sig  -> [..] $ do
      briDoc <- briDocMToPPM $ layoutSig (L loc sig)
      layoutBriDoc d briDoc
    ValD bind -> [..] $ do
      briDoc <- [..] layoutBind (L loc bind)
      layoutBriDoc d briDoc
    _         -> briDocMToPPM (briDocByExactNoComment d) >>= layoutBriDoc d
  ~~~~

  which matches on the type of module top-level syntax node and
  dispatches to `layoutSig`/`layoutBind` to layout type signatures
  and equations. For all other constructs, it currently falls back to using
  ExactPrint to reproduce the exact original.

2. Lets look at a "lower" level fragment that actually produces BriDoc (from Type.hs):

  ~~~~.hs
    -- if our type is an application; think "HsAppTy Maybe Int"
    HsAppTy typ1 typ2 -> do
      typeDoc1 <- docSharedWrapper layoutType typ1 -- layout `Maybe`
      typeDoc2 <- docSharedWrapper layoutType typ2 -- layout `Int`
      docAlt                                       -- produce two possible layouts
        [ docSeq                                       -- a singular-line sequence, with a space in between
          [ docForceSingleline typeDoc1                -- "Maybe Int"
          , docLit $ Text.pack " "
          , docForceSingleline typeDoc2
          ]
        , docPar                                       -- an multi-line result, with the "child" indented.
            typeDoc1                                   -- "Maybe\
            (docEnsureIndent BrIndentRegular typeDoc2) --    Int"
        ]
  ~~~~

  here, all functions prefixed with "doc" produces new BriDoc(F) nodes.
  I think this example can be understood already, even when many details
  (what is `docSharedWrapper`?
  What are the exact semantics of the different `doc..` functions?
  Why do we need to wrap the `BriDoc` constructors behind those smart-constructor thingies?)
  are not explained yet.

  In [this example output](output-example-02.md) the BriDoc tree produced in
  this fashion is shown for the trivial input `x :: Maybe Int`. Can you spot
  the `BDAlt` node that matches the above `docAlt` invocation? (hint: the
  node is used twice, so we can see two identical `BDAlt` nodes.)
  This leads directly to:
  
## Size of BriDoc trees, Sharing and Complexity

In order to explain the `BriDocF` type and the reasoning behind smart
constructors, we need to consider the size of the `BriDoc` tree produced by
this whole process.
As seen above, we can have multiple alternative layouts (`docAlt`) for
the same node.
This means the number of nodes in the `BriDoc` value we produces in general is
exponential in the number for syntax nodes of the input.

But we are aiming for linear run-time, right? So what can save us here?
You might think: We have sharing! For `let x = 3+3; (x, x)` we only have one
`x` in memory ever. And indeed, we do the same above: `typeDoc1` and `2` are
used in exactly that manner: Both are referenced once in each of the two
alternatives.

Unfortunately this does not mean that we can forget this issue entirely.
The problem is that the BriDoc tree value will get transformed by multiple
transformations. And this "breaks" sharing: If we take an exponential-sized
tree that is linear-via-sharing and `fmap` some function `f` on it (think of
some general-purpose tree that is Functor) then `f` will be evaluated an
exponential number of times. And worse, the output will have lost any sharing.
Sharing is not automatic memoization.
And this holds for BriDoc, even when the transformations are not exactly
`fmap`s.

So.. we already mentioned "memoization" there, right?

1. The bad news:
   Any existing memoization utilities/approaches didn't work for one reason
   or another. (I suspect that there is a bug in the GHC StableName
   implementation, or I messed up..) After trying several memoization
   approaches and wasting tons of time, I went with a manual approach,
   and it worked more or less instantly. So that is where we are at.

   Manual memoization means that we manually tag every node of the `BriDoc`
   with a unique `Int`. This is rather annoying at places, but then again
   we can abstract over that pretty well.
   
2. The good news:
   With manual memoization, creating an exponentially-sized tree is no
   problem, presuming that it is linear-via-sharing. Not messing up this
   property can take a bit of consideration - but otherwise we are set.
   If the `BriDocF` tree is exponential, the transformations will still
   do only linear-amount of "selection work" in order to convert into a
   linear-sized `BriDoc` tree.

   This property is the defining one that motivates the BriDoc
   intermediate representation.

Lets have a look at this selection work! We saw at
[the above example](output-example-02.md) how `x :: Maybe Int` had a
non-trivial raw `BriDoc` representation, already with two nested `BDAlt`
nodes and resulting four alternatives. Removing those nodes is the first
step of the `BriDoc` transformation, and we can
[observe the output after removing those nodes](output-example-03.md).
Quite a bit shorter, the tree-printing-algorithm even thinks that it fits
in a single line now.

We will not go into detail about how this "alt-transformation" (the one doing
the "selection work" works and what other transformations follow here.
For this example not much happens; you can see so in the output which you
probably already noticed in the last example.

But for the "alt-transformation" itself, lets at least consider what it does:
We traverse the input BriDoc and whenever a `BDAlt` is encountered, one of the
alternatives is chosen; the other alternatives and the `BDAlt` node itself are
discarded.
The choice is made in such a fashion that, well, the final output does not
contain lines with more than 80 columns but otherwise relatively few newlines.
Magic! (for now at least.)

## BriDocF

The `BriDocF f` type encapsulates the idea that each subnode is wrapped
in the `f` container. This notion gives us the following nice properties:

`BriDocF Identity ~ BriDoc` and `BriDocF ((,) Int)` is the
manual-memoization tree with labeled nodes. Abstractions, abstractions..

Lets have a glance at related code/types we have so far:

~~~~.hs
-- The pure BriDoc: What we really want, but cannot use everywhere due
-- to sharing issues.
-- Isomorphic to `BriDocF Identity`. We still use this type, because
-- then we have to unwrap the `Identities` only in once place after reducing
-- the tree to a non-exponentially-sized one.
data BriDoc
  = BDEmpty
  | BDLit !Text
  | BDSeq [BriDoc]
  | BDAddBaseY BrIndent BriDoc
  | BDAlt [BriDoc]
  .. [a good amount more]

data BriDocF f
  = BDFEmpty
  | BDFLit !Text
  | BDFSeq [f (BriDocF f)]
  | BDFAddBaseY BrIndent (f (BriDocF f))
  | BDFAlt [f (BriDocF f)]
  .. [a good amount more]

type BriDocFInt = BriDocF ((,) Int)
type BriDocNumbered = (Int, BriDocFInt)

-- drop the labels
unwrapBriDocNumbered :: BriDocNumbered -> BriDoc
unwrapBriDocNumbered = ..
~~~~

And, because we will need it below: The monadic context that the creation
of the BriDocF tree uses:

~~~~.hs
-- If you are not familiar with the `multistate`
-- package and RWS, this is somewhat similar to:
-- ReaderT Config (ReaderT Anns (WriterT [LayoutError] (WriterT (Seq String) (State NodeAllocIndex))))
-- i.e. it is basically an environment allowing:
--   a) read access to global program config `Config` and the exactprint
--      annotations `Anns` of given input;
--   b) write access of errors and "good" output;
--   c) a local/"State" "variable" `NodeAllocIndex`
--      (yep, for the manual memoization node labels).
type ToBriDocM = MultiRWSS.MultiRWS '[Config, Anns] '[[LayoutError], Seq String] '[NodeAllocIndex]
~~~~

We don't use this directly, but the code below uses this,
and if the type `ToBriDocM` scared you, see how mundane it
is used here (`m` will be `ToBriDocM` mostly):

~~~~
allocNodeIndex :: MonadMultiState NodeAllocIndex m => m Int
allocNodeIndex = do
  NodeAllocIndex i <- mGet
  mSet $ NodeAllocIndex (i + 1)
  return i
~~~~
 
## The `doc..` smart constructors

In most cases the smart constructors are fairly dumb: Their main purpose
is to allocate the unique label for the current node, and return it
together with the node itself. Lets look at two examples to get a
feeling for the types involved:

~~~~.hs
docEmpty :: ToBriDocM BriDocNumbered
docEmpty = allocateNode BDFEmpty -- what a "smart" constructor, right?

docSeq :: [ToBriDocM BriDocNumbered] -> ToBriDocM BriDocNumbered
docSeq l = allocateNode . BDFSeq =<< sequence l
-- this is a bit more elaborate: In order to allow proper
-- composition of these smart constructors, we accept a list of
-- actions instead of just `BriDocNumbered`s, and use `sequence`
-- to make it work. Nothing unusual otherwise.
~~~~

There is one rather special `doc..` function: `docSharedWrapper`.
Lets consider the code first:

~~~~.hs
docSharedWrapper :: Monad m => (x -> m y) -> x -> m (m y)
docSharedWrapper f x = return <$> f x
~~~~

How is this useful? Consider this: All the smart constructors
expect as input actions returning (freshly labeled) nodes.
But what if we want sharing? In those cases we do _not_ want
fresh labels on multiple uses. Here `docSharedWrapper` comes
into play: It executes the contained label-allocation once
and returns a pure action via `return`; this pure action
can then be passed e.g. to docSeq but does not do any new
allocation. This gives us sharing in the cases where we
want it.

But wait, one more thing: Not all `BriDoc` constructors have
an exactly matching smart constructor, and there are smart
constructors that involve multiple BriDoc constructors behind
the scenes. For this reason, we will focus on the smart
constructors in the following, because they define the
real interface to be used.

You now might have a glance at [bridoc-api.md](bridoc-api.md).
