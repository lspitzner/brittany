# Exactprinting

Brittany uses the `ghc-exactprint` library/wrapper around the GHC API to
parse haskell source code into a syntax tree and into "annotations". The
unannotated syntax tree would lose information, such as the exact (relative)
position in the source text, or any comments; this is what annotations provide.

Following that name, we'll call "exactprinting" the aspect of reproducing
comments and relative positions - most importantly additional newlines - while
round-tripping through brittany. The focus is not on the API of the
`ghc-exactprint` library, but on the corresponding data-flow through brittany.

**Take note that the `--dump-bridoc-*` output filters out the constructors
responsible for comments and for applying DeltaPositions.**
This is done to keep the output more readable, but could confuse you if you
try to understand how comments work.

## TLDR - Practical Suggestions for Implementing Layouters

This advice does not explain how comments work, but if you are implementing
a layouter it might cover most cases without requiring you to understand the
details.

- Ideally, we should wrap the `BriDoc` of any construct that as a location
  (i.e. has the form `(L _ something)`) (and consequently has an `AnnKey`)
  using `docWrapNode`. As an example, look at the `layoutExpr` function and
  how it applies `docWrapNode lexpr $ ..` right at the top.

- If we have not done the above, it is somewhat likely that comments
  "get eaten". For such cases:

  1. Take a small reproduction case

  1. Throw it at `brittany --dump-ast-full` and see where the comment is
       in the syntax tree. See where the corresponding syntax node is
       consumed/transformed by brittany and wrap it with `docWrapNode`.

  1. If it is unclear what alternative (of a `docAlt` or
       `runFilteredAlternative`) applies, try inserting `docDebug "myLabel"`
       nodes to track down which alternative applies.

- For comments that _do_ appear in the output but at the wrong location, there
  are two classes of problems: Firstly we have comments that move "down" past
  other stuff (even switching order of comments is possible). Use the steps
  from the last item to figure out which syntax tree constructor is relevant,
  and try inserting `docMoveToKWDP` or replace `docWrapNode` with a manually
  refined combination of `docWrapNodePrior` and `docWrapNodeRest`.

- For comments that _do_ appear in the output in roughly the right position,
  only with the wrong indentation, the cause most likely is a
  mis-interpretation of DPs that can be fixed by inserting a
  `docSetIndentLevel` at the right position - right before printing the
  thing that provides the "layouting rule" indentation, i.e. the body of a
  `do`/`let`/`where` block.

- There is one other cause for off-by-one errors in comment position:
  Whitespace. In general, layouters should prefer to use `docSeparator` to
  insert space between syntax elements rather than including spaces in
  literal strings. As an example, use `docSeq [docLit "then", docSeparator]`
  or the equivalent `appSep (docLit "then")` rather than `docLit "then "`.
  The reason is that comment positions are relative to the last non-whitespace,
  and `docSeparator` is interpreted in just the right fashion: It inserts
  a whitespace, but keeps track of the correct comment offset. (Also,
  subsequent `docSeparators` are merged into one.)

- If all of this fails, read below, bother the maintainers and/or make use of
  the more advanced debugging features (there is a `#define` in
  `BackendUtils.hs` that you can turn on to insert all kinds of verbose
  output in-line with the actual output).

## A Small Example

~~~~.hs
main = do
  putStr "hello" -- very suspense
  putStrLn " world" --nice
~~~~

If you pass this to `brittany --dump-ast-full` you'll see .. a 100 line syntax
tree. Yeah, raw syntax tree are a bit unwieldly.

(btw I'd use `clipread | brittany --dump-ast-full` for that purpose, where
`clipread` boils down to `xclip -o -selection clipboard`. If you have not set
up that script on your system, you really should.)

To simplify this slightly, we will focus down on just the syntax tree of
the `do` block, which is the `HsDo` constructor.

~~~~
---- ast ----
A Just (Ann (DP (0,0)) [] [] [((AnnComment (Comment "--nice" stdin:3:21-26 Nothing)),DP (0,1)),((G AnnEofPos),DP (1,0))] Nothing Nothing)
  HsModule
    ..
    [ A Just (Ann (DP (0,0)) [] [] [] Nothing Nothing)
        ValD
          FunBind
            A Just (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
              Unqual {OccName: main}
            MG
              A Nothing
                [ A Just (Ann (DP (0,0)) [] [] [((G AnnEqual),DP (0,1))] Nothing Nothing)
                    Match
                      FunRhs
                        ..main..
                        Prefix
                        NoSrcStrict
                      []
                      GRHSs
                        [ A Just (Ann (DP (0,-1)) [] [] [] Nothing Nothing)
                            GRHS
                              []
                              A Just (Ann (DP (0,1)) [] [] [((G AnnDo),DP (0,0))] Nothing Nothing)
                                HsDo
                                  DoExpr
                                  A Nothing
                                    [ A Just (Ann (DP (1,2)) [] [] [] Nothing Nothing)
                                        BodyStmt
                                          A Just (Ann (DP (0,0)) [] [] [] Nothing Nothing)
                                            HsApp
                                              ..putStr..
                                              .."hello"..
                                          ..
                                          ..
                                    , A Just (Ann (DP (1,0)) [((Comment "-- very suspense" stdin:2:18-33 Nothing),DP (0,1))] [] [] Nothing Nothing)
                                        BodyStmt
                                          A Just (Ann (DP (0,0)) [] [] [] Nothing Nothing)
                                            HsApp
                                              ..putStrLn..
                                              .." world"..
                                          ..
                                          ..
                                    ]
                        ]
                        A (Nothing) (EmptyLocalBinds)
                ]
              FromSource
            WpHole
            []
    ]
    ..
~~~~

So this is a haskell module, `HsModule` containing a function bind `FunBind`
containing a match group, containing a Match, containing some right-hand-side
expression which in this case is just a do block `HsDo` which contains two
applications `HsApp` of a function `putStr(Ln)` plus some string literal.

There is no need to understand this, as long as you can roughly see how this
representation corresponds to the input source code.

For the purpose of exactprinting, what we need to look at are the annotations.
The `ghc-exactprint` library returns the syntax tree and annotations as two
different entities:
- [You can start looking at the module level](https://downloads.haskell.org/ghc/latest/docs/html/libraries/ghc-8.8.1/HsSyn.html#v:HsModule)
and work your way down to any syntactical construct from there;
- The [Annotation type and its `Ann` constructor](https://hackage.haskell.org/package/ghc-exactprint-0.6.2/docs/Language-Haskell-GHC-ExactPrint-Types.html#t:Annotation).

In the above `--dump-ast-full` output these two are mixed together using the
fake `A` constructor that is just a pair of a `Maybe Annotation` and of one
node in the syntax tree. It was produced by recursively printing the syntax
tree, and for each node `n` we print `A (getAnnotation n) n`. So let's focus
on the `Annotation` type.

## The `ghc-exactprint` Annotation Type

~~~~.hs
Ann  
  { annEntryDelta        :: !DeltaPos
  , annPriorComments     :: ![(Comment, DeltaPos)]
  , annFollowingComments :: ![(Comment, DeltaPos)]
  , annsDP               :: ![(KeywordId, DeltaPos)]
  , annSortKey           :: !(Maybe [SrcSpan])
  , annCapturedSpan      :: !(Maybe AnnKey)
  }
~~~~

But please refer to [the ghc-exactprint docs](https://hackage.haskell.org/package/ghc-exactprint-0.6.2/docs/Language-Haskell-GHC-ExactPrint-Types.html#t:Annotation) for the fully commented version.

A few things to point out:

- There are _three_ constructors that contain the `Comment` type in that
  constructor. `annPriorComments` and `annFollowingComments` are obvious, but
  a third hides behind the `KeywordId` type. Source code comments may appear
  in one of these three locations.
- The `DeltaPos` type and its `DP` constructor can be seen in the above output
  everywhere. It contains information about relative positioning of both
  comments and syntax nodes. Please test what changes if you insert a newline
  before `putStrLn`, or add spaces before one of the comments, and see how the
  `--dump-ast-full` output changes.
- The exact semantics of the `DP` value, especially when it comes to
  indentation, are a source of constant joy. If the values don't make sense,
  you are on the right track. Just figure out what DP is connected to what
  change in the syntax tree for now.
- We have two comments in the source code, which appear in opposite order
  in the `--dump-ast-full` output. The reason is that comments mostly appear
  in the middle of two AST nodes, and it is somewhat arbitary whether we
  connected them as an "after" comment of the first or as an "before" comment
  of the second node. And keep in mind that we have a third constructor that
  can contain comments that are somewhere in the "middle" of a node, too.
- We have `DP`s with negative offsets. Did I mention how much fun `DP`s are?
  I have no idea where the above `-1` comes from.
- The `annsDP` field may also contain the `DP`s of syntax that is somewhere
  "in the middle" of a syntax node, e.g. the position of the `else` keyword.

  We will discuss the semantics of `DP` further down below.

## Data-Flow of a Comment When Round-Tripping

Parsing with `ghc-exactprint` returns both a syntax tree and a map of
annotations (`Map AnnKey Annotation`). Let's consider just the comment
"-- very suspense" in the above example: The annotations map would contain
the following mapping:

~~~~
AnnKey {stdin:3:3-19} (CN "BodyStmt")
  -> Ann { annEntryDelta = DP (1,0)
         , annPriorComments =
             [((Comment "-- very suspense" stdin:2:18-33 Nothing),DP (0,1))]
         , annFollowingComments = []
         , annsDB = []
         , annSortKey = Nothing
         , annCapturedSpan = Nothing
         }
~~~~

where the `AnnKey` is connected to the syntax node `BodyStmt` with the given
source location.

Brittany keeps the annotations map around, and the `BriDoc` structure contains
nodes that have `AnnKey` values, i.e. the `BriDoc` nested documented structure
similarly only contains references into the annotations map. The corresponding
constructors of the `BriDoc(F)` type are:

~~~~.hs
data BriDoc
  = ..
  | BDAnnotationPrior AnnKey BriDoc
  | BDAnnotationKW AnnKey (Maybe AnnKeywordId) BriDoc
  | BDAnnotationRest  AnnKey BriDoc
  | BDMoveToKWDP AnnKey AnnKeywordId Bool BriDoc -- True if should respect x offset
  | ..
~~~~

when rendering a `BriDoc` to text, the semantics of the above nodes can be
described roughly like this:
- `render (BDAnnotationPrior annkey bd)` extracts the "before" type comments
  under the given `annkey` from the annotations map (this is a stateful
  process - they are really removed from the map). It renders these comments.
  If we are in a new line, we respect the `annEntryDelta :: DeltaPos` value
  to insert newlines. The "if in a new line" check prevents us from inserting
  newlines in the case that brittany chose to transform a multi-line layout
  into a single-line layout.

  Then we recursively process `render bd`.
- `render (BDAnnotationsKW annkey mkwId bd)` similarly first renders the comments
  extracted from the annotations map under the given `annkey` before calling
  `render bd`. For example, this would allow us to print the comments _before_
  the closing bracket `]` of an empty list literal e.g.
  `[{-put numbers here to do X-}]`.
- `render (BDMoveToKWDP annkey kwId xToo bd` moves to the relative position of
  the given keyword before continuing with `render bd`.
  It is used for example to insert newlines before a `where` keyword to
  match those of the original source code.
- `render (BDAnnotationsRest annkey bd)` first calls `render bd` and _then_
  takes _any remaining comments_ it can find in the annotations map under the
  given `annkey` and prints them.

### Some Notes to This Design

- We heavily rely on the `ghc-exactprint` library and its types and
  their semantics. We could define our own data structures to capture comments
  and whitespace offsets. While this could allow us to make the later steps
  of the process easier by more closely matching the information we need when
  rendering a `BriDoc` document, it would involve a mildly complex
  extra transformation step from `ghc-exactprint` annotations to hypothetical
  `brittany` annotations.

- For those cases where we rely on `ghc-exactprint` to output syntax that
  `brittany` does not know yet, it is mandatory that we keep the annotations
  around.

- We make the rendering stateful in the annotations. The main advantage to
  this is that we can keep track of any comments that have not yet been
  reproduced in the output, and as a last resort append them at the end. The
  effect of that is that comments "move" down in the document when brittany is
  not exact, but at least it does not "eat" comments. The latter can still
  happen though if we forget to include a given `AnnKey` at all in the `BriDoc`
  document.

  Of course this is a bit yucky, but it seems to be a sensible measure for
  the long transitioning period where `brittany` is not perfect.

- It may be surprising to nest things like we do in the `BriDoc` type.
  The intuitive document representation for something like

    ~~~~.hs
    -- before
    foo
    -- after
    ~~~~

  might be

    ~~~~
    sequence [comment "-- before", text "foo", comment "-- after"]
    ~~~~

  but instead we use

    ~~~~
    BDAnnotationsPrior annkey1 -- yields "-- before"
      BDAnnotationsRest annkey1 -- yields "-- after"
        BDLit "foo"
    ~~~~

  which may seem unnecessarily nested. But this representation has certain
  advantages, most importantly rewriting/restructuring the tree is
  straigh-forward: consider how `BDAnnotationsPrior annkey (BDSeq [a, b])` can
  be transformed into `BDSeq [BDAnnotationsPrior annkey a, b]`. You can do the
  same transformation using the "flat" representation, but there are way more
  cases to consider.

## DeltaPosition semantics

DeltaPositions (we'll just say `DP` which matches the constructor name for this
type) are used to specify where to place comments and regular syntax (including
keywords). This covers both newlines and indentation, and for indentation
includes the case where indentation is mandatory ("layouting rule").

Let us look at this example, which was constructed so that each comment
contains its own DP:

~~~~.hs
do -- DP (0, 1)

  -- DP (2, 2)   two newlines, two spaces indentation
  abc
  -- DB (1, 0)   one newline, zero indentation relative to the do-indentation
  def
~~~~

The first comment is of the easy sort, because it occurs at the end of a
non-empty line: There is no row offset, and the column offset matches the
number of spaces (before the "--") after the last thing in the line.

The second comment _does_ have a row offset: After the last comment, we have
to insert two line-breaks, then apply the indentation (two spaces) and then
insert the comment starting with "--". This is straight-forward so far.

The third comment however highlights how DPs are affected by the layouting
rule.

### Caveat One: Indentation relative to layouting rule indentation level

Following the first two cases, one would assume that the DP would be
`(1, 2)`. However, for cases where the layouting rule applies
(`do`, `let`, `where`) the indentation of the comments is expressed relative
to the _current indentation_ according to the layouting rule. Unfortunately,
this _current indentation_ is not known until the first construct after
the let, so in the above example, the comment between the `do` and the first
construct (`abc`) has an indentation relative to the enclosing indentation
level (which is 0 for this example). This applies _even_ if the comment is
connected to the first construct (if the first comment is a "prior" comment
of the "abc" syntax node).

This applies not only to comments, but also to the DPs of all syntax nodes
(including keywords).

This also means that it is possible to have negative indentation. Consider
this comment:

~~~~.hs
do
  abc
 -- fancy comment
  def
~~~~

### Caveat Two: Caveat one applies to more than the layouting rule

There are syntactic constructs, for example data declarations, where the
layouting rule does not apply, but for the purpose of `DP` indentations
`ghc-exactprint` pretends that it does. For example:

~~~~.hs
data Foo = Foo
  { myInt :: Int
    -- DP (1, -7)    relative to the `Foo` constructor (!)
  }
~~~~

The layouting rule does _not apply in any way_ here. Still, we get a rather
unexpected DP.

### DeltaPositions of keywords and syntax nodes

We have mostly talked about comments, but DPs exist and work for keywords
and syntax nodes just like they do for comments.

~~~~.hs
func = x
 
 where

  x = 42
~~~~

here, the `where` keyword has a DP of `(2, 1)` and the `x = 42` equation
has a DP of `(2, 2)`. We make use of these DPs using the `BDMoveToKWDP` or the
`BDAnnotationPrior` constructors of the `BriDoc` document. The former would be
used for the `where` keyword, the latter would be applied to the equation
document.
