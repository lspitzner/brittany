# Introduction

[The readme](../../master/README.md) mentions a couple of goals for this
project, including the following two:

- Be clever about using the available horizontal space while not overflowing
  it if it cannot be avoided;
- Have linear complexity in the size of the input.

These two goals stand in conflict, and this project chooses a solution
to this that distinguishes it from (all) other existing formatters. This
approach was an important motivation for writing Brittany. The approach might
also be applicable to more general-purposes structured-document formatters
(although we will not expand on that here). Before explaining the idea, we
will first consider

## The problem

Every haskell module can be written in a single line - of course, in most
cases, an unwieldly long one. We humans prefer our lines limitted to some
laughingly small limit like 80 or 160 or whatever. Further, we generally
prefer the indentation of our expressions(, statements etc.) line up with
its syntactic structure. This preferences (and the layouting rule which
already enforces it partially) still leaves a good amount of choice for
where exactly to insert newlines. For example, these are all consistent
in their newline/indentation usage:

~~~~.hs
myRecord = MyRecord { abc = "abc", def = "def"}
myRecord = MyRecord
  { abc = "abc"
  , def = "def"
  }
myList = [ "abc", "def"]
myList =
  [ "abc"
  , "def"
  ]
~~~~

While consistency has the first priority, we also prefer short code: If it
fits, we prefer the version/layout with less lines of code. So we wish to trade
more lines for less columns, but only until things fit.

For simple cases we can give a trivial rule: If there is space
for the one-line layout, use it; otherwise use the indented-multiline
layout (this applies to `myRecord` example). The
straight-forward approach works well: Calculate the length of the
one-line layout for each node, then recurse top-down and make the choices
according to that rule. Linear runtime with good results.

Things get more interesting with nesting, and with structures with more than
two alternatives. Consider the following four alternative layouts of the
same expression:

~~~~.hs
--        10        20        30        40
-- 1)                                   -- lets assume the user wants
nestedCaseExpr = case e1 of             -- 40 columns max.
  Left x -> if func x then "good" else "bad" -- too long
-- 2)                                   -- 
nestedCaseExpr = case e1 of             -- 
  Left x -> if func x                   -- 
    then "good"                         -- 
    else "bad"                          -- 
-- 3)                                   -- 
nestedCaseExpr = case e1 of             -- 
  Left x ->                             -- 
    if func x then "good" else "bad"    -- 
-- 4)                                   -- 
nestedCaseExpr = case e1 of             -- 
  Left x ->                             -- 
    if func x                           -- 
      then "good"                       -- 
      else "bad"                        -- 
~~~~

- We nest a case-expression and an if-expression; for both we consider two
  alternative layouts, meaning that
- There are a total of four, and in general an exponential amount of
  possible layouts;
- With a top-down approach, one would choose 2), as "if func x" has space in
  the current line. But..
- Layout 3) is the optimal solution considering lines-of-code and the 40-column
  limit.

So our question: Is there an algorithm which, given some input syntax tree,
returns the/an optimal (least lines-of-code while respecting max-column limit)
layout (out of a potentially exponential number of valid layouts). Further,
this algorithm's time/space usage should be linear in the size of the input
(we might weaken this to some polynomial upper bound - but that should be the
limit).

If we pessimistically assume that such algorithm does not exist, we may ask
alternatively: What linear algorithm returns solutions which,
on average, are closest to the optimal solution?

In the following we will describe _one_ such algorithm that seems to return
the optimal solution in several non-trivial cases, and near-optimal solutions
in many other. We won't try to prove that it is the best algorithm, but we will
consider the circumstances for which a non-optimal solution is returned.

## The Reasoning

A top-down approach is so bad because when there are exponentially many
layouts to consider, there information passed down from the parents does
not help at all in pruning the alternatives on a given layer. In the above
`nestedCaseExpr` example, we might obtain a better solution by looking not
at just the first, but the first n possible layouts, but against an exponential
search-space, this does not scale: Just consider the possibility that there
are exponentially many sub-solutions for layout 2) (replace "good" with some
slightly more complex expression). You basically always end up with either
"the current line is not yet full, try to fill it up" or
"more than n columns used, abort".

But a (pure) bottom-up approach does not work either: If we have no clue about
the "current" indentation while layouting some node of our syntax tree,
information about the (potential) size of (the layout of) child-nodes does
not allow us to make good decisions.

So we need information to flow bottom-to-top to allow for pruning whole trees
of possible layouts, and top-to-bottom for making the actual decisions.. well,
this can be arranged.

# The Algorithm

This algorithm works in two passes over the input syntax tree. The first pass
is bottom-up and adds a label to each node. This label contains (a set of)
"spacings" - certain information regarding the number of columns/lines needed
for the textual representation of potential layouts of that node.
For example we might return two possible spacings for the following (same)
expression:

~~~~.hs
if func x then "good" else "bad"
-- => 1 line, 32 columns used
if func x
  then "good"
  else "bad"
-- => 3 lines, 13 columns used
~~~~

This is heavily simplified; in Brittany spacing information is (as usual) a
bit more complex.

We restrict the size of these sets. Given the sets of spacings for the
child-nodes in the syntax-tree, we generate a limited number of possible
spacings in the current node. We then prune nodes that already violate desired
properties, e.g. any spacing that already uses more columns locally than
globally available.

The second pass is top-down and uses the spacing-information to decide on one
of the possible layouts for the current node. It passes the current
"cursor position" (indentation, etc.) downwards, allowing to check that the
layout fits (e.g. ensure "current indentation + columns used < max columns").

This algorithm is trivially linear - two traversals and only linear space
required per node of the input.

### Consequences

- when calculating spacings during bottom-up, several spacings are combined
  into a single one via min/max/some other associative operation. Perhaps
  consider how in a multi-line list layout the "columns used" will be derived
  from the element spacings. For additional information embedded into the
  spacings, we will need at least one such Semigroup instance.

- We require an order on the alternatives for each syntactical construct.
  The first alternative where we find some combination of spacings for the
  children which is acceptable will be used.

  In unlucky cases, all spacings might get pruned. In that case, we will
  default to the last alternative, which therefor should be the most
  "conservative" choice, i.e. the one that gives the child-nodes the most
  space.

  The first alternative should always be the "one-liner" layout, if it exists;
  this alternative is preferable in general and will be filtered first should
  it not fit.

  In between those two extremes can be other choices that gradually trade
  "columns used" for "lines used".

- Sometimes there exist several (near) optimal solutions. E.g. when there exist
  multiple nodes where we can trade lines for columns in such a way that the
  whole result fits.

  In such cases, Brittany makes the following choice: We prefer "trading"
  in the node higher up in the syntax tree. The reasoning is that we rather
  spend one line early on to gain additional horizontal space for _all_ the
  children than the other way around. (In those cases where there are
  alternatives to choose from, there are often several children.) We can apply
  this to the `nestedCaseExpr` example above: The options are to either
  put the right-hand-side of the case-alternative into a new line, or
  split up the if-then-else. The "case" is the outer one, so Brittany will
  prefer 3) over 2), which proves to be the right choice (at least in this
  case).

  As a consequence, we are most interested in the maximum spacings. Still we do
  not have a total order because we cannot generally prefer one of two spacings
  where the first uses more columns, the second more lines.

- The number of syntactical constructs in haskell is large. If we were to
  work directly on the syntax tree to do our traversals, we'd have to write two
  (or even three - one to generate spacings, one to make the choices, one to
  do the output and insert comments) different functions each respecting every
  syntactical construct in haskell. It would be an incredible amount of code
  (and work) times three.

  Instead we can use some recursive data-type describing a structured document,
  which abstracts of different syntactical constructs and only considers the
  things relevant for layouting. This data-type is called `BriDoc`.

- If we did not share values, we'd work on `BriDoc` trees of exponential size.
  By sharing child-nodes across different alternatives we instead obtain a
  rooted DAG of linear size, but still with an exponential number of different
  paths.
  In the `nestedCaseExpr` example above, note how there are four layouts, but
  essentially only two ways in which the "if" is layouted.
  Either as a single line or with then/else on new lines. We can handle
  spacings in such a way that we can share them for 1/3 and 2/4.
  This already hints at how "columns used" will need to be redesigned slightly
  so that 2/4 really have the same spacing label at the "if".

### Concessions/non-Optimality

- Prefering to trade in the higher node can give non-optimal results, e.g.
  when there is only one child.

- Spacings and the pruning of spacings happens in bottom-up fashion, meaning
  that we might not prune a "70 columns used" spacing even though we later
  notice that the current indentation already needs to be 20 and we only have
  60 columns left. If all spacings found in one label have this property,
  the top-down traversal might be forced to make the "conservative" choice,
  trading more lines for less indentation to prevent potential (but unknown)
  overflow during child-node layouting.

- We can increase the limits arbitrarily (i.e. increase the constant factor
  while remaining linear) to get better (optimal) results in more cases. We can
  choose constant factors in such a way that we get optimal results for all
  inputs up to a certain size. Of course calling them "constant factors" is
  delusional if they grow exponentially - but it is still nice that this
  heuristical algorithm can trivially be transformed into the perfect
  (but exponential) algorithm.

### Examples of non-Optimality and Border-Cases in General

TODO

# Practical details

## Comments

Comments don't affect semantics and thus are free-form; as the user we almost
never want them reformatted in any way. The tempting approach is to keep them
entirely separate: Don't layout comments, and don't let comments affect
layout. However, this not possible always:

~~~~.hs
val = f -- useful comment here
      x
-- reformatted to
val = f -- useful comment here x
~~~~

Does not work too well, even when the one-liner "f x" would certainly be the
layout of choice.

Brittany handles this in two ways:

1) Some alternatives are pruned based on the existence of comments;
2) Always insert newlines after end-of-line comments, and restore the
   current indetation to prevent violation of the layouting rule:

    ~~~~.hs
    val = do
      myAction -- useful comment here
               x
      g
    ~~~~

## CPP

Conditional text-based source-code insertion is horrible.
"#if"-guarded code in one line can easily affect in which way all other code
is parsed. This makes it more or less impossible to layout code involving
the preprocessor. One might be able to re-implement the preprocessor to then
determine when things are safe to layout, but that is no fun, and Brittany
does not bother.

Brittany allows one thing: Working on the already pre-processed code, treating
any disabled sections like comments. This is not safe in general, but is nice
when the user makes only responsible usage of CPP (e.g.: only put "#if"-guards
around module top-level constructs).

## Horizontal alignment

Sometimes horizontal alignment can make things more readable. However there
are good reasons against using such whitespace: It can cause larger diffs
on simple changes and it is rather subjective in which cases things are
"more readable" rather than "annoyingly spaced".

Nonetheless Brittany has a fully-featured implementation for horizontal
alignment:

~~~~.hs
func (MyLongFoo abc def) = 1
func (Bar       a   d  ) = 2
func _                   = 3
~~~~

Again we have to ask the question: Does alignment affect the layouting choices,
and does layouting affect alignment? The answer is clear in this case: No.
First we make layouting-choices, then, independently, we add alignment but only
in those cases where it does not cause overflows.

