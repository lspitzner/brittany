# BriDoc nodes/Smart constructors and their semantics

At this point, you should have a rough idea of what the involved
types mean. This leaves us to explain the different `BriDoc`
(smart) constructors and their exact semantics.

### Special nodes

- docDebug/BDDebug

  Like the `trace` statement of the `BriDoc` type. It does not affect the
  normal output, but prints stuff to stderr when the transformation traverses
  this node.

- BDExternal is used for original-source reproduction.

### Basic nodes

- docEmpty/BDEmpty Text

  ""

  The empty document. Has empty output. Should never affect layouting.

- docLit/BDLit

  "a" "Maybe" "("

  The most basic building block - a simple string. Has nothing to do with
  literals in the parsing sense. Will always be produces as-is in the output.
  It must be free of newline characters and should normally be free of any
  spaces (because those would never be considered for line-breaking - but there
  are cases where this makes sense still).

- docSeq/BDSeq [BriDoc]
  
  "func foo = 13"

  A in-line/horizontal sequence of sub-docs. The sub-documents should not
  contain any newlines, but there is an exception: The last element of the
  sequence may be multi-line. In combination with `docSetBaseY` this allows
  for example:

  ~~~~.hs
  foo | bar = 1
      | baz = 2
  ~~~~

  which is represented roughly like

  ~~~~
  docSeq
    "foo"
    space
    docSetBaseY
      docLines
        stuff that results in "| bar = 1"
        stuff that results in "| baz = 2"
  ~~~~

  But in general it should be preferred to use `docPar` to handle multi-line
  sub-nodes, where possible.

- docAlt/BDAlt [BriDoc]

  Specify multiple alternative layouts. Take care to appropriately maintain
  sharing for the documents representing the children of the current node.

  See the "Controlling layouting" section below.

- docAltFilter

  simple utility wrapper around `docAlt`: Each alternative is accompanied by
  a boolean; if False the alternative is discarded.

- docPar `:: m BriDocNumbered -> m BriDocNumbered -> m BriDocMumbered`

  (does not completely match `BDPar`, which has an extra argument.)
  
  Describes a "paragraph" - a layout consisting of some headline (which must
  be free of newlines) and content (that may contain newlines). Simple example
  is a `do`-block:

  ~~~~.hs
  do -- headline
    stmt -- content
    stmt -- content
    stmt -- content
  ~~~~

  But let us first consider the simplest case: `docPar fooDoc barDoc`
  placed at the start of the line; it will be layouted like this:

  ~~~~.hs
  foo
  bar
  ~~~~

  As you can see, the content is not indented by default. In this form,
  `docPar a b` behaves like `docLines [a,b]`, and `docPar a (docLines bs)` like
  `docLines (a:bs)`. What makes `docPar` special is that it allows differing
  indentation of headline and content, where the lines of `docLines` are
  supposed to have the same indentation.

  This allows two common uses of `docPar`:

  1. The pattern `docAddBaseY BrIndentRegular $ docPar _ _`. `docAddBaseY`
     does not affect
     the current line (i.e. the headline of `docPar`) but it _does_ indent the
     content.

  2. At the end of a sequence; the following is valid and common:
     `docSeq [elem1, elem2, docPar elem3 content]` which looks like
     ~~~~.hs
     elem1 elem2 elem3
     content
     ~~~~

     So the headline does not need occur at the start of the line.

   This interaction between `docSeq`, `docAddBaseY`, and `docPar` allows us to
   add indentation to the content of a childnode without even knowing if that
   childnode will actually make use of `docPar`. We can simply use
   `docAddBaseY BrIndentRegular $ docSeq [foo, bar, childNodeDoc]` and get
   sensible layout including indentation of the _potential_ content-part of the
   child node. Such a behaviour would not be possible without this interaction
   unless we resorted to analysing the doc created for the childnode - which
   would lead to complex special-casing.

    ~~~~.hs
    foo bar child-oneline
    -- or
    foo bar child-headline
      child-content
    ~~~~

   This pattern does however require that we keep this interaction in mind
   when writing the layouting of such parent/childnode relationships. For
   example using `docLines` in the child node instead of `docPar` would
   probably lead to bad results if the parent used `docAddBaseY`.

- docLines/BDLines

  Where `docSeq` is horizontal sequence, `docLines` is the vertical sequence
  operator. `docLines` has one important requirement: All lines must have the
  same indentation. Violating this will lead to undefined layouting behaviour.

  As a consequence, there are two valid usage patterns:

  1. `docLines` is used at the start of a line, e.g. as the content of a
     `docPar`.

  2. The `docSetBaseY $ docLines _` or
     `docSetBaseAndIndent $ docNonBottomSpacing $ docLines _` patterns allow
     using `docLines` as a final element of a sequence; the `docSetBase~`
     constructs ensure that the rest-lines are indented as much as the
     headline. An example is:
  
     ~~~~.hs
     foo | bar = 1
         | baz = 2
     ~~~~

     where "| bar = 1" and "| bar = 2" are two lines of a docLines.

- docSeparator/BDSeparator

  Adds a space, unless it is the last element in a line. Also merges with
  other separators and has no effect if inserted right after inserting space
  (e.g. in the start of a line when indented) or if already indented due to
  horizontal alignment.

  Note also this helper:

  ~~~~.hs
  appSep :: ToBriDocM BriDocNumbered -> ToBriDocM BriDocNumbered
  appSep x = docSeq [x, docSeparator]
  ~~~~


### Creating horizontal alignment

- docCols/BDCols ColSig [BriDoc]
  
  This works like docSeq, but adds horizontal alignment if possible. The
  implementation involves a lot of special-case trickeries and I assume that
  it is impossible to specify the exact semantics. But the rough idea is:
  If

  1. horizontal alignment is not turned off via global config
  2. there are consecutive lines (created e.g. by docLines or docPar) and
  3. both lines consist of docCols (where "consist" can ignore certain shallow
     wrappers like `docAddBaseY`) and
  4. the two ColSigs are equal and
  5. the two docCols contain an equal number of children and
  6. there is enough horizontal space to insert the additional spaces

  then the contained docs will be aligned horizontally.

  And further, if there are multiple lines so that consecutive pairs fulfill
  these requirements, the whole block will be aligned to the same horizontal
  tabs.

  And further, if a docCols contains another docCols, and the docCols in the
  next line also does, and the child docCols also match in ColSigs and have
  the same number of arguments and so on, then the children's children are
  also aligned horizontally.

  And of course this nesting also works over blocks built of matching
  consecutive pairs.

  Wait, was this not supposed to be broadly simplifying? Well.. it is. uhm.
  Let us just.. example.. an example seems fine.

  Considering the following declaration/formatting:

  ~~~~.hs
  func (MyLongFoo abc def) = 1
  func (Bar       a   d  ) = 2
  func _                   = 3
  ~~~~

  Note how the "=" are aligned over all three lines, and the patterns in the
  first two lines are as well, but the pattern in the third line is just a
  structureless underscore?

  The representation behind that source is something in the direction of this
  (heavily simplified and not exact at all; e.g. spaces are not represented at
  all):

  ~~~~
  docLines
    docCols equationSigToken
      "func"
      docCols patternSigToken
        "("
        "MyLongFoo"
        "abc"
        "def"
        ")"
      docSeq
        "="
        "1"
    docCols equation
      "func"
      docCols patternSigToken
        "("
        "Bar"
        "a"
        "d"
        ")"
      docSeq
        "="
        "2"
    docCols equation
      "func"
      "_"
      docSeq
        "="
        "3"
  ~~~~

### Controlling indentation level

TODO

- docAddBaseY/BDAddBaseY
- docSetBaseY
- docSetIndentLevel
- docSetBaseAndIndent
- docEnsureIndent

### Controlling layouting

The purpose of these nodes/modifiers is affecting the choices of alternatives
(see `docAlt`) made. For example in a bridoc tree like

~~~~
docAlt
  docForceSingleLine
    [stuff]
  [otherOption]
~~~~

if stuff only returns layouts that use multiple lines, then this alternative
will not be considered, and this will be effectively simplified to just
`[otherOption]`.

- docNonBottomSpacing

  Enforces that this node is _not_ discarded even when all considered layouts
  use more space than available. This counteracts the fact that we consider
  a limited amount of layouts in order to retain linear runtime. Bad usage
  of this modifier will lead to unnecessary overflow over the max-columns (80
  by default) even when other layoutings were available.

  [TODO: consideration of valid usecases]

- docSetParSpacing and docForceParSpacing

  We say a node has "ParSpacing" if it looks like a `docPar` result.. it has
  a headline and (indented) content in new lines. This property can propagate
  somewhat non-trivially upwards and is used by certain parents. It mainly
  provides nice layouting choices in cases such as:

  ~~~~.hs
  foo = abc $ def $ do
    stmt
    stmt
  ~~~~

  Consider what we know when translating the equation: We have two
  possibilites:

  ~~~~.hs
  foo = child-node-doc -- note that child may contain a docPar.
  -- or
  foo =
    child-node-doc
  ~~~~

  As usual, we do not to inspect child-node-doc; this makes deciding between
  the two choices hard. Looking at is-single/multi-line is not sufficient.

  [TODO]

- docForceSingleline

  Discards child layouts that contain newlines.

- docForceMultiline

  Discards child layouts lacking newlines.

### Inserting comments / Controlling comment placement

TODO

- docAnnotationPrior
- docAnnotationKW
- docAnnotationRest

### Deprecated

- BDForwardLineMode is unused and apparently should be deprecated.
- BDProhibitMTEL is deprecated

