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

- docAltFilter

  simple utility wrapper around `docAlt`: Each alternative is accompanied by
  a boolean; if False the alternative is discarded.

- docPar/BDPar

  TODO

- docLines/BDLines

  TODO

- docSeparator/BDSeparator

  Adds a space, unless it is the last element in a line. Also merges with
  other separators and has no effect if inserted right after inserting space
  (e.g. in the start of a line when indented) or if already indented due to
  horizontal alignment.

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
    docCols equation
      "func"
      docCols
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
      docCols
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

TODO

- docNonBottomSpacing
- docSetParSpacing
- docForceParSpacing
- docForceSingleline
- docForceMultiline

### Inserting comments / Controlling comment placement

TODO

- docAnnotationPrior
- docAnnotationKW
- docAnnotationRest

### Deprecated

- BDForwardLineMode is unused and apparently should be deprecated.
- BDProhibitMTEL is deprecated

