# brittany
haskell source code formatter

This project's goals roughly are to:

- Be idempotent (this also directly ensures that only valid haskell is
  produced);
- Support the full ghc-haskell syntax including syntactic extensions;
- Retain newlines and comments unmodified;
- Be clever about using the available horizontal space while not overflowing
  it if it cannot be avoided;
- Be clever about aligning things horizontally.
- Have linear complexity in the size of the input.

At this point, these goals are completed to different degrees. It is not
ensured that only syntactically valid haskell is produced (yet), and coverage
of the project's testsuite is rather limited. By using ghc-exactprint as the
parser, brittany supports full ghc syntax including extensions, but many of the
more obscure cases are not handled yet. More importantly, only type-signatures
and function/value bindings are processed; other module elements (data-decls,
classes, instances, imports/exports etc.) are not
transformed in any way; this extends to e.g. bindings inside class instance
definitions - they won't be touched (yet).

The current algorithm is rather clever about horizontal space while still being
linear in the size of the input (although the constant factor is not small).

# Important notes

- `-XCPP` is not officially supported (yet).
- Some commandline flags mentioned in the help don't work yet (and won't even
  be parsed correctly.)
- Creates a `brittany.yaml` config file _in the current directory_. I am aware
  that this behaviour is not optimal.
- Currently some unhandled syntactical constructs don't raise errors; in such
  cases the output will contain ghc-exactprint'ed code and some debugging
  comment. This could easily make the output invalid haskell.
- There are cases where comments are not copied to the output (this will
  be detected and the user will get an error); there are other cases where
  comments are moved slightly; there are also cases where comments result in
  wonky newline insertion (although this should be a purely aesthetic issue.)

# Usage

- Currently one mode of operation: Transform a single module. By default read
  from `stdin` and written to `stdout`, but commandline arguments allow to
  read/write from/to files.
