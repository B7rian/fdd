Try to copy a file that does not exist
  $ dune exec fdd noexist repo_noexist
  fdd: SOURCE… arguments: no 'noexist' file or directory
  Usage: fdd [OPTION]… SOURCE… DEST
  Try 'fdd --help' for more information.
  [124]
  $ find repo_noexist
  find: 'repo_noexist': No such file or directory
  [1]
