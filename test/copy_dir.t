Make files in a directory tree full of random stuff and copy them
  $ mkdir -p a/b/c/d
  $ cat > a/b/c/d/test_file <<EOF
  > This is a test file
  > It has 2 lines
  > EOF
  $ cat > a/b/test_file <<EOF
  > This is a test file
  > It has 3 lines
  > This is the 3rd one
  > EOF
  $ cp a/b/c/d/test_file a/test_file2
  $ mkdir repo
  $ dune exec fdd a repo
  copy a/b/test_file...(0)copy a/b/test_file...(55) done
  copy a/b/c/d/test_file...(0)copy a/b/c/d/test_file...(35) done
  link repo/a/test_file2...(0)link repo/a/test_file2...done
  $ find a repo/a
  a
  a/b
  a/b/test_file
  a/b/c
  a/b/c/d
  a/b/c/d/test_file
  a/test_file2
  repo/a
  repo/a/b
  repo/a/b/test_file
  repo/a/b/c
  repo/a/b/c/d
  repo/a/b/c/d/test_file
  repo/a/test_file2
  $ diff -r a repo/a

