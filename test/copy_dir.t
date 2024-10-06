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
  $ find a repo/a
  $ ls -l repo/a/test_file2
  $ diff -r a repo/a

