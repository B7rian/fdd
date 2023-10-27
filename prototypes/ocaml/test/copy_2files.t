Make a file full of random stuff and copy it
  $ cat > test_file <<EOF
  > This is a test file
  > It has 2 lines
  > EOF
  $ cp test_file test_file2
  $ mkdir tmp
  $ dune exec pback test_file test_file2 tmp
  $ diff test_file tmp/test_file
  $ diff test_file2 tmp/test_file2
