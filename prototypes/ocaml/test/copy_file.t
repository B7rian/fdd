Make a file full of random stuff and copy it
  $ cat > test_file <<EOF
  > This is a test file
  > It has 2 lines
  > EOF
  $ mkdir tmp
  $ dune exec pback test_file tmp
  $ diff test_file tmp/test_file
