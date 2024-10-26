Make a file full of random stuff and copy it
  $ cat > test_file <<EOF
  > This is a test file
  > It has 2 lines
  > EOF
  $ mkdir tmp
  $ dune exec fdd test_file tmp
  copy test_file...(0)copy test_file...(35) done
  $ diff test_file tmp/test_file
