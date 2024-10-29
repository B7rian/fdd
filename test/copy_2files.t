Make a file full of random stuff and copy it
  $ cat > test_file <<EOF
  > This is a test file
  > It has 2 lines
  > EOF
  $ cp test_file test_file2
  $ mkdir tmp
  $ dune exec fdd test_file test_file2 tmp
  copy test_file...(0)copy test_file...(35) done
  link tmp/test_file2...(0)link tmp/test_file2...done
  $ diff test_file tmp/test_file
  $ diff test_file2 tmp/test_file2
  $ sha256sum -c tmp/checksums
  test_file2: OK
  test_file: OK
