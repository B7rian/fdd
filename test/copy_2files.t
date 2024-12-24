Make a file full of random stuff and copy it
  $ cat > test_file <<EOF
  > This is a test file
  > It has 2 lines
  > EOF
  $ cp test_file test_file2
  $ dune exec fdd test_file test_file2 tmp
  copy test_file2...(0)copy test_file2...(35) done
  link tmp/0/test_file...(0)link tmp/0/test_file...done
  $ diff test_file tmp/0/test_file
  $ diff test_file2 tmp/0/test_file2
  $ (cd tmp/0 && sha256sum -c checksums | sort)
  test_file2: OK
  test_file: OK
