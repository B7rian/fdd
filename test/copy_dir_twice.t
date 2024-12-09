Make files in a directory tree full of stuff and copy them
  $ mkdir -p a/b/c/d
  $ cat > a/b/c/d/test_file <<EOF
  > This is a test file
  > It has 2 lines
  > EOF
Here is a file with the same name but different contents
  $ cat > a/b/test_file <<EOF
  > This is a test file
  > It has 3 lines
  > This is the 3rd one
  > EOF
test_file2 is the same as test_file and should link
  $ cp a/b/c/d/test_file a/test_file2
  $ dune exec fdd a repo
  copy a/b/test_file...(0)copy a/b/test_file...(55) done
  copy a/test_file2...(0)copy a/test_file2...(35) done
  link repo/0/a/b/c/d/test_file...(0)link repo/0/a/b/c/d/test_file...done
  $ (cd repo/0 && sha256sum -c checksums | sort)
  a/b/c/d/test_file: OK
  a/b/test_file: OK
  a/test_file2: OK
  $ diff -r a repo/0/a
Modify test_file4 so it gets cooied again
  $ cat >> a/b/test_file <<EOF
  > Oh now it has 4 lines
  > EOF
Rewrite this test file so it has the same size but a 
different sha
  $ cat > a/b/c/d/test_file <<EOF
  > This is a test file
  > It has n lines
  > EOF
  $ dune exec fdd a repo
  link repo/1/a/test_file2...(0)link repo/1/a/test_file2...done
  copy a/b/c/d/test_file...(0)copy a/b/c/d/test_file...(35) done
  copy a/b/test_file...(0)copy a/b/test_file...(77) done
  $ (cd repo/1 && sha256sum -c checksums | sort)
  a/b/c/d/test_file: OK
  a/b/test_file: OK
  a/test_file2: OK
  $ find a repo -type f | sort
  a/b/c/d/test_file
  a/b/test_file
  a/test_file2
  repo/0/a/b/test_file
  repo/0/a/test_file2
  repo/0/checksums
  repo/1/a/b/c/d/test_file
  repo/1/a/b/test_file
  repo/1/checksums
  $ find a repo -type l | sort
  repo/0/a/b/c/d/test_file
  repo/1/a/test_file2

