Make a bunch of identical files
  $ mkdir -p dir
  $ cat > dir/test_file <<EOF
  > This is a test file
  > It has 2 lines
  > EOF
  $ for i in 1 2 3 4 5 6 7; do cp dir/test_file dir/test_file_$i; done
  $ dune exec -- fdd dir repo1
  copy dir/test_file_7...(0)copy dir/test_file_7...(35) done
  link repo1/0/dir/test_file_6...(0)link repo1/0/dir/test_file_6...done
  link repo1/0/dir/test_file_3...(0)link repo1/0/dir/test_file_3...done
  link repo1/0/dir/test_file_5...(0)link repo1/0/dir/test_file_5...done
  link repo1/0/dir/test_file_2...(0)link repo1/0/dir/test_file_2...done
  link repo1/0/dir/test_file_4...(0)link repo1/0/dir/test_file_4...done
  link repo1/0/dir/test_file_1...(0)link repo1/0/dir/test_file_1...done
  link repo1/0/dir/test_file...(0)link repo1/0/dir/test_file...done
There will also be checksum files in the repo
  $ find repo1 -type f | wc
        2       2      42
  $ find repo1 -type l | wc
        7       7     166

  $ dune exec -- fdd dir repo1
  link repo1/1/dir/test_file_7...(0)link repo1/1/dir/test_file_7...done
  link repo1/1/dir/test_file_6...(0)link repo1/1/dir/test_file_6...done
  link repo1/1/dir/test_file_3...(0)link repo1/1/dir/test_file_3...done
  link repo1/1/dir/test_file_5...(0)link repo1/1/dir/test_file_5...done
  link repo1/1/dir/test_file_2...(0)link repo1/1/dir/test_file_2...done
  link repo1/1/dir/test_file_4...(0)link repo1/1/dir/test_file_4...done
  link repo1/1/dir/test_file_1...(0)link repo1/1/dir/test_file_1...done
  link repo1/1/dir/test_file...(0)link repo1/1/dir/test_file...done
  $ find repo1 -type f | wc
        3       3      60
  $ find repo1 -type l | wc
       15      15     356

  $ dune exec -- fdd -n 3 dir repo1
  copy dir/test_file_7...(0)copy dir/test_file_7...(35)copy dir/test_file_7...(35) done
  link repo1/2/dir/test_file_3...(0)link repo1/2/dir/test_file_3...done
  link repo1/2/dir/test_file_5...(0)link repo1/2/dir/test_file_5...done
  link repo1/2/dir/test_file_2...(0)link repo1/2/dir/test_file_2...done
  link repo1/2/dir/test_file_4...(0)link repo1/2/dir/test_file_4...done
  link repo1/2/dir/test_file_1...(0)link repo1/2/dir/test_file_1...done
  link repo1/2/dir/test_file...(0)link repo1/2/dir/test_file...done
  $ find repo1 -type f | wc
        6       6     126
  $ find repo1 -type l | wc
       21      21     498

  $ dune exec -- fdd -n 3 dir repo2
  copy dir/test_file_7...(0)copy dir/test_file_7...(35)copy dir/test_file_7...(35)copy dir/test_file_7...(35) done
  link repo2/0/dir/test_file_5...(0)link repo2/0/dir/test_file_5...done
  link repo2/0/dir/test_file_2...(0)link repo2/0/dir/test_file_2...done
  link repo2/0/dir/test_file_4...(0)link repo2/0/dir/test_file_4...done
  link repo2/0/dir/test_file_1...(0)link repo2/0/dir/test_file_1...done
  link repo2/0/dir/test_file...(0)link repo2/0/dir/test_file...done
  $ find repo2 -type f | wc
        4       4      90
  $ find repo2 -type l | wc
        5       5     118

  $ dune exec -- fdd -n 2 dir repo2
  link repo2/1/dir/test_file_7...(0)link repo2/1/dir/test_file_7...done
  link repo2/1/dir/test_file_6...(0)link repo2/1/dir/test_file_6...done
  link repo2/1/dir/test_file_3...(0)link repo2/1/dir/test_file_3...done
  link repo2/1/dir/test_file_5...(0)link repo2/1/dir/test_file_5...done
  link repo2/1/dir/test_file_2...(0)link repo2/1/dir/test_file_2...done
  link repo2/1/dir/test_file_4...(0)link repo2/1/dir/test_file_4...done
  link repo2/1/dir/test_file_1...(0)link repo2/1/dir/test_file_1...done
  link repo2/1/dir/test_file...(0)link repo2/1/dir/test_file...done
  $ find repo2 -type f | wc
        5       5     108
  $ find repo2 -type l | wc
       13      13     308

  $ dune exec -- fdd -n 10 dir repo3
  copy dir/test_file_7...(0)copy dir/test_file_7...(35)copy dir/test_file_7...(35)copy dir/test_file_7...(35)copy dir/test_file_7...(35)copy dir/test_file_7...(35)copy dir/test_file_7...(35)copy dir/test_file_7...(35)copy dir/test_file_7...(35)copy dir/test_file_7...(35)copy dir/test_file_7...(35) done
  $ find repo3 -type f | wc
       11      11     282
  $ find repo3 -type l | wc
        0       0       0
