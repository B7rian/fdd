let%expect_test "mkdirs" = 
  let _ = Filesystem.mkdirs "adir/bdir/cdir" in
  let _ = Unix.system "find adir" in
  [%expect {|
    adir
    adir/bdir
    adir/bdir/cdir
    |}]

let%expect_test "dir_to_seq" =
  let _ = Filesystem.mkdirs "seq_dir" in
  let _ = Unix.system 
            "touch seq_dir/file_a seq_dir/file_b" in
  let s = Filesystem.dir_to_seq "seq_dir" in
  List.iter (Stdio.printf "%s ") @@ List.of_seq s;
  [%expect {| .. file_a file_b . |}]
          



