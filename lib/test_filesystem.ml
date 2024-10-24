let%expect_test "mkdirs" =
  let _ = Filesystem.mkdirs "adir/bdir/cdir" in
  let _ = Unix.system "find adir" in
  [%expect
    {|
    adir
    adir/bdir
    adir/bdir/cdir
    |}]

let%expect_test "dir_to_seq" =
  let _ = Filesystem.mkdirs "seq_dir" in
  let _ =
    Unix.system "touch seq_dir/file_a seq_dir/file_b"
  in
  let s = Filesystem.dir_to_seq "seq_dir" in
  Seq.iter (Stdio.printf "%s ") s;
  [%expect {| seq_dir/file_a seq_dir/file_b |}]

let%expect_test "find" =
  let _ = Filesystem.mkdirs "fdir1/fdir2/fdir3" in
  let _ = Unix.system "touch fdir1/file_a" in
  let _ = Unix.system "touch fdir1/fdir2/file_a" in
  let _ = Unix.system "touch fdir1/fdir2/file_b" in
  let _ =
    Unix.system "touch fdir1/fdir2/fdir3/file_c"
  in
  let s =
    Filesystem.find (fun _x -> true) [ "fdir1" ]
  in
  let t =
    Filesystem.find
      (fun x -> x = "fdir1/file_a")
      [ "fdir1" ]
  in
  Seq.iter (Stdio.printf "[%s] ") s;
  Seq.iter (Stdio.printf "[%s] ") t;
  [%expect
    {| [fdir1/file_a] [fdir1/fdir2] [fdir1/fdir2/fdir3] [fdir1/fdir2/fdir3/file_c] [fdir1/fdir2/file_a] [fdir1/fdir2/file_b] [fdir1/file_a] |}]
