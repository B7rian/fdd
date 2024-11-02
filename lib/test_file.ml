open File

let%expect_test "from_hash" =
  let _ = Unix.system "echo abc > abc" in
  let f = File.from_checksums "abc" "hashhashhash" in
  Printf.printf "%s %s %i\n" (File.path f)
    (File.hash f) (File.size f);
  [%expect {| abc hashhashhash 4 |}]

let%expect_test "from_path" =
  let _ = Unix.system "echo 12345 > test" in
  let _ = Unix.system "echo 123 > test3" in
  let f = from_path "test" in
  let f2 = from_path "test" in
  let f3 = from_path "test3" in
  let _ =
    if same_data f f2 then
      print_endline "same_data f f2"
    else print_endline "NOT same_data f f2"
  in

  let _ =
    if same_name f f2 then
      print_endline "same_name f f2"
    else print_endline "NOT same_name f f2"
  in

  let _ =
    if same_data f f3 then
      print_endline "same_data f f3"
    else print_endline "NOT same_data f f3"
  in

  let _ =
    if same_name f f3 then
      print_endline "same_name f f3"
    else print_endline "NOT same_name f f3"
  in
  Printf.printf "%s %s %i\n" (File.path f)
    (File.hash f) (File.size f);
  Printf.printf "%s %s %i\n" (File.path f)
    (File.hash f) (File.size f);
  [%expect
    {|
    same_data f f2
    same_name f f2
    NOT same_data f f3
    NOT same_name f f3
    test f33ae3bc9a22cd7564990a794789954409977013966fb1a8f43c35776b833a95 6
    test f33ae3bc9a22cd7564990a794789954409977013966fb1a8f43c35776b833a95 6
    |}]
