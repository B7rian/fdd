open File

let%test_module _ =
  (module struct
    let _status = Unix.system "echo 12345 > test"
    let f = from_path "test"
    let%test _ = path f = "test"

    (* Duplicates below sanity-check lazy evaluation *)
    let%test _ =
      hash f
      = "f33ae3bc9a22cd7564990a794789954409977013966fb1a8f43c35776b833a95"

    let%test _ =
      hash f
      = "f33ae3bc9a22cd7564990a794789954409977013966fb1a8f43c35776b833a95"

    let%test _ = size f = 6
    let%test _ = size f = 6
    let f2 = from_path "test"
    let%test _ = same_data f f2 = true
    let%test _ = same_name f f2 = true
    let _status = Unix.system "echo 123 > test3"
    let f3 = from_path "test3"
    let%test _ = same_data f f3 = false
    let%test _ = same_name f f3 = false
  end)
