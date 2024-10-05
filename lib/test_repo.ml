let%test_module _ =
  (module struct
    let _ = Unix.system "echo 12345 > test"
    let _ = Unix.system "echo 12345 > test2"
    let _ = Unix.system "echo 1234567 > test3"
    let _ = Unix.system "echo 1234567 > test4"
    let _ = Unix.mkdir "test_repo" 0o777
    let file1 = File.from_path "test"
    let file2 = File.from_path "test2"
    let file3 = File.from_path "test3"
    let file4 = File.from_path "test4"
    let test_repo = Repo.empty "test_repo"

    let result =
      El_result.(
        Repo.add "test" test_repo
        >>= Repo.add "test2" >>= Repo.add "test3")

    let test_repo = result |> El_result.get
    let%test "has1" = Repo.has "test" test_repo = true
    let%test "has2" = Repo.has "test2" test_repo = true
    let%test "has3" = Repo.has "test3" test_repo = true

    let%test "has4" =
      Repo.has "test4" test_repo = false

    let%test "copy1" =
      Repo.find_copy file1 test_repo = Some file2

    let%test "copy2" =
      Repo.find_copy file2 test_repo = Some file1

    let%test "copy3" =
      Repo.find_copy file3 test_repo = None

    let%test "copy4" =
      Repo.find_copy file4 test_repo = Some file3
  end)
