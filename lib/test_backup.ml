module FS =
  Filesystem.Make (Notifiable.IgnoreNotifications)
module Backup = Backup.Make

let%test_module _ =
  (module struct
    let _ = FS.mkdirs "dog/cat/bat"
    let _ = Unix.system "echo 12345 > dog/test"
    let _ = Unix.system "echo 12345 > test2"

    let _ =
      Unix.system "echo 1234567 > dog/cat/bat/test3"

    let _ = Unix.system "echo 1234567 > test4"
    let _ = Unix.mkdir "test_backup" 0o777
    let file1 = File.from_path "dog/test"
    let file2 = File.from_path "test2"
    let file3 = File.from_path "dog/cat/bat/test3"
    let file4 = File.from_path "test4"

    let test_backup =
      Backup.empty "test_backup"
        (module FS : Filesystem.S)

    let result =
      List.fold_left
        (fun r p -> Exnlogger.bind r (Backup.add p))
        (Exnlogger.return test_backup)
        [ "dog"; "test2" ]

    let test_backup = result |> Exnlogger.get

    let%test "has1" =
      Backup.has "dog/test" test_backup = true

    let%test "has2" =
      Backup.has "test2" test_backup = true

    let%test "has3" =
      Backup.has "dog/cat/bat/test3" test_backup = true

    let%test "has4" =
      Backup.has "test4" test_backup = false

    let%test "copy1" =
      Backup.find_copy file1 test_backup = Some file2

    let%test "copy2" =
      Backup.find_copy file2 test_backup = Some file1

    let%test "copy3" =
      Backup.find_copy file3 test_backup = None

    let%test "copy4" =
      Backup.find_copy file4 test_backup = Some file3
  end)
