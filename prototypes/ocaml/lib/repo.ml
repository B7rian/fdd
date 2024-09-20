type t = {
    dir : String.t;
    files : File.t list;
}

let empty d = { dir = d; files = [] }

let add r f =
  match Filesystem.copy_file_to_dir (File.path f) r.dir
  with
    Ok _ -> { dir = r.dir; files = f :: r.files }
  | _ -> r

let has r p =
  List.exists
    (fun e -> File.path e = File.path p)
    r.files


(*
  Tests
*)

let%test_module _ = (module struct
  let _ = Unix.system "echo 12345 > test"
  let _ = Unix.system "echo 12345 > test2"
  let _ = Unix.system "echo 1234567 > test3"
  let f1 = File.from_path "test"
  let f2 = File.from_path "test2"
(*   let f3 = File.from_path "test3" *)

  let _ = Unix.mkdir "repo_test" 0o777
  let r = empty "repo_test"
  let%test _ = has r f1 = false

  let r2 = add r f1
  let%test _ = has r2 f1 = true
  let%test _ = has r2 f2 = false
end)
