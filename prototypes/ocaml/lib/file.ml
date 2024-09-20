(* A File is a name/path and cached metadata. *)

type t = {
    name : String.t;
    hash : String.t;
    size : int;
  }

let from_path p =
  let sum = Digest.sha256sum_file_by_name p in
  let stat_info = Unix.stat p in
  { name = p;
    hash = sum;
    size = stat_info.st_size; }

let path x = x.name
let size x = x.size

(* hash computes the hash if necessary and returns
   it *)
let hash x = x.hash

(* Comparisons are straightforward on the surface
   but may trigger hash generation or filesystem
   stat calls to get the data needed *)
let same_name t1 t2 = String.equal t1.name t2.name

let same_data t1 t2 =
  t1.size = t2.size && t2.hash = t1.hash

(*
  Tests
*)

let%test_module _ = (module struct
    let _status = Unix.system("echo 12345 > test")
    let f = from_path "test"
    let%test _ = path f = "test"
    let%test _ = hash f = "f33ae3bc9a22cd7564990a794789954409977013966fb1a8f43c35776b833a95"
    let%test _ = size f = 6
end)
