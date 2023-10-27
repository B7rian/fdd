(* default_bs is the default block size for channel reads and writes *)
let default_bs = 4096

let rec copy_channel ic oc buffer size =
  let count = In_channel.input ic buffer 0 size in
  match count with
    0 -> ()
  | _ ->
    Out_channel.output oc buffer 0 count;
    copy_channel ic oc buffer size

let copy_file_by_name fn1 fn2 =
  let b = Bytes.create default_bs in
  In_channel.with_open_bin fn1
    (fun ic -> Out_channel.with_open_bin fn2
        (fun oc -> copy_channel ic oc b default_bs))

let copy_file_to_dir dir file =
  copy_file_by_name file (dir ^ "/" ^ file)
      
(* let sha256sum_file_by_name fn = *)
(*   let digest = Sha256.file_fast fn in *)
(*   Printf.printf "%s  %s" (Sha256.to_hex digest) fn *)

let cp srcs dest =
  let many = List.length srcs > 1 in
  if many && (not (Sys.file_exists dest) || not (Sys.is_directory dest))
  then `Error (false, dest ^ ": not a directory")
  else `Ok (List.iter (copy_file_to_dir dest) srcs)
