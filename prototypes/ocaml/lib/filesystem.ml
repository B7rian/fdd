type r = Ok of string | Error of string * string
(** [r] is the copy result type
[Ok filename] means the file copied ok
[Error (filename, msg)] means there was an error *)

(** [default_bs] is the default block size for
    channel reads and writes *)
let default_bs = 4096

let rec copy_channel ic oc buffer size =
  let count = In_channel.input ic buffer 0 size in
  match count with
    0 -> ()
  | _ ->
    Out_channel.output oc buffer 0 count;
    copy_channel ic oc buffer size

let copy_file_by_name f1 f2 =
  let b = Bytes.create default_bs in
  In_channel.with_open_bin f1
    (fun ic -> Out_channel.with_open_bin f2
        (fun oc -> copy_channel ic oc b default_bs))

let copy_file_to_dir file dir =
  let dest = dir ^ "/" ^ file in
  match copy_file_by_name file dest with
  | exception e -> Error (file, Printexc.to_string e)
  | _ -> Ok dest

(* let cp srcs dest = *)
(*   (List.map (copy_file_to_dir dest) srcs) *)




