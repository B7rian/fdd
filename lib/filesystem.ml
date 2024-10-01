(** Filesystem provides a high(er) level interface
    to the underlying file system  

    All functions in here signal errors using
    exceptions, usually from the underlying
    function
*)

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
  copy_file_by_name file dest

let symlink_file target link_name =
  Unix.symlink ~to_dir:false target link_name

let mkdirs p =
  let dirs_to_make =
    (* If p is a/b/c dirs_to_make will be
     [ a/b/c; a/b; a ]*)
    List.fold_left
      (fun acc d ->
        match acc with
          | [] -> [ d ]
          | h :: rest -> (h ^ "/" ^ d) :: (h :: rest))
      []
      @@ String.split_on_char '/' p
  in
  List.iter (fun d -> Unix.(
          match mkdir d 0o700 with
          exception Unix_error (EEXIST,_,_) -> ()
          | exception e -> raise e
          | _ -> ()))
  @@ List.rev dirs_to_make

let dir_to_seq path =
  Seq.unfold
    (fun handle -> match Unix.readdir handle with
                   | x -> Some (x, handle)
                   | exception End_of_file ->
                      Unix.closedir handle; None
                   | exception e ->
                      Unix.closedir handle; raise e)
  @@ Unix.opendir path

