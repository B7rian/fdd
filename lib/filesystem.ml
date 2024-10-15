(** Filesystem provides a high(er) level interface
    to the underlying file system  

    All functions in here signal errors using
    exceptions, usually from the underlying
    function
*)

let is_dir p =
  match Unix.stat p with
  | { st_kind = S_DIR; _ } -> true
  | _ -> false

let is_file p =
  match Unix.stat p with
  | { st_kind = S_REG; _ } -> true
  | _ -> false

let path_to dst src =
  (* Here’s how this code will find the relative path from one directory to another
   * 1. Find the common parent directory and ignore this as is does not need to be included in the relative path
   * 2. Generate a series of ..s to go from the source directory to the common parent
   * 3. Append the paths from the common parent to the destination. 
   * The code will work from left to right to ignore the path to the common parent, create the series of ..s and then append what’s left of the destination path. *)
  let rec fold_paths acc d s =
    match (d, s) with
    | [], [] -> acc
    | dh :: dt, [] ->
        fold_paths (Filename.concat acc dh) dt []
    | [], _sh :: st ->
        fold_paths (Filename.concat ".." acc) [] st
    | dh :: dt, sh :: st ->
        if String.length acc = 0 && dh = sh then
          fold_paths acc dt st
        else
          fold_paths
            (Filename.concat
               (Filename.concat ".." acc)
               dh)
            dt st
  in
  fold_paths ""
    (String.split_on_char '/' dst)
    (String.split_on_char '/' src)

let%expect_test "path_to" =
  Stdio.printf "%s\n" @@ path_to "a/b/c/d" "a/e/f";
  [%expect {| ../../b/c/d |}]

let%expect_test "path_to_short_dst" =
  Stdio.printf "%s\n" @@ path_to "a" "a/e/f";
  [%expect {| ../../ |}]

let%expect_test "path_to_short_src" =
  Stdio.printf "%s\n" @@ path_to "a/b/c/d" "a";
  [%expect {| b/c/d |}]

(** [default_bs] is the default block size for
    channel reads and writes *)
let default_bs = 4096

type buffer = {
  buf : bytes;
  size : int;
  used : int;
  rd_cnt : int;
  wr_cnt : int;
}

(** [read] upgrades the interface to [In_channel.input]
 * to accept a buffer, max size and used count; it
 * returns an option with the same tuple
 *)
let read channel x =
  let { buf; size; used; rd_cnt; _ } = x in
  let c =
    In_channel.input channel buf used (size - used)
  in
  if c > 0 then
    Some { x with used = c; rd_cnt = rd_cnt + c }
  else None

(** [write] upgrades the interface to [Out_channel.output]
 * to accept and return a buffer, max size, and used 
 * count. Since we are consuming data in the buffer,
 * the used count is set to 0. *)
let write channel x =
  let { buf; used; wr_cnt; _ } = x in
  let _ = Out_channel.output channel buf 0 used in
  { x with used = 0; wr_cnt = wr_cnt + used }

let rec copy_channel rd wr buf =
  match rd buf with
  | None -> ()
  | Some x ->
      let y = wr x in
      copy_channel rd wr y

let copy_file_by_name f1 f2 =
  let b =
    {
      buf = Bytes.create default_bs;
      size = default_bs;
      used = 0;
      rd_cnt = 0;
      wr_cnt = 0;
    }
  in
  In_channel.with_open_bin f1 (fun ic ->
      Out_channel.with_open_bin f2 (fun oc ->
          copy_channel (read ic) (write oc) b))

let copy_file_to_dir file dir =
  let dest = Filename.concat dir file in
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
        | h :: rest -> Filename.concat h d :: h :: rest)
      []
    @@ String.split_on_char '/' p
  in
  List.iter (fun d ->
      Unix.(
        match mkdir d 0o700 with
        | exception Unix_error (EEXIST, _, _) -> ()
        | exception e -> raise e
        | _ -> ()))
  @@ List.rev dirs_to_make

let dir_to_seq path =
  Unix.opendir path
  |> Seq.unfold (fun h ->
         match Unix.readdir h with
         | exception End_of_file ->
             Unix.closedir h;
             None
         | exception e ->
             Unix.closedir h;
             raise e
         | x -> Some (x, h))
  |> Seq.filter (fun x -> x <> "." && x <> "..")
  |> Seq.map (fun x -> Filename.concat path x)

let rec find filter dirs =
  dirs |> List.to_seq |> Seq.map dir_to_seq
  |> Seq.concat
  |> Seq.flat_map (fun x ->
         match Unix.stat x with
         | { st_kind = S_DIR; _ } ->
             Seq.cons x (find filter [ x ])
         | _ -> Seq.return x)
  |> Seq.filter filter
