(*
   Copyright 2024 Brian W Hughes

   Licensed under the Apache License, Version 2.0 (the
   "License"); you may not use this file except in
   compliance with the License.  You may obtain a copy
   of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in
   writing, software distributed under the License is
   distributed on an "AS IS" BASIS, WITHOUT WARRANTIES
   OR CONDITIONS OF ANY KIND, either express or
   implied.  See the License for the specific language
   governing permissions and limitations under the
   License.
*)

(** Filesystem provides a high(er) level interface to
    the underlying file system

    All functions in here signal errors using
    exceptions, usually from the underlying function *)

open Unix

let ue_to_opt f x =
  try Option.some @@ f x
  with
  | Unix_error ((E2BIG as e), f, p)
  | Unix_error ((EACCES as e), f, p)
  | Unix_error ((EBADF as e), f, p)
  | Unix_error ((EBUSY as e), f, p)
  | Unix_error ((EEXIST as e), f, p)
  | Unix_error ((EFBIG as e), f, p)
  | Unix_error ((EINVAL as e), f, p)
  | Unix_error ((EIO as e), f, p)
  | Unix_error ((EMLINK as e), f, p)
  | Unix_error ((ENAMETOOLONG as e), f, p)
  | Unix_error ((ENODEV as e), f, p)
  | Unix_error ((ENOENT as e), f, p)
  | Unix_error ((ENXIO as e), f, p)
  | Unix_error ((EPERM as e), f, p)
  | Unix_error ((EROFS as e), f, p)
  | Unix_error ((EXDEV as e), f, p)
  | Unix_error ((ELOOP as e), f, p)
  | Unix_error ((EOVERFLOW as e), f, p)
  ->
    Printf.eprintf "Error at %s:%s: %s\n" f p
    @@ Unix.error_message e;
    Option.none

let is_dir p =
  match stat p with
  | { st_kind = S_DIR; _ } -> true
  | _ -> false

let is_file p =
  match stat p with
  | { st_kind = S_REG; _ } -> true
  | _ -> false

let is_symlink p =
  match lstat p with
  | { st_kind = S_LNK; _ } -> true
  | _ -> false

let file_size p =
  let { st_size = c; _ } = stat p in
  c

let file_size_opt p = ue_to_opt file_size p

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
  @@ List.rev dirs_to_make;
  p

let mkdirs_for_files files =
  let open Option.Infix in
  files
  |> List.filter_map (fun x ->
         x |> Filename.dirname |> ue_to_opt mkdirs
         <> Option.some x >|= snd)

let in_dir dir1 dir2 =
  let d1 = Unix.realpath dir1 in
  let d2 = Unix.realpath dir2 in
  String.starts_with ~prefix:d1 d2

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

(* [find_seq] takes paths as a sequence and does all
     the hard work for [find] *)
let rec find_seq is_dir filter paths =
  paths
  |> Seq.map (fun x ->
         if is_dir x then
           Seq.cons x
             (dir_to_seq x |> find_seq is_dir filter)
         else Seq.return x)
  |> Seq.concat |> Seq.filter filter

let find ?(is_dir = is_dir) filter paths =
  find_seq is_dir filter @@ List.to_seq paths

let rec copy_channel rd wrs buf dsts =
  match rd buf with
  | None -> ()
  | Some x ->
      let _ = List.apply wrs x in
      copy_channel rd wrs (Buffer.clear buf) dsts

let clone_file src dsts =
  let dsts_with_dirs = mkdirs_for_files dsts in
  let b = Buffer.empty () in
  In_channel.with_open_bin src (fun ic ->
      let rd = Buffer.read ic in
      Out_channel.with_many_open_bin dsts_with_dirs
        (fun ocs ->
          let wrs = List.map Buffer.write ocs in
          copy_channel rd wrs b dsts))

let symlink_file target link_name =
  let tname = Filename.basename target in
  let tdir = Filename.dirname target in
  let ldir = Filename.dirname link_name in
  let new_tgt =
    Filename.concat (Filename.path_to tdir ldir) tname
  in
  Unix.symlink ~to_dir:false new_tgt link_name

let symlink_many done_cb srcs target =
  let open Portal.Infix in
  List.iter
    (fun x ->
      let _ = Filename.dirname x |> mkdirs in
      symlink_file target x |< done_cb target x)
    srcs

let symlink_many_opt done_cb x y =
  ue_to_opt (symlink_many done_cb x) y
