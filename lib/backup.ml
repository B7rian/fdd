(** [backup] use case *)

module Intset = Set.Make (Int)
module Stringmap = Map.Make (String)
module FS = Filesystem.Make (Ui)

(** Given a function and a list of possible inputs,
 * [reverse_fn_map] produces a map that gives 
 * a list of the inputs of [f] that produce the 
 * given output
 *)
let reverse_fn_map f l =
  List.fold_left
    (fun a x -> Stringmap.add_to_list (f x) x a)
    Stringmap.empty l

let intset_mem_of set x = Intset.mem x set

let symlink_many srcs target =
  List.iter
    (fun x ->
      let _ = Filename.dirname x |> FS.mkdirs in
      FS.symlink_file target x)
    srcs

let backup_dup l dst =
  match l with
  | [] -> ()
  | x :: tl ->
      let dst_file = Filename.concat dst x in
      let _ = Filename.dirname dst_file |> FS.mkdirs in
      let _ = FS.copy_file_to_dir x dst in
      let rest = List.map (Filename.concat dst) tl in
      symlink_many rest dst_file

let dump_checksums map dst =
  Out_channel.with_open_text
    (Filename.concat dst "checksums") (fun oc ->
      Stringmap.iter
        (fun k v ->
          List.iter
            (fun x -> Printf.fprintf oc "%s  %s\n" k x)
            v)
        map)

(* 
 * Create a set of src sizes and map of sha256sum 
 *    to src files with that sum
 * For each file (not symlink) already in dst,  
 *   if the size is in the src size set,
 *     compute the sha of dst 
 *     if match, symlink and delete file from src map
 * Anything left in the src map did not have a 
 *     match in dst and needs to be copied
 *     unless its a duplicate in the src,
 *     in which case it should be symlinked
 * *)

let backup srcs dst =
  let open Exnlogger in
  let src_files =
    FS.find FS.is_file srcs |> List.of_seq
  in
  let files_with_hash =
    reverse_fn_map Digest.sha256sum_file_by_name
      src_files
  in
  let src_sizes =
    List.fold_left
      (fun a x -> Intset.add (FS.file_size x) a)
      Intset.empty src_files
  in
  let dst_filter x =
    (not @@ FS.is_symlink x)
    && FS.is_file x
    && (intset_mem_of src_sizes @@ FS.file_size x)
  in
  let _ = FS.mkdirs dst in
  let _ =
    FS.find dst_filter [ dst ]
    |> Seq.fold_left
         (fun a x ->
           let x_sha =
             Digest.sha256sum_file_by_name x
           in
           match Stringmap.find_opt x_sha a with
           | Some l ->
               symlink_many l x;
               Stringmap.remove x_sha a
           | None -> a)
         files_with_hash
    |> Stringmap.iter (fun _ l -> backup_dup l dst)
  in
  let _ = dump_checksums files_with_hash dst in
  let result = return dst in
  match get_exns result with
  | [] -> `Ok ()
  | r ->
      List.iter
        (fun x ->
          Stdio.printf "%s\n" @@ Printexc.to_string x)
        r;
      `Error (false, "Error copying files")
