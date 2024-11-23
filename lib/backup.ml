(** [backup] use case *)

module Intset = Set.Make (Int)
module Stringmap = Map.Make (String)
module FS = Filesystem.Make (Ui)

let intset_mem_of set x = Intset.mem x set

(** [backup_dup] backs up a list of files that are all
 * the same 
 *)
let backup_dup l dst =
  match l with
  | [] -> ()
  | x :: tl ->
      let dst_file = Filename.concat dst x in
      let _ = Filename.dirname dst_file |> FS.mkdirs in
      let _ = FS.copy_file_to_dir x dst in
      let rest = List.map (Filename.concat dst) tl in
      FS.symlink_many rest dst_file

(** [backup_name] finds a name for a new directory in
 * the backup location [dst] that doesn't exist yet
 *)
let backup_name dst =
  let rec find_next_dir n =
    let name =
      Filename.concat dst @@ Printf.sprintf "%i" n
    in
    let open Unix in
    match FS.is_dir name with
    | exception Unix_error (ENOENT, _, _) -> name
    | _ -> find_next_dir (n + 1)
  in
  find_next_dir 0

(** Given the backup source list, which may contain
 * directories, and backup location [dst],
 * [src_file_list] builds the list of source files to back up
 * *)
let src_file_list srcs dst =
  FS.find
    (fun x -> FS.is_file x && (not @@ FS.in_dir dst x))
    srcs
  |> List.of_seq

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

let backup_files srcs dst backup_dir =
  let src_files = src_file_list srcs dst in
  let src_hashes =
    Stringmap.of_results Digest.sha256sum_file_by_name
      src_files
  in
  let src_sizes =
    Intset.of_results FS.file_size src_files
  in
  let dst_filter x =
    (not @@ FS.is_symlink x)
    && FS.is_file x
    && (intset_mem_of src_sizes @@ FS.file_size x)
  in
  let _ =
    FS.find dst_filter [ dst ]
    |> Seq.fold_left
         (fun a x ->
           let x_sha =
             Digest.sha256sum_file_by_name x
           in
           match Stringmap.find_opt x_sha a with
           | Some l ->
               FS.symlink_many
                 (List.map
                    (Filename.concat backup_dir)
                    l)
                 x;
               Stringmap.remove x_sha a
           | None -> a)
         src_hashes
    |> Stringmap.iter (fun _ l ->
           backup_dup l backup_dir)
  in
  src_hashes

(** [dump_checksums] creates a file in the given dir
 * with the backed-up file checksums in it, as found in
 * [map]
 *)
let dump_checksums map dir =
  Out_channel.with_open_text
    (Filename.concat dir "checksums") (fun oc ->
      Stringmap.fprintf oc "%s  %s\n" map)

let backup srcs dst =
  let open Exnlogger in
  let backup_dir = backup_name dst in
  FS.mkdirs backup_dir;

  let src_hashmap = backup_files srcs dst backup_dir in

  dump_checksums src_hashmap backup_dir;
  let result = return dst in

  match get_exns result with
  | [] -> `Ok ()
  | r ->
      List.iter
        (fun x ->
          Stdio.printf "%s\n" @@ Printexc.to_string x)
        r;
      `Error (false, "Error copying files")
