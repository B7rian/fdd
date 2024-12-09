(** [backup] use case *)

module Intset = Set.Make (Int)
module Stringmap = Map.Make (String)
module FS = Filesystem.Make (Ui)

(** [sp] stands for Swap Parameters *)
let sp f a b = f b a

(** [find_next_dir_in] finds a name for a new directory
 * in the backup location [dst] that doesn't exist yet
 *)
let find_next_dir_in dst =
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
 * [src_file_list] builds the list of source files to
 * back up.
 * *)
let src_file_list srcs dst =
  FS.find
    (fun x -> FS.is_file x && (not @@ FS.in_dir dst x))
    srcs
  |> List.of_seq

(** [dst_file_list] uses the destination directory and a
 * set of source file sizes to produce a sequence of file
 * names that might match against one of the source files
 * and needs to be considered as a possible link target.
 *
 * If the filter throws a Unix_error, it returns false
 * so that file is not included in the sequence output
 * and is ignored by the backup functions. In this
 * case, a source file that might have matched
 * the problematic dst will be copied, so no data
 * is lost  
 *)
let dst_file_seq dst src_sizes =
  let dst_filter x =
    try
      (not @@ FS.is_symlink x)
      && FS.is_file x
      && (sp Intset.mem src_sizes @@ FS.file_size x)
    with Unix.Unix_error (e, f, p) ->
      Ui.notify @@ Ui.UNIX_ERROR (e, f, p);
      false
  in
  let is_dir_noexn x =
    try FS.is_dir x
    with Unix.Unix_error (e, f, p) ->
      Ui.notify @@ Ui.UNIX_ERROR (e, f, p);
      false
  in
  FS.find ~is_dir:is_dir_noexn dst_filter [ dst ]

(** [file_size_opt] is a safe wrapper around
 * [Filesystem.file_size] that returns an option instead
 * of throwing Unix_errors. Non-Unix_errors are not
 * captured *)
let file_size_opt x =
  try Option.some @@ FS.file_size x
  with Unix.Unix_error (e, f, p) ->
    Ui.notify @@ Ui.UNIX_ERROR (e, f, p);
    Option.none

(** [sha256sum_opt] is a safe wrapper around
 * [Digest.sha256sum_file_by_name] that returns an option
 * instead of throwing Failure.  Other exceptions are not
 * captured *)
let sha256sum_opt x =
  try Option.some @@ Digest.sha256sum_file_by_name x
  with Unix.Unix_error (e, f, p) ->
    Ui.notify @@ Ui.UNIX_ERROR (e, f, p);
    Option.none

(** [symlink_many_opt] is a safe wrapper around
 * [Filesystem.symlink_many] that returns an option
 * instead of throwing Unix_errors. Non-Unix_errors are
 * not captured *)
let symlink_many_opt x y =
  try Option.some @@ FS.symlink_many x y
  with Unix.Unix_error (e, f, p) ->
    Ui.notify @@ Ui.UNIX_ERROR (e, f, p);
    Option.none

(*
 * [backup_old_files] finds files that have been
 * previously backed up and makes symlinks in the
 * new backup for them (pointing at the old data).
 * It returns a map containig sha256sums and filenames
 * that did not appear in any previous backup. 
 *
 * If anything goes wrong the source file list is not
 * updated so the files are seen by [backup_new_files]
 * and there is no data loss  
 *
 * @param [backup_dir] is the dir that the new backup is
 * going into.  
 * @param [dst_seq] is a sequence of filenames that have
 * been previously backed up. 
 * @param [src_map] is a map of sha256sums to lists of
 * source files that have that signature.
 * *)
let backup_old_files backup_dir dst_seq src_map =
  let open Option.Syntax in
  Seq.fold_left
    (fun a x ->
      let x_sha = sha256sum_opt x in
      x_sha
      >>= sp Stringmap.find_opt a
      >|= List.map (Filename.concat backup_dir)
      >|= sp symlink_many_opt x
      <*> x_sha >|= snd
      >|= sp Stringmap.remove a
      |> Option.value ~default:a)
    src_map dst_seq

(** [backup_new_files] backs up files that aren't found
 * in any previous backup, as listed in [src_map]. 
 * When multiple files with the same signature are
 * found, makes 1 actual copy to the backup and
 * symlinks the rest
 *)
let backup_new_files backup_dir src_map =
  Stringmap.iter
    (fun _ l ->
      match l with
      | [] -> ()
      | x :: tl ->
          let dst_file =
            Filename.concat backup_dir x
          in
          let _ =
            Filename.dirname dst_file |> FS.mkdirs
          in
          let _ = FS.copy_file_to_dir x backup_dir in
          let rest =
            List.map (Filename.concat backup_dir) tl
          in
          FS.symlink_many rest dst_file)
    src_map

(* 
 * The algorithm for backup_files is something
 * like this:
 *
 * 1. Create a set of src sizes to filter against
 * 2. For each file (not symlink) already in dst,  
 *    if the size is in the src size set,
 *      compute the sha of dst 
 *      if match, symlink and delete file from src map
 * 3. Anything left in the src map did not have a 
 *    match in dst and needs to be copied
 *    unless its a duplicate in the src,
 *    in which case it should be symlinked
 *
 * Step 2 is mostly in [backup_old_files]
 * Step 3 is in [backup_new_files]
 *
 * Step 1 is below.  If we can't get the size of a file,
 * it won't be represented in the source size set and
 * we'll copy it later rather than making it a link. This
 * is desirable when we're backing up from fishy source
 * media.
 *)
let backup_files src_map dst backup_dir =
  let src_sizes =
    src_map |> Stringmap.bindings
    |> List.map (fun (_, x) -> x)
    |> List.concat
    |> Intset.of_results file_size_opt
  in
  let dst_seq = dst_file_seq dst src_sizes in
  let _ =
    src_map
    |> backup_old_files backup_dir dst_seq
    |> backup_new_files backup_dir
  in
  []

(** [dump_checksums] creates a file in the given dir
 * with the backed-up file checksums in it, as found in
 * [map]
 *)
let dump_checksums map dir =
  Out_channel.with_open_text
    (Filename.concat dir "checksums") (fun oc ->
      Stringmap.fprintf oc "%s  %s\n" map)

(** [backup] glues it all together and interfaces to the
 * outside world 
 *
 * Error handling will work like this:
 * 1. If setup or cleanup fails, its a show-stopper for
 * backing up any files. We can catch these exns
 * in the [backup] function and tell the user
 * 2. Errors with individual files shouldn't keep
 * other files from being backed up. These exns
 * should be caught in the loop(s) in [backup_files]
 * so they don't make it to the top [try] and abort
 * the backup
 * 
 * *)
let backup srcs dst =
  let backup_dir, src_hashmap =
    Unix.handle_unix_error
      (fun () ->
        let _ = FS.mkdirs dst in
        ( dst |> find_next_dir_in |> FS.mkdirs,
          src_file_list srcs dst
          |> Stringmap.of_results
               Digest.sha256sum_file_by_name ))
      ()
  in
  let errors =
    backup_files src_hashmap dst backup_dir
  in
  dump_checksums src_hashmap backup_dir;

  match errors with
  | [] -> `Ok ()
  | exns ->
      List.iter
        (fun x ->
          Stdio.printf "%s\n" @@ Printexc.to_string x)
        exns;
      `Error (false, "Error copying files")
