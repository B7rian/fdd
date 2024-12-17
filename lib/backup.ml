(** [backup] implements the backup use case *)

module Intset = Set.Make (Int)
module Stringmap = Map.Make (String)
module FS = Filesystem.Make (Ui)
module D = Digest.Make (FS)

(** [sp] stands for Swap Parameters *)
let sp f a b = f b a

(** [find_next_dir_in] finds a name for a new directory
    in the backup location [dst] that doesn't exist yet
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
    directories, and backup location [dst],
    [src_file_list] builds the list of source files to
    back up. *)
let src_file_list srcs dst =
  FS.find
    (fun x -> FS.is_file x && (not @@ FS.in_dir dst x))
    srcs
  |> List.of_seq

(** [dst_file_list] uses the destination directory and
    set of source file sizes to produce a sequence of
    filenames that might match against one of the
    source files and needs to be considered as a
    possible link target.

    If the filter throws a Unix_error, it returns false
    so that file is not included in the sequence output
    and is ignored by the backup functions. In this
    case, a source file that might have matched the
    problematic dst will be copied, so no data is lost
*)
let dst_file_seq dst src_sizes =
  let dst_filter x =
    (not @@ FS.is_symlink x)
    && FS.is_file x
    && (sp Intset.mem src_sizes @@ FS.file_size x)
  in
  let dst_filter_noexn x =
    FS.ue_to_opt dst_filter x
    |> Option.value ~default:false
  in
  let is_dir_noexn x =
    FS.ue_to_opt FS.is_dir x
    |> Option.value ~default:false
  in
  FS.find ~is_dir:is_dir_noexn dst_filter_noexn [ dst ]

(** [copy_any] tries to copy 1 source file at a time to
    directory [dst] until it succeeds without a
    Unix_error exception being thrown
    @param [srcs] is a list of source file names to try
    @param [dst]
      is the destination directory and will be created
      if it doesn't exist
    @return
      The file (string) from [srcs] that was copied *)
let rec copy_any srcs dst =
  match srcs with
  | [] -> Option.none
  | x :: tl -> (
      let copy1 y =
        Filename.concat dst y
        |> Filename.dirname |> FS.mkdirs
        |> FS.copy_file_to_dir y
      in
      match FS.ue_to_opt copy1 x with
      | Option.None -> copy_any tl dst
      | _ -> Option.some x)

(** [backup_old_files] finds files that have been
    previously backed up and makes symlinks in the new
    backup for them (pointing at the old data).

    If anything goes wrong the source file list is not
    updated so the files are seen by [backup_new_files]
    and there is no data loss

    @param [backup_dir]
      is the dir that the new backup is going into.
    @param [dst_seq]
      is a sequence of filenames that have been
      previously backed up.
    @param [src_map]
      is a map of sha256sums to lists of source files
      that have that signature.

    @return
      a map containing sha256sums and filenames that
      did not appear in any previous backup. *)
let backup_old_files backup_dir dst_seq src_map =
  let open Option.Syntax in
  Seq.fold_left
    (fun a x ->
      let x_sha = D.sha256sum_opt x in
      x_sha
      >>= sp Stringmap.find_opt a
      >|= List.map (Filename.concat backup_dir)
      >|= sp FS.symlink_many_opt x
      <*> x_sha >|= snd
      >|= sp Stringmap.remove a
      |> Option.value ~default:a)
    src_map dst_seq

(** [backup_new_files] backs up files that aren't found
    in any previous backup, as listed in [src_map].
    When multiple files with the same signature are
    found, makes 1 actual copy to the backup and
    symlinks the rest

    @return
      A map containing hashes and files to put in the
      [checksums] file, currently the whole [src_map]
*)
let backup_new_files backup_dir src_map =
  let open Option.Syntax in
  Stringmap.iter
    (fun _ l ->
      (let* copied_file = copy_any l backup_dir in
       let link_srcs =
         List.filter (fun y -> y <> copied_file) l
         |> List.map (Filename.concat backup_dir)
       in
       let link_target =
         Filename.concat backup_dir copied_file
       in
       FS.symlink_many_opt link_srcs link_target)
      |> Option.value ~default:())
    src_map

(** The algorithm for [backup_files] is something like
    this:

    + Create a set of src sizes to filter against. This
      just keeps us from running sha256sum on files
      that won't match anyway.
    + For each file (not symlink) already in dst, if
      the size is in the src size set, compute the sha
      of dst. If it matches, symlink and delete file
      from src map.
    + Anything left in the src map did not have a match
      in dst and needs to be copied unless its a
      duplicate in the src, in which case it should be
      symlinked

    Step 2 is mostly in [backup_old_files] Step 3 is in
    [backup_new_files]

    Step 1 is below. If we can't get the size of a
    file, it won't be represented in the source size
    set and we'll copy it later rather than making it a
    link. This is desirable when we're backing up from
    fishy source media. *)
let backup_files src_map dst backup_dir =
  let src_sizes =
    src_map |> Stringmap.bindings
    |> List.map (fun (_, x) -> x)
    |> List.concat
    |> Intset.of_results FS.file_size_opt
  in
  let dst_seq = dst_file_seq dst src_sizes in
  let _ =
    src_map
    |> backup_old_files backup_dir dst_seq
    |> backup_new_files backup_dir
  in
  src_map

(** [dump_checksums] creates a file in the given dir
    with the backed-up file checksums in it, as found
    in [map] *)
let dump_checksums dir map =
  Out_channel.with_open_text
    (Filename.concat dir "checksums") (fun oc ->
      Stringmap.fprintf oc "%s  %s\n" map)

(** [backup] glues it all together and interfaces to
    the outside world

    Error handling will work like this
    + If setup or cleanup fails, its a show-stopper for
      backing up any files. We can catch these exns in
      the [backup] function and tell the user
    + Errors with individual files shouldn't keep other
      files from being backed up. These exns should be
      caught in the loop(s) in [backup_files] so they
      don't make it to the top [try] and abort the
      backup *)
let backup srcs dst =
  let backup_dir, src_hashmap =
    Unix.handle_unix_error
      (fun () ->
        let _ = FS.mkdirs dst in
        ( dst |> find_next_dir_in |> FS.mkdirs,
          src_file_list srcs dst
          |> Stringmap.of_results D.sha256sum ))
      ()
  in
  let file_map =
    backup_files src_hashmap dst backup_dir
  in
  Unix.handle_unix_error
    (dump_checksums backup_dir)
    file_map;
  `Ok ()
