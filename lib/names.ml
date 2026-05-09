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

module FS = Filesystem
(** [Names] tells other modules how to name directories
    and files in the backup *)

(** [next_backup_dir dst] finds a name for a new
    directory in the backup location [dst] that doesn't
    exist yet. The returned path has [dst] prepended.
*)
let next_backup_dir dst =
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

(** [checksum_file d] is the path to the checksum file
    in backup directory [d] *)
let checksum_file dir = Filename.concat dir "checksums"

(** [next_copy_dir d] returns the next directory name
    for extra copies of files given the directory name
    for the current set of copies or an empty string if
    no extra cooy directories have been made yet. The
    given name [d] and the returned path do jot have
    the backup directory prepended *)
let next_copy_dir dir =
  if String.equal dir "" then "extra_copies"
  else
    dir
    |> String.split_on_char '_'
    |> List.rev |> List.hd |> int_of_string_opt
    |> Option.value ~default:0
    |> succ |> string_of_int
    |> String.cat "extra_copies_"

(** [backups s d n] creates 2 lists of files to back up
    for source files [s] being backed up to [d] with
    the user having requested [n] copies of the file
    contents and all files in [s] having identical
    contents. The first list is a list of destinations
    for actual copies, and the second is a list of
    symlinks that can point at any of the files in the
    first list *)
let backups srcs dst ncopies =
  let rec add_copies dir acc =
    if List.length acc >= ncopies then acc
    else
      let new_copies =
        List.map
          (Filename.concat dir)
          (List.take (ncopies - List.length acc) srcs)
      in
      add_copies (next_copy_dir dir) (acc @ new_copies)
  in
  let all_copies =
    srcs
    |> add_copies (next_copy_dir "")
    |> List.map (Filename.concat dst)
  in
  ( List.take ncopies all_copies,
    List.drop ncopies all_copies )
