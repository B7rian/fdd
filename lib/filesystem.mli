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

(* Filesystem operations *)
val is_dir : string -> bool
val is_file : string -> bool
val is_symlink : string -> bool
val file_size : string -> int
val file_size_opt : string -> int option

val clone_file : string -> string list -> unit
(** [clone_file s ds] copies file [s] to each
    destination in [ds] where each [d] in [ds] is a
    path including filename. Makes directories as
    necessary *)

val symlink_file : string -> string -> unit
(** [symlink_file t l] creates a symlink called [l]
    that points to [t]. [t] must be a file and the path
    up to [l] must exist. Different than Unix.symlink
    in that both args should be eithr absolute or
    relative to the same dir; the relative path from l
    to t is computed so that the resulting link works
*)

val symlink_many :
  (string -> string -> unit -> unit) ->
  string list ->
  string ->
  unit
(** [symlink_many cb xs y] creates all links in xs and
    makes them point to y. Makes directories as
    necessary and calls [cb x y] after each symlink is
    made*)

val symlink_many_opt :
  (string -> string -> unit -> unit) ->
  string list ->
  string ->
  unit option

val mkdirs : string -> string
(** [mkdirs p] creates all the directories in path [p]
    similar to mkdir -p *)

val in_dir : string -> string -> bool
(** [in_dir a b] returns true if b is underneath a in *
    the directory tree. Both a and b must exist *)

val dir_to_seq : string -> string Seq.t
(** [dir_to_seq path] creates a sequence that returns a
    list of files in the directory at [path]. Throws an
    exception if path is not a directory. *)

val find :
  ?is_dir:(string -> bool) ->
  (string -> bool) ->
  string list ->
  string Seq.t
(** [find is_dir filter paths] recursively finds files
    and stuff in [paths] and produces a sequence of
    them for which [filter] returns [true]. Any
    filesystem item in [paths] that passes [filter] is
    returned in the sequence. [is_dir] is used to
    identify subdirectories and can be overridden to
    provide different error handling behavior. *)

val ue_to_opt : ('a -> 'b) -> 'a -> 'b option
(** [ue_to_opt f x] calls f with x and captures
    [Unix_error]s that may be related to just the
    current file, notifies the Ui, and returns an
    Option.none. This allows the caller to move to the
    next file and continue processing if it wants to *)
