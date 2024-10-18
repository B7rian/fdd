(** Filesystem operations *)

val is_dir : string -> bool
val is_file : string -> bool
val file_size : string -> int

val path_to : string -> string -> string
(** [path to dst src] finds a relative path from src to
 * dst. Args are in the same order as [symlink].
 * Paths must be absolute, or relative to the
 * same directory (usually the one that the program
 * is running in) *)

val copy_file_to_dir :
  ?report:(int -> unit) -> string -> string -> unit
(** [copy_file_to_dir f d] copies file [f] into
    directory [d] *)

val symlink_file : string -> string -> unit
(** [symlink_file t l] creates a symlink called [l]
    that points to [t]. [t] must be a file *)

val mkdirs : string -> unit
(** [mkdirs p] creates all the directories in path
    [p] similar to mkdir -p *)

val dir_to_seq : string -> string Seq.t
(** [dir_to_seq path] creates a sequence that returns
    a list of files in the directory at [path] *)

val find :
  (string -> bool) -> string list -> string Seq.t
(** [find filter] recursively finds files and stuff in
 * [dirs] and produces a sequence of them for which
 * [filter] returns [true] *)
