(** Filesystem operations *)

(* val cp : string list -> string -> r list *)
(** [cp srcs... dest] copies a list of source files to
    a destination directory *)

val copy_file_to_dir : string -> string -> unit
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
