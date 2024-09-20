(** Filesystem operations *)

type r = Ok of string | Error of string * string

(* val cp : string list -> string -> r list *)
(** [cp srcs... dest] copies a list of source files to
    a destination directory *)

val copy_file_to_dir : string -> string -> r
(** [copy_file_to_dir f d] copies file [f] into
    directory [d] *)

