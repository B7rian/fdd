(** Abstract data type for a file in the repository

    Does not actually read or write files - use
    other ocaml things for that. This File is
    more about the metadata
*)

type t
(** [t] is an abstract type that represents a file
    in the repository *)

val from_path : String.t -> t
(** [from_path p] creates a new file with the given
    path. This does not make a file on the filesystem,
    just makes a data type for the program to use *)

val path : t -> String.t
(** [path x] returns the String path of the file *)

val hash : t -> String.t
(** [hash x] returns the hash of the file. Will access
    the filesystem if the hash needs to be computed *)

val size : t -> int
(** [size x] returns the number of bytes in [x].
    May access the filesystem to get the file size *)

val same_name : t -> t -> bool
(** [same_name file1 file2] returns true if the given
    files have the same path and filename. This
    function is not smart enough to accurately
    compare absolute and relative paths or different
    relative paths that resolve to the same file *)

val same_data : t -> t -> bool
(** [same_data file1 file2] returns true if the given
    files have the same contents *)
