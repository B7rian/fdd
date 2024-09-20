(** [repo] is a backup repository that contains files
    and stuff

    You can add files to the repo and also query it to
    see if a file is there, but you cant remove them
    through the [repo] interface.

    When you add a file, it will act on the filesystem.
*)

type t
(** [t] represents the repository **)

val empty : String.t -> t
(** [empty d] creates a new empty repo in the given
    dir *)

val add : t -> File.t -> t
(** [add t f] adds a file to the repository *)

val has : t -> File.t -> bool
(** [has t f] returns true if the repo has the given
    file in it *)
