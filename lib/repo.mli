(** [repo] is a backup repository that contains files
    and stuff

    You can add files to the repo and also query it to
    see if a file is there, but you cant remove them
    through the [repo] interface.

    When you add a file, it will act on the filesystem.
*)

type t
(** [t] represents the repository **)

val empty : String.t -> (module Filesystem.S) -> t
(** [empty d fs] creates a new empty repo in the given
    dir accessible through filesystem [fs] *)

val add : string -> t -> t Exnlogger.t
(** [add path t] adds a file to the repository *)

val has : string -> t -> bool
(** [has path t] returns true if the repo has the given
    file in it *)

val find_copy : File.t -> t -> File.t option
(** Determines if a copy of the given file already
    exists in the repository or not and returns
    it in an option if it exists, None otherwise *)
