(** [backup] is a collection of files
    and stuff associated with a single invocation
    of the program.

    You can add files to the backup and also query it to
    see if a file is there, but you cant remove them
    through the [vackup] interface.

    When you add a file, it will act on the filesystem.
*)

type t
(** [t] represents the backup *)

val empty : String.t -> (module Filesystem.S) -> t
(** [empty d fs] creates a new empty backup in the given
    dir accessible through filesystem [fs] *)

val add : string -> t -> t Exnlogger.t
(** [add path t] adds a file to the backup *)

val has : string -> t -> bool
(** [has path t] returns true if the backup has the given
    file in it *)

val find_copy : File.t -> t -> File.t option
(** Determines if a copy of the given file already
    exists in the backup or not and returns
    it in an option if it exists, None otherwise *)

val close : t -> t Exnlogger.t
(** [close t] is to be called after all files are
 * added and writes out the checksum file *)
