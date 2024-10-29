(** [fileset] is a collection of files
    and stuff 

    You can add files to the set and also query it to
    see if a file is there, but you cant remove them
    through the [fileset] interface.
*)

module type S = sig
  type t
  (** [t] represents the set of files *)

  val empty : String.t -> (module Filesystem.S) -> t
  (** [empty d fs] creates a new empty set in the given
    dir accessible through filesystem [fs] *)

  val add : string -> t -> t Exnlogger.t
  (** [add path t] adds a file to the set *)

  val has : string -> t -> bool
  (** [has path t] returns true if the set has the given
    file in it *)

  val find_copy : File.t -> t -> File.t option
  (** Determines if a copy of the given file already
    exists in the set or not and returns
    it in an option if it exists, None otherwise *)

  val close : t -> t Exnlogger.t
  (** [close t] is to be called after all files are
 * added *)
end
