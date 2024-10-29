(** [Notifiable] is an interface to receive events
 * that occur while the program is running. This 
 * exists to abstract the UI but coukd be used for
 * statistics and debug, too
 *)

module type S = sig
  type e =
    | START_COPY of string
    | COPY_PROGRESS of string * int
    | FINISH_COPY of string
    | START_LINK of string
    | FINISH_LINK of string
    | START_VERIFY of string
    | VERIFY_PROGRESS of string * int
    | FINISH_VERIFY of string
    | FOUND_FILE of string
    | FOUND_DIR of string

  val notify : e -> unit
end

module IgnoreNotifications = struct
  (** [IgnoreNotifications] receives notifications
   * for a UI but doesnt do anything with them
   *)

  type e =
    | START_COPY of string
    | COPY_PROGRESS of string * int
    | FINISH_COPY of string
    | START_LINK of string
    | FINISH_LINK of string
    | START_VERIFY of string
    | VERIFY_PROGRESS of string * int
    | FINISH_VERIFY of string
    | FOUND_FILE of string
    | FOUND_DIR of string

  let notify _ = ()
end
