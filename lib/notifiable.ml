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

(** [Notifiable] is an interface to receive events *
    that occur while the program is running. This *
    exists to abstract the UI but coukd be used for *
    statistics and debug, too *)

module type S = sig
  type e =
    | START_COPY of string list
    | COPY_PROGRESS of string list * int
    | FINISH_COPY of string list
    | START_LINK of string
    | FINISH_LINK of string
    | START_VERIFY of string
    | VERIFY_PROGRESS of string * int
    | FINISH_VERIFY of string
    | FOUND_FILE of string
    | FOUND_DIR of string
    | UNIX_ERROR of (Unix.error * string * string)

  val notify : e -> unit
end

module IgnoreNotifications = struct
  (** [IgnoreNotifications] receives notifications *
      for a UI but doesnt do anything with them *)

  type e =
    | START_COPY of string list
    | COPY_PROGRESS of string list * int
    | FINISH_COPY of string list
    | START_LINK of string
    | FINISH_LINK of string
    | START_VERIFY of string
    | VERIFY_PROGRESS of string * int
    | FINISH_VERIFY of string
    | FOUND_FILE of string
    | FOUND_DIR of string
    | UNIX_ERROR of (Unix.error * string * string)

  let notify _ = ()
end
