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
  | UNIX_ERROR of (Unix.error * string * string)

open Printf

let notify = function
  | START_COPY f -> eprintf "copy %s...(0)" f
  | COPY_PROGRESS (f, i) ->
      eprintf "\rcopy %s...(%i)" f i
  | FINISH_COPY _f -> eprintf " done\n"
  | START_LINK f -> eprintf "link %s...(0)" f
  | FINISH_LINK f -> eprintf "\rlink %s...done\n" f
  | START_VERIFY f -> eprintf "hash %s...(0)" f
  | VERIFY_PROGRESS (f, i) ->
      eprintf "\rhash %s...(%i)" f i
  | FINISH_VERIFY _f -> eprintf " done\n"
  | UNIX_ERROR (e, f, p) ->
      eprintf "Error in %s on %s: %s\n" f p
      @@ Unix.error_message e
  | _ -> ()
