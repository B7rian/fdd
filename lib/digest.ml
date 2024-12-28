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

module type S = sig
  val sha256sum : string -> string
  (** [sha256sum n] computes the sha256 sum of the file
      at the given path and name and returns the sum in
      a string *)

  val sha256sum_opt : string -> string option
end

module Make (FS : Filesystem.S) : S = struct
  let sha256sum x =
    try Sha256.file_fast x |> Sha256.to_hex
    with Failure _ ->
      raise
      @@ Unix.Unix_error
           (Unix.ENOENT, "Sha256.file_fast", x)

  let sha256sum_opt x = FS.ue_to_opt sha256sum x
end
