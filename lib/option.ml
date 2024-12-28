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

include Stdlib.Option

let prod x y =
  match (x, y) with
  | Some a, Some b -> Some (a, b)
  | _ -> None

(** [Syntax] provides operator bindings that consider *
    exceptions as non-fatal errors. * Operators from
    http://jobjo.github.io/2019/04/24/ocaml-has-some-new-shiny-syntax.html
*)
module Syntax = struct
  let ( let+ ) o f = map f o
  let ( and+ ) = prod
  let ( let* ) = bind
  let ( >|= ) x f = map f x
  let ( =|< ) = map
  let ( >>= ) = bind
  let ( =<< ) f x = bind x f
  let ( <$> ) = map
  let ( <*> ) = prod
end
