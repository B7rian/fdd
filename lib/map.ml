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

module Make (Ord : Stdlib.Map.OrderedType) = struct
  module M = Stdlib.Map.Make (Ord)

  type 'a t = int * (int * 'a) M.t

  let empty max_hp = (max_hp, M.empty)

  let add_to_list k v (max_hp, m) =
    let new_v_tuple =
      match M.find_opt k m with
      | None -> (max_hp, [ v ])
      | Some (hp, l) -> (hp, v :: l)
    in
    (max_hp, M.add k new_v_tuple m)

  let remove k (max_hp, m) = (max_hp, M.remove k m)

  let weak_remove k (max_hp, m) =
    match M.find_opt k m with
    | None -> (max_hp, m)
    | Some (hp, l) when hp > 1 ->
        (max_hp, M.add k (hp - 1, l) m)
    | _ -> remove k (max_hp, m)

  let find_opt k (_, m) =
    match M.find_opt k m with
    | None -> None
    | Some (_, l) -> Option.some l

  let bindings (_, m) =
    m |> M.bindings
    |> List.map (fun (k, (_, v)) -> (k, v))

  let iter f x =
    x |> bindings |> List.iter (fun (k, v) -> f k v)

  (** Given a function and a list of possible inputs, *
      [of_results] produces a map that gives * a list
      of the inputs of [f] that produce the * given
      output *)
  let of_results f l =
    List.fold_left
      (fun a x -> add_to_list (f x) x a)
      (empty 1) l

  let fprintf channel fmt map =
    iter
      (fun k v ->
        List.iter
          (fun x -> Printf.fprintf channel fmt k x)
          v)
      map
end
