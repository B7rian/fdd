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

(** [map] is a wrapper around [Stdlib.map] that adds
    functionality to support our use case.

    Each key has "hit points" that determine how many
    time [weak_remove] has to be called in it before it
    is actually removed. The default hit points are set
    by [empty] sometimes via [of_results] and are the
    same for every key. This hit point concept
    implementation is not really complete. Shortcomings
    include
    + If weak_remove has decremented the hit points for
      a key and anither value is added, the hit points
      remain the same
    + However if weak_remove is called enough to remove
      the key and a new value is adder, the full hit
      points are restored *)

module Make (Ord : Stdlib.Map.OrderedType) = struct
  module M = Stdlib.Map.Make (Ord)

  type 'a t = int * (int * 'a) M.t
  (** t stores the max hit points and a map, and each
      map entry stores the hit points for that key with
      the data *)

  (** [empty] works like [Stdlib.empty] but stoes the
      max hit points, which defaults to 1 if not given
  *)
  let empty ?(max_hp = 1) () = (max_hp, M.empty)

  (** [add_to_list] sets the hit points to max for new
      keys, or oreserves it fir existing keys *)
  let add_to_list k v (max_hp, m) =
    let new_v_tuple =
      match M.find_opt k m with
      | None -> (max_hp, [ v ])
      | Some (hp, l) -> (hp, v :: l)
    in
    (max_hp, M.add k new_v_tuple m)

  (** [remove] takes an entry out of the map regardless
      of rhe remaining hit points *)
  let remove k (max_hp, m) = (max_hp, M.remove k m)

  (** [weak_remove] removes an entry from the map only
      when its hit points are 0; it decrements the
      entry's hit points otherwise *)
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

  (** Given a function and a list of possible inputs,
      [of_results] produces a map that gives a list of
      the inputs to [f] that produce the given output
  *)
  let of_results ?(max_hp = 1) f l =
    List.fold_left
      (fun a x -> add_to_list (f x) x a)
      (empty ~max_hp ()) l

  (** [fprintf] prints the map contents to the given
      channel using [fmt]. [fmt] needs to include 2 %s
      format specifiers; this code needs improvement *)
  let fprintf channel fmt map =
    iter
      (fun k v ->
        List.iter
          (fun x -> Printf.fprintf channel fmt k x)
          v)
      map
end
