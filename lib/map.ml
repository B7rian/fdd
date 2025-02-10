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

    Each key has a skip count that determines how many
    times [skip_find] has to be called on it before it
    is actually found. The default skips are set by
    [empty] sometimes via [of_results] and are the same
    for every key. This skip concept implementation is
    not really complete. Shortcomings include
    + If [skip_find] has decremented the skip count for
      a key and another value is added, the skip count
      for that key remains the same
    + If an entry is removed and tben a new item is
      added with the same key, the skip count is
      restored *)

module Make (Ord : Stdlib.Map.OrderedType) = struct
  module M = Stdlib.Map.Make (Ord)

  type 'a t = int * (int * 'a) M.t
  (** t stores the max skip count and a map, and each
      map entry stores the skip count for that key with
      the data *)

  (** [empty] works like [Stdlib.empty] but stoes the
      max skip count, which defaults to 0 if not given
  *)
  let empty ?(max_skips = 0) () = (max_skips, M.empty)

  (** [add_to_list] sets the skip count to max for new
      keys, or preserves it for existing keys *)
  let add_to_list k v (max_skips, m) =
    let new_v_tuple =
      match M.find_opt k m with
      | None -> (max_skips, [ v ])
      | Some (skips, l) -> (skips, v :: l)
    in
    (max_skips, M.add k new_v_tuple m)

  (** [remove] takes an entry out of the map *)
  let remove k (max_skips, m) =
    (max_skips, M.remove k m)

  let find_opt k (_, m) =
    match M.find_opt k m with
    | None -> None
    | Some (_, l) -> Option.some l

  let skips_opt k (_, m) =
    match M.find_opt k m with
    | None -> None
    | Some (s, _) -> Option.some s

  let skip_find k ((s, m) as x) =
    match M.find_opt k m with
    | None -> `None x
    | Some (skips, l) ->
        if skips > 0 then
          `None (s, M.add k (skips - 1, l) m)
        else `Some l

  let bindings (_, m) =
    m |> M.bindings
    |> List.map (fun (k, (_, v)) -> (k, v))

  let iter f x =
    x |> bindings |> List.iter (fun (k, v) -> f k v)

  let iter_with_skips f (_, m) = M.iter f m

  (** Given a function and a list of possible inputs,
      [of_results] produces a map that gives a list of
      the inputs to [f] that produce the given output
  *)
  let of_results ?(max_skips = 0) f l =
    List.fold_left
      (fun a x -> add_to_list (f x) x a)
      (empty ~max_skips ())
      l

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
