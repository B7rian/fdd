module Make (Ord : Stdlib.Map.OrderedType) = struct
  include Stdlib.Map.Make (Ord)

  (** Given a function and a list of possible inputs,
 * [of_results] produces a map that gives 
 * a list of the inputs of [f] that produce the 
 * given output
 *)
  let of_results f l =
    List.fold_left
      (fun a x -> add_to_list (f x) x a)
      empty l

  let fprintf channel fmt map =
    iter
      (fun k v ->
        List.iter
          (fun x -> Printf.fprintf channel fmt k x)
          v)
      map
end
