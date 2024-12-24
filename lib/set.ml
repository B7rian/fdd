module Make (Ord : Stdlib.Set.OrderedType) = struct
  include Stdlib.Set.Make (Ord)

  let of_results f l =
    List.fold_left
      (fun a x ->
        match f x with Some y -> add y a | None -> a)
      empty l
end
