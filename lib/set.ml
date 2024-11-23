module Make (Ord : Stdlib.Set.OrderedType) = struct
  include Stdlib.Set.Make (Ord)

  let of_results f l =
    List.fold_left (fun a x -> add (f x) a) empty l
end
