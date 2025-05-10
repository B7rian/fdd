include Stdlib.List

(** [apply fl x] calls each function in function list
    [fl] with parameter [x] and returns a list with the
    result from each call *)
let apply fl x = Stdlib.List.map (fun f -> f x) fl
