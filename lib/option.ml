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
