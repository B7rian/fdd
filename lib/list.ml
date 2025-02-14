include Stdlib.List

let apply fl x = Stdlib.List.map (fun f -> f x) fl
