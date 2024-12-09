(**
   [Errlogger] is a monad that carries an error log.
   Both fatal and non-fatal errors can be represented.
   Useful when performing commutative updates to a data
   type and you want it to keep going to process other
   items if a failure occurs on an earlier item. The
   [Errlogger] contains the data type in a valid state,
   unlike option and result which discard it, so
   processing can continue
*)

type err_t = Exn of exn | Msg of string
type 'a t = { thing : 'a option; errs : err_t list }

let some x = { thing = Option.some x; errs = [] }

let none_exn e =
  { thing = Option.none; errs = [ Exn e ] }

let none_err s =
  { thing = Option.none; errs = [ Msg s ] }

let of_option o = { thing = o; errs = [] }
let add_exn x e = { x with errs = Exn e :: x.errs }
let add_err x s = { x with errs = Msg s :: x.errs }
let get x = x.thing
let get_errs x = x.errs

(** In [map], [f] is not aware of [Errlogger] so we
      wrap [thing] in a [Errlogger] to return it.
      [f] should throw exceptions on errors which
      will be captured and logged *)
let map f x =
  try
    let f_thing = Option.map f x.thing in
    { x with thing = f_thing }
  with e -> { x with errs = Exn e :: x.errs }

(** [map_fatal] is like [map] but an exception
 * is considered a fatal error, so we internally
 * set our data value to [None]
 *)
let map_fatal f x =
  try
    let f_thing = Option.map f x.thing in
    { x with thing = f_thing }
  with e ->
    { thing = Option.none; errs = Exn e :: x.errs }

(** [bind] applies a function to [Errlogger] that
      takes a [thing]s value and returns another
      [Errlogger]. [f] can throw exceptions or log errors
      in its returned [Errlogger].

    Note that there's no benefit to using [Option.bind]
    (or maybe we can't even if we tried) because [f] will
    return an [Errlogger] 
    *)
let bind x f =
  match x.thing with
  | None -> x
  | Some v -> (
      try
        let f_v = f v in
        { f_v with errs = f_v.errs @ x.errs }
      with e -> { x with errs = Exn e :: x.errs })

(** [bind_fatal] is like [bind] but an exception
 * is considered a fatal error, so we internally
 * set our data value to [None]
 *)
let bind_fatal x f =
  match x.thing with
  | None -> x
  | Some v -> (
      try
        let f_v = f v in
        { f_v with errs = f_v.errs @ x.errs }
      with e ->
        { thing = Option.none; errs = Exn e :: x.errs }
      )

(** [prod] combines 2 [Errlogger] into a product form
 *)
let prod x y =
  match (x.thing, y.thing) with
  | Some a, Some b ->
      {
        thing = Option.some (a, b);
        errs = x.errs @ y.errs;
      }
  | _ ->
      { thing = Option.none; errs = x.errs @ y.errs }

(** [Syntax] provides operator bindings that consider
 * exceptions as non-fatal errors.
 * Operators from http://jobjo.github.io/2019/04/24/ocaml-has-some-new-shiny-syntax.html
 *)
module Syntax = struct
  let ( let+ ) o f = map f o
  let ( and+ ) = prod
  let ( let* ) = bind
  let ( <$> ) = map
  let ( <*> ) = prod
  let ( >>= ) = bind
end

(** [Syntax_Fatal] provides operator bindings that consider
 * exceptions as fatal errors.
 * Operators from http://jobjo.github.io/2019/04/24/ocaml-has-some-new-shiny-syntax.html
 *)
module Syntax_Fatal = struct
  let ( let+ ) o f = map_fatal f o
  let ( and+ ) = prod
  let ( let* ) = bind_fatal
  let ( <$> ) = map_fatal
  let ( <*> ) = prod
  let ( >>= ) = bind_fatal
end
