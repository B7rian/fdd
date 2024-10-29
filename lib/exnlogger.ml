(**
   [Exnlogger] is a monad that carries an exception log.
   Useful when performing commutative updates to a data
   type and you want it to keep going to process other
   items if a failure occurs on an earlier item. The
   [Exnlogger] contains the data type in a valid state,
   unlike option and result, so processing can continue
*)

type 'a t = { thing : 'a; exns : exn list }
(** [t] has a valid [thing] and a log of all the
    [exns] that operations on [thing] have caused *)

let return x = { thing = x; exns = [] }
let add_error x e = { x with exns = e :: x.exns }
let get x = x.thing
let get_exns x = x.exns

(** In [map], [f] is not aware of [Exnlogger] so we
      wrap [thing] in a [Exnlogger] to return it.
      This map captures exceptions too *)
let map f { thing; exns } =
  match f thing with
  | exception e -> { thing; exns = e :: exns }
  | x -> { thing = x; exns }

(** [bind] applies a function to [Exnlogger] that
      takes a [thing] and returns another [Exnlogger].
      Captures exceptions, too*)
let bind { thing; exns } f =
  match f thing with
  | { thing = new_thing; exns = new_exns } ->
      { thing = new_thing; exns = new_exns @ exns }
  | exception e -> { thing; exns = e :: exns }

let ( >>= ) = bind
