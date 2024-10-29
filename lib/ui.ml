type e =
  | START_COPY of string
  | COPY_PROGRESS of string * int
  | FINISH_COPY of string
  | START_LINK of string
  | FINISH_LINK of string
  | START_VERIFY of string
  | VERIFY_PROGRESS of string * int
  | FINISH_VERIFY of string
  | FOUND_FILE of string
  | FOUND_DIR of string

open Printf

let notify = function
  | START_COPY f -> eprintf "copy %s...(0)" f
  | COPY_PROGRESS (f, i) ->
      eprintf "\rcopy %s...(%i)" f i
  | FINISH_COPY _f -> eprintf " done\n"
  | START_LINK f -> eprintf "link %s...(0)" f
  | FINISH_LINK f -> eprintf "\rlink %s...done\n" f
  | START_VERIFY f -> eprintf "hash %s...(0)" f
  | VERIFY_PROGRESS (f, i) ->
      eprintf "\rhash %s...(%i)" f i
  | FINISH_VERIFY _f -> eprintf " done\n"
  | _ -> ()
