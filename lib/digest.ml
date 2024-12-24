module type S = sig
  val sha256sum : string -> string
  (** [sha256sum n] computes the sha256 sum of the file
      at the given path and name and returns the sum in
      a string *)

  val sha256sum_opt : string -> string option
end

module Make (FS : Filesystem.S) : S = struct
  let sha256sum x =
    try Sha256.file_fast x |> Sha256.to_hex
    with Failure _ ->
      raise
      @@ Unix.Unix_error
           (Unix.ENOENT, "Sha256.file_fast", x)

  let sha256sum_opt x = FS.ue_to_opt sha256sum x
end
