(*
   Copyright 2024 Brian W Hughes

   Licensed under the Apache License, Version 2.0 (the
   "License"); you may not use this file except in
   compliance with the License.  You may obtain a copy
   of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in
   writing, software distributed under the License is
   distributed on an "AS IS" BASIS, WITHOUT WARRANTIES
   OR CONDITIONS OF ANY KIND, either express or
   implied.  See the License for the specific language
   governing permissions and limitations under the
   License.
*)

(** Filesystem provides a high(er) level interface to
    the underlying file system

    All functions in here signal errors using
    exceptions, usually from the underlying function *)

module type S = sig
  (* Filesystem operations *)
  val is_dir : string -> bool
  val is_file : string -> bool
  val is_symlink : string -> bool
  val file_size : string -> int
  val file_size_opt : string -> int option

  val clone_file : string -> string list -> unit
  (** [clone_file s ds] copies file [s] to each
      destination in [ds] where each [d] in [ds] is a
      path including filename *)

  val symlink_file : string -> string -> unit
  (** [symlink_file t l] creates a symlink called [l]
      that points to [t]. [t] must be a file and the
      path up to [l] must exist. Different than
      Unix.symlink in that both args should be eithr
      absolute or relative to the same dir; the
      relative path from l to t is computed so that the
      resulting link works *)

  val symlink_many : string list -> string -> unit
  (** [symlink_many xs y] creates all links in xs and *
      makes them point to y. Makes directories as *
      necessary *)

  val symlink_many_opt :
    string list -> string -> unit option

  val mkdirs : string -> string
  (** [mkdirs p] creates all the directories in path
      [p] similar to mkdir -p *)

  val in_dir : string -> string -> bool
  (** [in_dir a b] returns true if b is underneath a in
      * the directory tree. Both a and b must exist *)

  val dir_to_seq : string -> string Seq.t
  (** [dir_to_seq path] creates a sequence that returns
      a list of files in the directory at [path].
      Throws an exception if path is not a directory.
  *)

  val find :
    ?is_dir:(string -> bool) ->
    (string -> bool) ->
    string list ->
    string Seq.t
  (** [find is_dir filter paths] recursively finds
      files and stuff in [paths] and produces a
      sequence of them for which [filter] returns
      [true]. Any filesystem item in [paths] that
      passes [filter] is returned in the sequence.
      [is_dir] is used to identify subdirectories and
      can be overridden to provide different error
      handling behavior. *)

  val ue_to_opt : ('a -> 'b) -> 'a -> 'b option
  (** [ue_to_opt f x] calls f with x and captures
      [Unix_error]s that may be related to just the
      current file, notifies the Ui, and returns an
      Option.none. This allows the caller to move to
      the next file and continue processing if it wants
      to *)
end

module Make (N : Notifiable.S) : S = struct
  open Unix

  let ue_to_opt f x =
    try Option.some @@ f x
    with
    | Unix_error ((E2BIG as e), f, p)
    | Unix_error ((EACCES as e), f, p)
    | Unix_error ((EBADF as e), f, p)
    | Unix_error ((EBUSY as e), f, p)
    | Unix_error ((EEXIST as e), f, p)
    | Unix_error ((EFBIG as e), f, p)
    | Unix_error ((EINVAL as e), f, p)
    | Unix_error ((EIO as e), f, p)
    | Unix_error ((EMLINK as e), f, p)
    | Unix_error ((ENAMETOOLONG as e), f, p)
    | Unix_error ((ENODEV as e), f, p)
    | Unix_error ((ENOENT as e), f, p)
    | Unix_error ((ENXIO as e), f, p)
    | Unix_error ((EPERM as e), f, p)
    | Unix_error ((EROFS as e), f, p)
    | Unix_error ((EXDEV as e), f, p)
    | Unix_error ((ELOOP as e), f, p)
    | Unix_error ((EOVERFLOW as e), f, p)
    ->
      N.notify @@ N.UNIX_ERROR (e, f, p);
      Option.none

  let is_dir p =
    match stat p with
    | { st_kind = S_DIR; _ } -> true
    | _ -> false

  let is_file p =
    match stat p with
    | { st_kind = S_REG; _ } -> true
    | _ -> false

  let is_symlink p =
    match lstat p with
    | { st_kind = S_LNK; _ } -> true
    | _ -> false

  let file_size p =
    let { st_size = c; _ } = stat p in
    c

  let file_size_opt p = ue_to_opt file_size p

  let mkdirs p =
    let dirs_to_make =
      (* If p is a/b/c dirs_to_make will be
         [ a/b/c; a/b; a ]*)
      List.fold_left
        (fun acc d ->
          match acc with
          | [] -> [ d ]
          | h :: rest ->
              Filename.concat h d :: h :: rest)
        []
      @@ String.split_on_char '/' p
    in
    List.iter (fun d ->
        Unix.(
          match mkdir d 0o700 with
          | exception Unix_error (EEXIST, _, _) -> ()
          | exception e -> raise e
          | _ -> ()))
    @@ List.rev dirs_to_make;
    p

  let mkdirs_for_files files =
    let open Option.Infix in
    files
    |> List.filter_map (fun x ->
           x |> Filename.dirname |> ue_to_opt mkdirs
           <> Option.some x >|= snd)

  let in_dir dir1 dir2 =
    let d1 = Unix.realpath dir1 in
    let d2 = Unix.realpath dir2 in
    String.starts_with ~prefix:d1 d2

  let dir_to_seq path =
    Unix.opendir path
    |> Seq.unfold (fun h ->
           match Unix.readdir h with
           | exception End_of_file ->
               Unix.closedir h;
               None
           | exception e ->
               Unix.closedir h;
               raise e
           | x -> Some (x, h))
    |> Seq.filter (fun x -> x <> "." && x <> "..")
    |> Seq.map (fun x -> Filename.concat path x)

  (* [find_seq] takes paths as a sequence and does all
     the hard work for [find] *)
  let rec find_seq is_dir filter paths =
    paths
    |> Seq.map (fun x ->
           if is_dir x then
             Seq.cons x
               (dir_to_seq x |> find_seq is_dir filter)
           else Seq.return x)
    |> Seq.concat |> Seq.filter filter

  let find ?(is_dir = is_dir) filter paths =
    find_seq is_dir filter @@ List.to_seq paths

  (* [default_bs] is the default block size for
      channel reads and writes *)
  let default_bs = 4096

  type buffer = {
    filename : string;
    data : bytes;
    size : int;
    used : int;
    rd_cnt : int;
    wr_cnt : int;
  }

  (* [read] upgrades the interface to
     [In_channel.input] to accept a buffer type
     @return an option with a non-emoty buffer *)
  let read channel x =
    let { data; size; used; rd_cnt; _ } = x in
    let c =
      In_channel.input channel data used (size - used)
    in
    if c > 0 then
      Some { x with used = c; rd_cnt = rd_cnt + c }
    else None

  (* [write] upgrades the interface to *
      [Out_channel.output] to accept and return a
      buffer. *)
  let write channel x =
    let { data; used; wr_cnt; filename; _ } = x in
    let _ = Out_channel.output channel data 0 used in
    let wr_cnt_new = wr_cnt + used in
    let _ =
      N.notify @@ COPY_PROGRESS (filename, wr_cnt_new)
    in
    { x with wr_cnt = wr_cnt_new }

  let clear b = { b with used = 0 }

  let rec copy_channel rd wrs buf =
    match rd buf with
    | None -> ()
    | Some x ->
        let _ = List.apply wrs x in
        copy_channel rd wrs (clear buf)

  let clone_file src dsts =
    let _ = N.notify @@ START_COPY src in
    let dsts_with_dirs = mkdirs_for_files dsts in
    let b =
      {
        filename = src;
        data = Bytes.create default_bs;
        size = default_bs;
        used = 0;
        rd_cnt = 0;
        wr_cnt = 0;
      }
    in
    let r =
      In_channel.with_open_bin src (fun ic ->
          let rd = read ic in
          Out_channel.with_many_open_bin dsts_with_dirs
            (fun ocs ->
              let wrs = List.map write ocs in
              copy_channel rd wrs b))
    in
    let _ = N.notify @@ FINISH_COPY src in
    r

  let symlink_file target link_name =
    let _ = N.notify @@ START_LINK link_name in
    let tname = Filename.basename target in
    let tdir = Filename.dirname target in
    let ldir = Filename.dirname link_name in
    let new_tgt =
      Filename.concat
        (Filename.path_to tdir ldir)
        tname
    in
    let x =
      Unix.symlink ~to_dir:false new_tgt link_name
    in
    let _ = N.notify @@ FINISH_LINK link_name in
    x

  let symlink_many srcs target =
    List.iter
      (fun x ->
        let _ = Filename.dirname x |> mkdirs in
        symlink_file target x)
      srcs

  let symlink_many_opt x y =
    ue_to_opt (symlink_many x) y
end
