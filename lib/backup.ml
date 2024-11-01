module Make : Fileset.S = struct
  type t = {
    dir : String.t;
    files : File.t list;
    fs : (module Filesystem.S);
  }

  let empty dir fs = { dir; files = []; fs }

  let has path x =
    List.exists
      (fun backup_file -> File.path backup_file = path)
      x.files

  let backup_path f x =
    Filename.concat x.dir @@ File.path f

  let backup_dir f x =
    Filename.dirname @@ backup_path f x

  let find_copy f x =
    List.find_opt
      (fun e ->
        (not @@ File.same_name e f)
        && File.same_data e f)
      x.files

  let rec add path x =
    let module FS = (val x.fs : Filesystem.S) in
    let open FS in
    let open Exnlogger in
    try
      if has path x then return x
      else if is_dir path then
        find is_file [ path ]
        |> Seq.fold_left
             (fun x p -> bind x (add p))
             (return x)
      else
        let file = File.from_path path in
        let _ = mkdirs @@ backup_dir file x in
        let _ =
          match find_copy file x with
          | Some c ->
              symlink_file
                (Filename.concat
                   (path_to (backup_dir c x)
                      (backup_dir file x))
                   (File.filename c))
                (backup_path file x)
          | None ->
              copy_file_to_dir (File.path file) x.dir
        in
        return { x with files = file :: x.files }
    with e -> add_exn (return x) e

  let close x =
    let open Exnlogger in
    try
      let _ =
        Out_channel.with_open_text
          (Filename.concat x.dir "checksums")
          (fun oc ->
            List.iter
              (fun f ->
                Printf.fprintf oc "%s  %s\n"
                  (File.hash f) (File.path f))
              x.files)
      in
      return x
    with e -> add_exn (return x) e
end
