module FS =
  Filesystem.Make (Notifiable.IgnoreNotifications)

type t = {
  dir : String.t;
  files : File.t list;
  fs : (module Filesystem.S);
}

let empty d fs = { dir = d; files = []; fs }

let has path r =
  List.exists
    (fun repo_file -> File.path repo_file = path)
    r.files

let repo_path f r =
  Filename.concat r.dir @@ File.path f

let repo_dir f r = Filename.dirname @@ repo_path f r

let find_copy f r =
  List.find_opt
    (fun e ->
      File.same_data e f && (not @@ File.same_name e f))
    r.files

let rec add path r =
  let module FS = (val r.fs : Filesystem.S) in
  let open FS in
  let open Exnlogger in
  try
    if has path r then return r
    else if is_dir path then
      find is_file [ path ]
      |> Seq.fold_left
           (fun r p -> bind r (add p))
           (return r)
    else
      let file = File.from_path path in
      let _ = mkdirs @@ repo_dir file r in
      let _ =
        match find_copy file r with
        | Some c ->
            symlink_file
              (Filename.concat
                 (path_to (repo_dir c r)
                    (repo_dir file r))
                 (File.filename c))
              (repo_path file r)
        | None ->
            copy_file_to_dir (File.path file) r.dir
      in
      return { r with files = file :: r.files }
  with e -> add_error (return r) e
