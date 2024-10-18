type t = { dir : String.t; files : File.t list }

let empty d = { dir = d; files = [] }

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
  let open Filesystem in
  let open El_result in
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
            Ui.with_percent_progress "link" path
              (fun report ->
                let _ =
                  symlink_file
                    (Filename.concat
                       (path_to (repo_dir c r)
                          (repo_dir file r))
                       (File.filename c))
                    (repo_path file r)
                in
                report 100)
        | None ->
            Ui.with_file_progress "copy" path
              (fun report ->
                copy_file_to_dir ~report
                  (File.path file) r.dir)
      in
      return { r with files = file :: r.files }
  with e -> add_error (return r) e
