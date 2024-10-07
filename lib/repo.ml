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
  if has path r then El_result.return r
  else if Filesystem.is_dir path then
    Filesystem.find Filesystem.is_file [ path ]
    |> Seq.fold_left
         (fun r p -> El_result.bind r (add p))
         (El_result.return r)
  else
    let file = File.from_path path in
    let _ = Filesystem.mkdirs @@ repo_dir file r in
    let _ =
      match find_copy file r with
      | Some c ->
          Filesystem.symlink_file
            (Filename.concat
               (Filesystem.path_to (repo_dir c r)
                  (repo_dir file r))
               (File.filename c))
            (repo_path file r)
      | None ->
          Filesystem.copy_file_to_dir (File.path file)
            r.dir
    in
    El_result.return { r with files = file :: r.files }
