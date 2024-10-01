type t = {
    dir : String.t;
    files : File.t list;
}

let empty d = { dir = d; files = [] }

let has path r =
  List.exists
    (fun repo_file -> File.path repo_file = path)
    r.files

let find_copy f r =
  List.find_opt
    (fun e -> (File.same_data e f)
                && (not @@ File.same_name e f))
    r.files

let add path r =
  if has path r  then
    El_result.return r
  else
    let file = File.from_path path in
    let _ = Filesystem.mkdirs
            @@ Filename.concat r.dir
            @@ Filename.dirname path
    in
    let _ = match find_copy file r with
      | Some c -> Filesystem.symlink_file
                    (File.path c)
                    (r.dir ^ "/" ^ File.path file)
      | None -> Filesystem.copy_file_to_dir
                  (File.path file)
                  r.dir
    in
    El_result.return { r with files = file :: r.files }
