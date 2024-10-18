let fdd_bar label max =
  let open Progress.Line in
  list
    [
      rpad 20 (const label);
      bar ~style:`UTF8 max;
      count_to ~pp:Progress.Units.Bytes.of_int max;
      bytes_per_sec;
    ]

let with_file_progress action path f =
  let size = Filesystem.file_size path in
  let name = action ^ " " ^ Filename.basename path in
  Progress.with_reporter (fdd_bar name size) f

let with_percent_progress action path f =
  let name = action ^ " " ^ Filename.basename path in
  Progress.with_reporter (fdd_bar name 100) f
