let with_count_progress _max action path f =
  let name = Filename.basename path in
  let _ = Printf.fprintf stderr "%s %s" action name in
  let _ = flush stderr in
  f (fun _ ->
      prerr_char '.';
      flush stderr)

let with_file_progress action path f =
  with_count_progress 0 action path f
