(* let default_bs = 4096 *)

(* let copy_block ic oc bytes len = *)
(*   let bytes_in = In_channel.input ic bytes 0 len in *)
(*   Out_channel.output oc bytes 0 bytes_in; *)
(*   bytes_in *)

(* let rec copy_channel ic oc bytes buffer_size = *)
(*   let count = copy_block ic oc bytes buffer_size in *)
(*   match count with *)
(*     0 -> () *)
(*   | _ -> copy_channel ic oc bytes buffer_size *)

(* let copy_file_by_name fn1 fn2 = *)
(*   let b = Bytes.create default_bs in *)
(*   In_channel.with_open_bin fn1 (fun ic -> *)
(*       Out_channel.with_open_bin fn2 (fun oc -> *)
(*           copy_channel ic oc b default_bs)) *)
      
(* let sha256sum_file_by_name fn = *)
(*   let digest = Sha256.file_fast fn in *)
(*   Printf.printf "%s  %s" (Sha256.to_hex digest) fn *)

let cp verbose recurse force srcs dest =
  let many = List.length srcs > 1 in
  if many && (not (Sys.file_exists dest) || not (Sys.is_directory dest))
  then `Error (false, dest ^ ": not a directory") else
  `Ok (Printf.printf
         "verbose = %B\nrecurse = %B\nforce = %B\nsrcs = %s\ndest = %s\n"
         verbose recurse force (String.concat ", " srcs) dest)

(* Command line interface *)

open Cmdliner

let verbose =
  let doc = "Print file names as they are copied." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let recurse =
  let doc = "Copy directories recursively." in
  Arg.(value & flag & info ["r"; "R"; "recursive"] ~doc)

let force =
  let doc = "If a destination file cannot be opened, remove it and try again."in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let srcs =
  let doc = "Source file(s) to copy." in
  Arg.(non_empty & pos_left ~rev:true 0 file [] & info [] ~docv:"SOURCE" ~doc)

let dest =
  let doc = "Destination of the copy. Must be a directory if there is more \
             than one $(i,SOURCE)." in
  let docv = "DEST" in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv ~doc)

let cmd =
  let doc = "Copy files" in
  let man_xrefs =
    [ `Tool "mv"; `Tool "scp"; `Page ("umask", 2); `Page ("symlink", 7) ]
  in
  let man =
    [ `S Manpage.s_bugs;
      `P "Email them to <bugs@example.org>."; ]
  in
  let info = Cmd.info "cp" ~version:"%%VERSION%%" ~doc ~man ~man_xrefs in
  Cmd.v info Term.(ret (const cp $ verbose $ recurse $ force $ srcs $ dest))


let main () = exit (Cmd.eval cmd)
let () = main ()

    

