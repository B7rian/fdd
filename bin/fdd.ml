
let backup srcs dst =
  let repo = Fdd.Repo.empty dst in
  let result = List.fold_left
    (fun r f -> Fdd.El_result.(r >>= Fdd.Repo.add f))
    (Fdd.Repo.add (List.hd srcs) repo)
    (List.tl srcs)
  in
  match Fdd.El_result.get_exns result with
    [] -> `Ok ()
  | _ -> `Error (false, "Error copying files")


(* Command line interface *)

open Cmdliner

let srcs =
  let doc = "Source file(s) to copy." in
  Arg.(non_empty
        & pos_left ~rev:true 0 file []
        & info [] ~docv:"SOURCE" ~doc)

let dest =
  let doc = "Destination of the copy. Must be a \
             directory if there is more \
             than one $(i,SOURCE)." in
  let docv = "DEST" in
  Arg.(required
        & pos ~rev:true 0 (some string) None
        & info [] ~docv ~doc)

let cmd =
  let doc = "Copy files" in
  let man_xrefs =
        [ `Tool "cp";
        `Tool "scp";
        `Tool "rsync";
        `Page ("umask", 2);
        `Page ("symlink", 7) ]
  in
  let man =
    [ `S Manpage.s_bugs;
      `P "Email them to <bugs@example.org>."; ]
  in
  let info = Cmd.info "fdd"
        ~version:"%%VERSION%%"
        ~doc
        ~man
        ~man_xrefs in
  Cmd.v info Term.(ret (const backup $ srcs $ dest))

let main () = exit (Cmd.eval cmd)
let () = main ()

