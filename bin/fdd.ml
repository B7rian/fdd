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

(* Command line interface *)

open Cmdliner

let srcs =
  let doc = "Source file(s) to copy." in
  Arg.(
    non_empty
    & pos_left ~rev:true 0 file []
    & info [] ~docv:"SOURCE" ~doc)

let dest =
  let doc =
    "Destination of the copy. Must be a directory if \
     there is more than one $(i,SOURCE)."
  in
  let docv = "DEST" in
  Arg.(
    required
    & pos ~rev:true 0 (some string) None
    & info [] ~docv ~doc)

let cmd =
  let doc = "Copy files" in
  let man_xrefs =
    [
      `Tool "cp";
      `Tool "scp";
      `Tool "rsync";
      `Page ("umask", 2);
      `Page ("symlink", 7);
    ]
  in
  let man =
    [
      `S Manpage.s_bugs;
      `P "Email them to <bugs@example.org>.";
    ]
  in
  let info =
    Cmd.info "fdd" ~version:"%%VERSION%%" ~doc ~man
      ~man_xrefs
  in
  Cmd.v info
    Term.(ret (const Fdd.Backup.backup $ srcs $ dest))

let main () = exit (Cmd.eval cmd)
let () = main ()
