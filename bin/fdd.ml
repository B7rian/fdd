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
  let doc =
    "Source files and directories to back up."
  in
  Arg.(
    non_empty
    & pos_left ~rev:true 0 file []
    & info [] ~docv:"SOURCE" ~doc)

let dest =
  let doc = "Backup location . Must be a directory." in
  let docv = "DEST" in
  Arg.(
    required
    & pos ~rev:true 0 (some string) None
    & info [] ~docv ~doc)

let ncopies =
  let docv = "NCOPIES" in
  let doc =
    "Make $(docv) copies of the file before starting \
     to deduplicate extra copies."
  in
  let info =
    Arg.info [ "n"; "c"; "ncopies" ] ~docv ~doc
  in
  Arg.value (Arg.opt Arg.int 1 info)

let cmd =
  let doc =
    "Backup files, symlinking duplicates to identical \
     files even if the have different names."
  in
  let man_xrefs =
    [
      `Tool "cp";
      `Tool "scp";
      `Tool "rsync";
      `Tool "tar";
      `Page ("umask", 2);
      `Page ("symlink", 7);
    ]
  in
  let man =
    [
      `S Manpage.s_bugs;
      `P
        "Not well tested yet.  If you find a bug, \
         please file an issue on \
         <https://github.com/b7rian/fdd>.";
    ]
  in
  let info =
    Cmd.info "fdd" ~version:"%%VERSION%%" ~doc ~man
      ~man_xrefs
  in
  Cmd.v info
    Term.(
      ret
        (const Fdd.Backup.backup
        $ ncopies $ srcs $ dest))

let main () = exit (Cmd.eval cmd)
let () = main ()
