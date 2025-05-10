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

module FS = Filesystem
module N = Names

let%expect_test "backups" =
  let srcs = [ "a"; "b"; "c"; "d" ] in
  let c, l = N.backups srcs "dst" 1 in
  List.iter (Stdio.printf "%s ") c;
  Stdio.printf "\n";
  List.iter (Stdio.printf "%s ") l;
  Stdio.printf "\n";
  let c, l = N.backups srcs "dst" 3 in
  List.iter (Stdio.printf "%s ") c;
  Stdio.printf "\n";
  List.iter (Stdio.printf "%s ") l;
  Stdio.printf "\n";
  let c, l = N.backups srcs "dst" 5 in
  List.iter (Stdio.printf "%s ") c;
  Stdio.printf "\n";
  List.iter (Stdio.printf "%s ") l;
  Stdio.printf "\n";
  let c, l = N.backups srcs "dst" 10 in
  List.iter (Stdio.printf "%s ") c;
  Stdio.printf "\n";
  List.iter (Stdio.printf "%s ") l;
  Stdio.printf "\n";
  [%expect
    {|
    dst/a
    dst/b dst/c dst/d
    dst/a dst/b dst/c
    dst/d
    dst/a dst/b dst/c dst/d dst/extra_copies/a

    dst/a dst/b dst/c dst/d dst/extra_copies/a dst/extra_copies/b dst/extra_copies/c dst/extra_copies/d dst/extra_copies_1/a dst/extra_copies_1/b
    |}]
