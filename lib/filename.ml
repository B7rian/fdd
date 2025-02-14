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

(** [Filename] extends [Stdlib.Filename] for our use
    cases *)

include Stdlib.Filename

(** [path_to dst src] finds a relative path from src
    dir to dst dir. Args are in the same order as
    [symlink]. Paths must be either absolute or
    relative to the same directory (usually the one
    that the program is running in). Does not care if
    src or dst don't exist *)
let path_to dst src =
  (* Here’s how this code will find the relative 
   * path from one directory to another
   * 1. Find the common parent directory and ignore 
   * this as is does not need to be included in the 
   * relative path
   * 2. Generate a series of ..s to go from the 
   * source directory to the common parent
   * 3. Append the paths from the common parent to 
   * the destination. 
   * The code will work from left to right to ignore 
   * the path to the common parent, create the series 
   * of ..s and then append what’s left of the 
   * destination path. *)
  let rec fold_paths acc d s =
    match (d, s) with
    | [], [] -> acc
    | dh :: dt, [] -> fold_paths (concat acc dh) dt []
    | [], _sh :: st ->
        fold_paths (concat ".." acc) [] st
    | dh :: dt, sh :: st ->
        if String.length acc = 0 && dh = sh then
          fold_paths acc dt st
        else
          fold_paths
            (concat (concat ".." acc) dh)
            dt st
  in
  fold_paths ""
    (String.split_on_char '/' dst)
    (String.split_on_char '/' src)
