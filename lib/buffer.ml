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

(* [default_bs] is the default block size for
      channel reads and writes *)
let default_bs = 4096

type t = {
  data : bytes;
  size : int;
  used : int;
  rd_cnt : int;
  wr_cnt : int;
}

let empty () =
  {
    data = Bytes.create default_bs;
    size = default_bs;
    used = 0;
    rd_cnt = 0;
    wr_cnt = 0;
  }

(** [read] upgrades the interface to [In_channel.input]
    to accept a buffer type
    @return an option with a non-emoty buffer *)
let read channel x =
  let { data; size; used; rd_cnt; _ } = x in
  let c =
    In_channel.input channel data used (size - used)
  in
  if c > 0 then
    Some { x with used = c; rd_cnt = rd_cnt + c }
  else None

(** [write] upgrades the interface to *
    [Out_channel.output] to accept and return a buffer.
*)
let write channel x =
  let { data; used; wr_cnt; _ } = x in
  let _ = Out_channel.output channel data 0 used in
  let wr_cnt_new = wr_cnt + used in
  { x with wr_cnt = wr_cnt_new }

let clear b = { b with used = 0 }
