(* let bs = int_of_string Sys.argv.(1) *)
(* let bs = 1000000 *)

(* let copy_block ic oc bytes len = *)
(*   let bytes_in = In_channel.input ic bytes 0 len in *)
(*   Out_channel.output oc bytes 0 bytes_in; *)
(*   bytes_in *)

(* let rec copy_channel ic oc bytes buffer_size =   *)
(*   let count = copy_block ic oc bytes buffer_size in *)
(*   match count with *)
(*     0 -> () *)
(*   | _ -> copy_channel ic oc bytes buffer_size *)

(* let copy_file_by_name fn1 fn2 = *)
(*   let b = Bytes.create bs in *)
(*   In_channel.with_open_bin fn1 (fun ic -> *)
(*       Out_channel.with_open_bin fn2 (fun oc -> *)
(*           copy_channel ic oc b bs)) *)
      
let sha256sum_file_by_name fn =
  let digest = Sha256.file_fast fn in
  Printf.printf "%s  %s" (Sha256.to_hex digest) fn

let () = sha256sum_file_by_name "foo"
    

