(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Unix
open Printf
  
let tms_u = ref 0.0
let tms_s = ref 0.0
  
let init () = 
  let t = times () in
  tms_s := t.tms_stime;
  tms_u := t.tms_utime
  
let stranches = Array.create 10 0.0
let utranches = Array.create 10 0.0

let stop i =
  let t = times () in
  stranches.(i) <- stranches.(i) +. (t.tms_stime -. !tms_s);  
  utranches.(i) <- utranches.(i) +. (t.tms_utime -. !tms_u);  
  tms_s := t.tms_stime;
  tms_u := t.tms_utime
  
let print () =
  for i = 0 to 9 do
    printf "Tranche %d: user %4f system %4f Total: %4f\n"
      i utranches.(i) stranches.(i) (utranches.(i) +. stranches.(i));
  done;
  print_newline ()