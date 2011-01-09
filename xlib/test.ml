open Printf
open Xrm
let t = create ()
let _ = load t "/home/specialix/lefessan/.Xdefaults"
  
let _ =
  Printf.printf "OK"; print_newline () ;
  save t "";
  printf "%s\n" (try 
      get t [ "XDvi";"background"] with Not_found -> "Not_found");
  printf "%s\n" (try 
      get t [ "XTerm";"scrollBar"] with Not_found -> "Not_found");
  set t ["*";"expert"] "false";
  printf "%s\n" (try 
      get t [ "XDvi";"expert"] with Not_found -> "Not_found");
  printf "%s\n" (try 
      get t [ "XDvi";"window";"expert"] with Not_found -> "Not_found");



