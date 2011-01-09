(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: fib.ml,v 1.1 1999/11/22 10:35:54 lefessan Exp $ *)

let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let _ =
  let n =
    if Array.length Sys.argv >= 2 
    then int_of_string Sys.argv.(1)
    else 30 in
  print_int(fib n); print_newline(); exit 0

