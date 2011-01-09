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

(* $Id: taku.ml,v 1.1 1999/11/22 10:35:57 lefessan Exp $ *)

let rec tak (x, y, z) =
  if x > y then tak(tak (x-1, y, z), tak (y-1, z, x), tak (z-1, x, y))
           else z

let rec repeat n =
  if n <= 0 then 0 else tak(18,12,6) + repeat(n-1)

let _ = print_int (repeat 50); print_newline(); exit 0
