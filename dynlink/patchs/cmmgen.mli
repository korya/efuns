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

(* $Id: cmmgen.mli,v 1.2 1999/09/20 13:49:02 lefessan Exp $ *)

(* Translation from closed lambda to C-- *)

val compunit: int -> Clambda.ulambda -> Cmm.phrase list

val apply_function: int -> Cmm.phrase
val curry_function: int -> Cmm.phrase list
val entry_point: string list -> Cmm.phrase
val global_table: string list -> Cmm.phrase
val frame_table: string list -> Cmm.phrase
val data_segment_table: string list -> Cmm.phrase
val code_segment_table: string list -> Cmm.phrase
val predef_exception: string -> Cmm.phrase
val globals_map : (string * string) list -> Cmm.phrase
