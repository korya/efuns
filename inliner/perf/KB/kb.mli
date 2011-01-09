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

(* $Id: kb.mli,v 1.1 1999/11/22 10:35:58 lefessan Exp $ *)

open Terms
open Equations

val super: term -> term -> (int list * (int * term) list) list
val super_strict: term -> term -> (int list * (int * term) list) list
val critical_pairs: term * term -> term * term -> (term * term) list
val strict_critical_pairs: term * term -> term * term -> (term * term) list
val mutual_critical_pairs: term * term -> term * term -> (term * term) list
val rename: int -> term * term -> term * term
val deletion_message: rule -> unit
val non_orientable: term * term -> unit
val partition: ('a -> bool) -> 'a list -> 'a list * 'a list
val get_rule: int -> rule list -> rule
val kb_completion: (term * term -> bool) -> int -> rule list -> (term * term) list -> int * int -> (term * term) list -> rule list
val kb_complete: (term * term -> bool) -> rule list -> rule list -> unit
