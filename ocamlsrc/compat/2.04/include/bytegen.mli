(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: bytegen.mli,v 1.1 2000/05/22 08:54:53 lefessan Exp $ *)

(* Generation of bytecode from lambda terms *)

open Lambda
open Instruct

val compile_implementation: string -> lambda -> instruction list
val compile_phrase: lambda -> instruction list * instruction list
