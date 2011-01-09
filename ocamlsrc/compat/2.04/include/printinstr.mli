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

(* $Id: printinstr.mli,v 1.1 2000/05/22 08:55:01 lefessan Exp $ *)

(* Pretty-print lists of instructions *)

open Instruct

val instruction: instruction -> unit
val instrlist: instruction list -> unit
