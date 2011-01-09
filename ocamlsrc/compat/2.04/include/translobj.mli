(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: translobj.mli,v 1.1 2000/05/22 08:55:08 lefessan Exp $ *)

val oo_prim: string -> Lambda.lambda

val meth: string -> Ident.t

val reset_labels: unit -> unit
val transl_label_init: Lambda.lambda -> Lambda.lambda
