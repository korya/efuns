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

(* $Id: translclass.mli,v 1.1 2000/05/22 08:55:06 lefessan Exp $ *)

open Typedtree
open Lambda

val class_stub : lambda
val transl_class :
  Ident.t list -> Ident.t -> int -> string list -> class_expr -> lambda;;

type error = Illegal_class_expr

exception Error of Location.t * error

val report_error: error -> unit
