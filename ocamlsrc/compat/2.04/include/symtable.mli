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

(* $Id: symtable.mli,v 1.1 2000/05/22 08:55:05 lefessan Exp $ *)

(* Assign locations and numbers to globals and primitives *)

open Emitcode

(* Functions for batch linking *)

val init: unit -> unit
val patch_object: string -> (reloc_info * int) list -> unit
val require_primitive: string -> unit
val initial_global_table: unit -> Obj.t array
val output_global_map: out_channel -> unit
val output_primitive_names: out_channel -> unit
val output_primitive_table: out_channel -> unit

(* Functions for the toplevel *)

val init_toplevel: unit -> unit
val update_global_table: unit -> unit
val get_global_value: Ident.t -> Obj.t
val assign_global_value: Ident.t -> Obj.t -> unit
val get_global_position: Ident.t -> int

type global_map

val current_state: unit -> global_map
val restore_state: global_map -> unit
val hide_additions: global_map -> unit
val filter_global_map: (Ident.t -> bool) -> global_map -> global_map

(* Error report *)

type error =
    Undefined_global of string
  | Unavailable_primitive of string
  | Wrong_vm of string

exception Error of error

val report_error: error -> unit
