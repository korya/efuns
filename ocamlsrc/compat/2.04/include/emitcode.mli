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

(* $Id: emitcode.mli,v 1.1 2000/05/22 08:54:54 lefessan Exp $ *)

(* Generation of bytecode for .cmo files *)

open Lambda
open Instruct

(* Relocation information *)

type reloc_info =
    Reloc_literal of structured_constant    (* structured constant *)
  | Reloc_getglobal of Ident.t              (* reference to a global *)
  | Reloc_setglobal of Ident.t              (* definition of a global *)
  | Reloc_primitive of string               (* C primitive number *)

(* Descriptor for compilation units *)

type compilation_unit =
  { cu_name: string;                    (* Name of compilation unit *)
    mutable cu_pos: int;                (* Absolute position in file *)
    cu_codesize: int;                   (* Size of code block *)
    cu_reloc: (reloc_info * int) list;  (* Relocation information *)
    cu_imports: (string * Digest.t) list; (* Names and CRC of intfs imported *)
    cu_primitives: string list;         (* Primitives declared inside *)
    mutable cu_force_link: bool;        (* Must be linked even if unref'ed *)
    mutable cu_debug: int;              (* Position of debugging info, or 0 *)
    cu_debugsize: int }                 (* Length of debugging info *)

(* Format of a .cmo file:
     magic number (Config.cmo_magic_number)
     absolute offset of compilation unit descriptor
     block of relocatable bytecode
     compilation unit descriptor *)

val to_file: out_channel -> string -> instruction list -> unit
        (* Arguments:
             channel on output file
             name of compilation unit implemented
             list of instructions to emit *)
val to_memory: instruction list -> instruction list ->
                    string * int * (reloc_info * int) list
        (* Arguments:
             initialization code (terminated by STOP)
             function code
           Results:
             block of relocatable bytecode
             size of this block
             relocation information *)

