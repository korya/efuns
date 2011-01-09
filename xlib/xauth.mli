(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Auto-generated mli file *)
val auth_filename : string
val home : string
val filename : string
val read_short : in_channel -> int
val read_counted_string : in_channel -> string
exception UnknownFamily
val readOneAuth : in_channel -> Xtypes.xauth_record
val print : Xtypes.xauth_record -> unit
val best_names : string array
val getBestAuth : Xtypes.xauth_record -> string * string
