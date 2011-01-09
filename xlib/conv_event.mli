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

val debug : bool ref
val eventNum : string -> int
val serial : string -> int
val sent : string -> bool
val convertCore2Event : string -> int -> Xtypes.xevent
val convertEvent2Core : Xtypes.event -> string
val set_extension_target : int -> (string -> int) -> unit