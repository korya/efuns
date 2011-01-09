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

val readWholeReply : Unix.file_descr -> string
val readReply : Xtypes.display -> unit
val send_alone : Xtypes.display -> string -> int
val send_with_wrapper :
    Xtypes.display -> string -> (string -> unit) -> unit
val openDisplay : string -> Xtypes.display
val closeDisplay : Xtypes.display -> unit
val alloc_id : Xtypes.display -> int

val newBuffer : int -> string
val newRequest : Xtypes.requestOpcode -> int -> string
val newWinRequest : Xtypes.requestOpcode -> int -> Xtypes.window -> string
val emptyRequest : Xtypes.requestOpcode -> string
val simpleRequest : Xtypes.requestOpcode -> 'a -> string
val doubleRequest : Xtypes.requestOpcode -> 'a -> 'b -> string
val littleRequest : Xtypes.requestOpcode -> 'a -> string
val parse_buffer : 'a Jeton.t -> (string -> 'a ) -> string -> unit

val print_xerror : Xtypes.errorEvent -> unit
val xerror_to_string : Xtypes.errorEvent -> string
