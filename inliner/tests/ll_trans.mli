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

exception BrokenConnection
val sock_io_read: Unix.file_descr -> int -> string -> int -> unit
val sock_io_write: Unix.file_descr -> int -> string -> int -> unit
exception NotADigit of int
val discardInt : string -> int -> int * int
val parseDisplayName : string -> string * int * int
val openConnectionWithDisplay :
    string -> Unix.file_descr * string * int * (string * string)
