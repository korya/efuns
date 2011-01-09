(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

type 'a t
val create : 'a -> 'a t
val put : 'a t -> 'a -> unit
val take : 'a t -> 'a
val clear : 'a t -> unit
val read : 'a t -> 'a
val peek : 'a t -> 'a
val put_back : 'a t -> 'a -> unit
val empty_take : 'a t -> bool
val empty_read : 'a t -> bool
val to_list : 'a t -> 'a list
val clean : 'a t -> unit
val clear_lookahead : 'a t -> unit
