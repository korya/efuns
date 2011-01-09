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

val set_shape : Gwml.wob -> Xtypes.pixmap -> int -> int -> unit
class pixmap :
  Gwml.wob ->
  object
    inherit Wob.wob_base
    method set_pixmap : Gwml.pixmap_desc -> unit
  end
val make : Gwml.wob -> Gwml.pixmap_desc -> pixmap
