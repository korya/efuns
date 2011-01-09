(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
  
type image_id

type image = {
    mutable image_id : image_id;
    mutable w : int;
    mutable h : int;
  }

type pixmap = {
    image : image;
    mutable pixmap : Xtypes.pixmap;
    mutable mask : Xtypes.pixmap;
  }

val image_init : string -> unit
val image_load : string -> image
val image_kill : image -> unit
val image_destroy : image -> unit
val image_pixmap : image -> int -> int -> pixmap
val pixmap_free : Xtypes.pixmap -> unit
