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

let image_init display = failwith "Imlib not available"
let image_load image = failwith "Imlib not available"
let image_kill image = failwith "Imlib not available"
let image_destroy image = failwith "Imlib not available"
let image_pixmap image dx dy = failwith "Imlib not available"
let pixmap_free pixmap = failwith "Imlib not available"
