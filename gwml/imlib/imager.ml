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

open Gwml_args
  
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

external c_image_init : string -> unit = "image_init"
external c_image_load : string -> image = "image_load"
external c_image_pixmap : image -> int -> int -> pixmap = "image_pixmap"
external c_pixmap_free : Xtypes.pixmap -> unit = "pixmap_free"
external c_image_kill : image -> unit = "image_kill"
external c_image_destroy : image -> unit = "image_destroy"
  
let init = ref false
let display = ref ""
  
let image_init d =
  let d = if d = "" then
      try
        Sys.getenv "DISPLAY"
      with _ -> ":0"
    else d in
      
  display := d
  
let image_load filename =
  if Sys.file_exists filename then begin
      if not !init then (init := true; c_image_init !display);
      assert (not !server_grabbed);
      c_image_load filename
    end else raise Not_found
    
let image_pixmap image dx dy =
  if not !init then (init := true; c_image_init !display);
  assert (not !server_grabbed);
  c_image_pixmap image dx dy
  
let pixmap_free pix =
  if not !init then (init := true; image_init !display);
  assert (not !server_grabbed);
  c_pixmap_free pix
  
let image_kill image = 
  assert (not !server_grabbed);
  c_image_kill image
let image_destroy image = 
  assert (not !server_grabbed);
  c_image_destroy image
