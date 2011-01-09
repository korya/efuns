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

open Xtypes

exception BadFile of string * int

type bitmap_data = int * int * int * int * string
type pixmap_data = int * int * Xtypes.colordef array * int array array
type bitmap = Xtypes.size * Xtypes.size * coord * coord * Xtypes.pixmap
type pixmap = Xtypes.size * Xtypes.size * int * Xtypes.pixmap * Xtypes.pixmap

val readPixmapDataFromFile : string -> pixmap_data
val readBitmapDataFromFile : string -> bitmap_data
val createBitmapFromData : 
  Xtypes.display -> Xtypes.window -> bitmap_data -> bitmap

val createPixmapFromData : Xtypes.display -> Xtypes.window -> Xtypes.colormap
  -> int -> pixmap_data -> pixmap

val createBitmapFromFile : Xtypes.display -> Xtypes.window -> string -> bitmap
val createPixmapFromFile : Xtypes.display -> Xtypes.window -> Xtypes.colormap
  -> int -> string ->  pixmap

val createMLStringFromPixmapData :  pixmap_data -> string -> string
