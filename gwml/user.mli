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

val noOwnerEvents : bool
val peek_window :
  (Xtypes.display -> Xtypes.window -> 'a) -> Gwml.screen_desc -> unit -> unit
type move_resize = Place | Move | Resize
val time : int ref
val motion_event : int ref
val aborted : bool ref
val move_mode : move_resize -> bool
val move_resize : Gwml.wob -> move_resize -> bool -> unit
val abort : Gwml.wob -> unit
val move : Gwml.wob -> bool -> unit
val place : Gwml.wob -> bool -> unit
val resize : Gwml.wob -> bool -> unit
val select : Gwml.wob -> Xtypes.window
val client_top : Gwml.wob -> Gwml.wob
val twm_ResizeCursor : Gwml.cursor_desc ref
val twm_resize : Gwml.wob -> bool -> unit
