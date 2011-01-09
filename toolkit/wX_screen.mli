(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
val screen_resize : WX_types.screen_struct -> unit
val iter_refresh : < refresh : 'a; .. > list -> unit
val screen_update : WX_types.screen_struct -> unit
class virtual t :
  WX_types.screen_struct ->
  object
    val s : WX_types.screen_struct
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method display : Xtypes.display
    method font_make : string -> bool -> WX_types.font
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method global_color_make : WX_types.color_desc -> bool -> WX_types.color
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method screen : WX_types.screen_struct
    method update : unit
  end
