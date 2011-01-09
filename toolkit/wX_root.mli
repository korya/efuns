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
class t :
  < display : Xtypes.display; eloop : Eloop.display; .. > ->
  int ->
  object
    val mutable default_font : WX_types.font
    val s : WX_types.screen_struct
    val w : WX_types.window
    method actions : (WX_types.event_desc * WX_types.handler) list
    method background : WX_types.color
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method container : WX_types.container
    method container_add : WX_types.contained -> unit
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method default_font : WX_types.font
    method display : Xtypes.display
    method font_make : string -> bool -> WX_types.font
    method foreground : WX_types.color
    method geometry : Xtypes.geometry
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method global_color_make : WX_types.color_desc -> bool -> WX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method name : string
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method root_coordinates : int * int
    method screen : WX_types.screen_struct
    method update : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
  end
class from_display :
  string ->
  int ->
  object
    val mutable default_font : WX_types.font
    val s : WX_types.screen_struct
    val w : WX_types.window
    method actions : (WX_types.event_desc * WX_types.handler) list
    method background : WX_types.color
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method container : WX_types.container
    method container_add : WX_types.contained -> unit
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method default_font : WX_types.font
    method display : Xtypes.display
    method font_make : string -> bool -> WX_types.font
    method foreground : WX_types.color
    method geometry : Xtypes.geometry
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method global_color_make : WX_types.color_desc -> bool -> WX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method name : string
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method root_coordinates : int * int
    method screen : WX_types.screen_struct
    method update : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
  end
