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
  WX_types.container ->
  WX_types.bar_desc ->
  WX_adjust.t ->
  WX_types.base_attributes list ->
  object
    val mutable activated : bool
    val adj : WX_adjust.t
    val mutable button : int option
    val id : int
    val mutable parent : WX_types.container
    val s : WX_types.screen_struct
    val mutable scale_win : Xtypes.window
    val sens : WX_types.bar_desc
    val mutable szhints : WX_types.szhints
    val w : WX_types.window
    method actions : (WX_types.event_desc * WX_types.handler) list
    method background : WX_types.color
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method configure : WX_types.base_attributes list -> unit
    method contained : WX_types.contained
    method container : WX_types.container
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method default_font : WX_types.font
    method destroy : unit
    method display : Xtypes.display
    method draw_relief : unit
    method focus : unit
    method font_make : string -> bool -> WX_types.font
    method foreground : WX_types.color
    method geometry : Xtypes.geometry
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method global_color_make : WX_types.color_desc -> bool -> WX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method inverse : unit
    method iter : (WX_types.contained -> unit) -> unit
    method iter_visible : (WX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : WX_types.container
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : WX_types.screen_struct
    method set_parent : WX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> unit
    method size_request : WX_types.szhints
    method timer : float -> int -> (unit -> unit) -> unit
    method to_refresh : WX_types.refresh_widget
    method to_resize : WX_types.resize_widget
    method update : unit
    method update_adjustement : unit -> unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
  end
class h :
  WX_types.container ->
  WX_adjust.t ->
  WX_types.base_attributes list ->
  object
    val mutable activated : bool
    val adj : WX_adjust.t
    val mutable button : int option
    val id : int
    val mutable parent : WX_types.container
    val s : WX_types.screen_struct
    val mutable scale_win : Xtypes.window
    val sens : WX_types.bar_desc
    val mutable szhints : WX_types.szhints
    val w : WX_types.window
    method actions : (WX_types.event_desc * WX_types.handler) list
    method background : WX_types.color
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method configure : WX_types.base_attributes list -> unit
    method contained : WX_types.contained
    method container : WX_types.container
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method default_font : WX_types.font
    method destroy : unit
    method display : Xtypes.display
    method draw_relief : unit
    method focus : unit
    method font_make : string -> bool -> WX_types.font
    method foreground : WX_types.color
    method geometry : Xtypes.geometry
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method global_color_make : WX_types.color_desc -> bool -> WX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method inverse : unit
    method iter : (WX_types.contained -> unit) -> unit
    method iter_visible : (WX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : WX_types.container
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : WX_types.screen_struct
    method set_parent : WX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> unit
    method size_request : WX_types.szhints
    method timer : float -> int -> (unit -> unit) -> unit
    method to_refresh : WX_types.refresh_widget
    method to_resize : WX_types.resize_widget
    method update : unit
    method update_adjustement : unit -> unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
  end
class v :
  WX_types.container ->
  WX_adjust.t ->
  WX_types.base_attributes list ->
  object
    val mutable activated : bool
    val adj : WX_adjust.t
    val mutable button : int option
    val id : int
    val mutable parent : WX_types.container
    val s : WX_types.screen_struct
    val mutable scale_win : Xtypes.window
    val sens : WX_types.bar_desc
    val mutable szhints : WX_types.szhints
    val w : WX_types.window
    method actions : (WX_types.event_desc * WX_types.handler) list
    method background : WX_types.color
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method configure : WX_types.base_attributes list -> unit
    method contained : WX_types.contained
    method container : WX_types.container
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method default_font : WX_types.font
    method destroy : unit
    method display : Xtypes.display
    method draw_relief : unit
    method focus : unit
    method font_make : string -> bool -> WX_types.font
    method foreground : WX_types.color
    method geometry : Xtypes.geometry
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method global_color_make : WX_types.color_desc -> bool -> WX_types.color
    method handle_button : unit -> unit
    method handle_key : unit -> unit
    method height : int
    method hide : unit
    method id : string
    method inverse : unit
    method iter : (WX_types.contained -> unit) -> unit
    method iter_visible : (WX_types.contained -> unit) -> unit
    method name : string
    method normal : unit
    method parent : WX_types.container
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method realize : unit
    method refresh : unit
    method reverse : bool
    method root_coordinates : int * int
    method screen : WX_types.screen_struct
    method set_parent : WX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> unit
    method size_request : WX_types.szhints
    method timer : float -> int -> (unit -> unit) -> unit
    method to_refresh : WX_types.refresh_widget
    method to_resize : WX_types.resize_widget
    method update : unit
    method update_adjustement : unit -> unit
    method update_size : unit
    method update_top_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
  end
