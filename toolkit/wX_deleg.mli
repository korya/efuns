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
class base :
  WX_base.t ->
  object
    method actions : (WX_types.event_desc * WX_types.handler) list
    method background : WX_types.color
    method click_type : WX_types.click
    method color_make : string -> bool -> WX_types.color
    method container : WX_types.container
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
    method wait_resize : unit
    method width : int
    method window : Xtypes.window
    method xevents : Xtypes.xevent -> unit
  end
class wx_object :
  WX_object.t ->
  object
    method configure : WX_types.base_attributes list -> unit
    method contained : WX_types.contained
    method destroy : unit
    method draw_relief : unit
    method focus : unit
    method hide : unit
    method id : string
    method inverse : unit
    method iter : (WX_types.contained -> unit) -> unit
    method iter_visible : (WX_types.contained -> unit) -> unit
    method normal : unit
    method parent : WX_types.container
    method realize : unit
    method refresh : unit
    method reverse : bool
    method set_parent : WX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> unit
    method size_request : WX_types.szhints
    method to_refresh : WX_types.refresh_widget
    method to_resize : WX_types.resize_widget
    method update_size : unit
    method wait_refresh :
      bool ->
      Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
  end
class top :
  WX_top.t ->
  object
    method destroy : unit
    method show : unit
    method change_position : int -> int -> unit
    method container_add : WX_types.contained -> unit
    method grab_pointer : unit
    method root_coordinates : int * int
    method top : WX_types.container
  end
class wmtop :
  WX_wmtop.t ->
  object
    inherit top
    
    method deiconify : unit
    method grab_pointer : unit
    method iconify : unit
    method root_coordinates : int * int
    method setWM_CLASS : string -> string -> unit
    method setWM_ICON_NAME : string -> unit
    method setWM_NAME : string -> unit
    method setWM_TRANSIENT_FOR : WX_types.container -> unit
    method withdraw : unit
      
end

class container_add : WX_types.container_add ->
  object
    method container_add : WX_types.contained -> unit
    method color_make : string -> bool -> WX_types.color
    method container : WX_types.container
    method cursor_make : WX_types.cursor_desc -> bool -> WX_types.cursor
    method default_font : WX_types.font
    method display : Xtypes.display
    method font_make : string -> bool -> WX_types.font
    method getHilite : WX_types.color -> WX_types.color
    method getShadow : WX_types.color -> WX_types.color
    method pixmap_make : string * WX_types.pixmap_desc -> Xpm.pixmap
    method screen : WX_types.screen_struct
    method update_top_size : unit
    method window : Xtypes.window
  end
class label :
  WX_label.t ->
  object
    method set_font : string -> unit
    method set_justification : WX_types.justified -> unit
    method set_options : WX_label.options list -> unit
    method set_string : string -> unit
    method string : string
  end
class pixmap :
  WX_pixmap.t ->
  object method set_pixmap : string * WX_types.pixmap_desc -> unit end
