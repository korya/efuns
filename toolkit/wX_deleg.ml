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

open Xtypes
open WX_types


class base (deleg : WX_base.t) =
  object 
    method default_font = deleg#default_font
    method actions = deleg#actions
    method background = deleg#background
    method click_type = deleg#click_type
    method color_make = deleg#color_make
    method container = deleg#container
    method cursor_make = deleg#cursor_make
    method virtual default_font : WX_types.font
    method display = deleg#display
    method font_make = deleg#font_make
    method foreground = deleg#foreground
    method geometry = deleg#geometry
    method getHilite = deleg#getHilite
    method getShadow = deleg#getShadow
    method global_color_make = deleg#global_color_make
    method handle_button = deleg#handle_button
    method handle_key = deleg#handle_key
    method height = deleg#height
    method name = deleg#name
    method pixmap_make = deleg#pixmap_make
    method root_coordinates = deleg#root_coordinates
    method screen = deleg#screen
    method update = deleg#update
    method update_top_size = deleg#update_top_size
    method wait_resize = deleg#wait_resize
    method width = deleg#width
    method window = deleg#window
    method xevents = deleg#xevents
end

class wx_object (deleg : WX_object.t) =
    object
    
    method configure = deleg#configure
    method contained = deleg#contained
    method destroy = deleg#destroy
    method draw_relief = deleg#draw_relief
    method focus = deleg#focus
    method hide = deleg#hide
    method show = deleg#show
    method id = deleg#id
    method inverse = deleg#inverse
    method iter = deleg#iter
    method iter_visible = deleg#iter_visible
    method normal = deleg#normal
    method parent = deleg#parent
    method realize = deleg#realize
    method refresh = deleg#refresh
    method reverse = deleg#reverse
    method set_parent = deleg#set_parent
    method size_allocate = deleg#size_allocate
    method size_request = deleg#size_request
    method to_refresh = deleg#to_refresh
    method to_resize = deleg#to_resize
    method update_size = deleg#update_size
    method wait_refresh = deleg#wait_refresh
  end

class top (deleg : WX_top.t) =
  object
    method destroy = deleg#destroy
    method show = deleg#show
    method root_coordinates = deleg#root_coordinates
    method change_position = deleg#change_position
    method container_add = deleg#container_add
    method grab_pointer = deleg#grab_pointer
    method top = deleg#top
end


class wmtop (deleg : WX_wmtop.t) =
  object    
    inherit top (deleg :> WX_top.t)
    
    method withdraw = deleg#withdraw
    method iconify = deleg#iconify
    method deiconify = deleg#deiconify
    method setWM_NAME n = deleg#setWM_NAME n
    method setWM_ICON_NAME n = deleg#setWM_ICON_NAME n
    method setWM_CLASS cls app = deleg#setWM_CLASS cls app
    method setWM_TRANSIENT_FOR t = deleg#setWM_TRANSIENT_FOR t
end   

class container_add (deleg : WX_types.container_add) =
  object (self)

    method container_add = deleg#container_add
    method screen = deleg#screen
    method color_make = deleg#color_make
    method cursor_make = deleg#cursor_make
    method display = deleg#display
    method font_make = deleg#font_make
    method pixmap_make = deleg#pixmap_make
    method update_top_size = deleg#update_top_size
    method window = deleg#window
    method getShadow = deleg#getShadow
    method getHilite = deleg#getHilite
    method default_font = deleg#default_font
    method container = (deleg :> container)
end

class label (deleg : WX_label.t) =
  object (self)
    method set_options = deleg#set_options
    method set_font = deleg#set_font
    method set_justification = deleg#set_justification
    method set_string = deleg#set_string
    method string = deleg#string
end
  
class pixmap (deleg : WX_pixmap.t) =
  object (self)
    method set_pixmap = deleg#set_pixmap
end
