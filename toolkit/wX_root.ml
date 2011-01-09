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

class t display i =
  let eloop = display#eloop in
  let display = display#display in
  let scr = display.dpy_roots.(i) in
  let fid = X.openFont display "fixed" in
  let qf = X.queryFont display fid in
  let maxb = qf.qf_info.font_max_bounds in
  let font = {
      font_name = "fixed";
      font_id = fid;
      font_info = qf;
      font_ascent = maxb.char_ascent;
      font_height = maxb.char_ascent + maxb.char_descent;
      font_width = maxb.char_width;      
    } in
  let s =  {
      s_display = display;
      s_screen = scr;
      s_colors = Hashtbl.create 23;
      s_fonts = Hashtbl.create 23;
      s_pixmaps = Hashtbl.create 23;
      s_cursors = Hashtbl.create 23;
      s_eloop = eloop;
      s_default_color = { c_pixel = scr.scr_black_pixel;
        c_red = 0; c_green = 0; c_blue = 0; };
      s_default_font = font;
      s_last_click = default_click;
      s_timestamp = 5;
      s_wait_resize = [];
      s_wait_refresh = [];
      s_gcs = GCCache.create display scr;
    } 
  
  in
  let root = display.dpy_roots.(i) in
  let window = { 
      w_clear = false;
      w_window = root.scr_root;
      w_shown = true;
      w_geometry = { x=0; y=0; border=0;
        width = root.scr_width; height = root.scr_height } ;
      w_override = true;
      w_background = noColor;
      w_foreground = noColor;
      w_relief_colors = None;
      w_relief = ReliefFlat;
      w_borderpixel = noColor;
      w_cursor = noCursor;
      w_mask = [];
      w_size_timestamp = 0;
      w_refresh_timestamp = 0;
      w_inverse = false;
      w_enter_window = null_handler;
      w_leave_window = null_handler;
      w_button_released = null_handler;
      w_button_press = null_handler;
      w_key_press = null_handler;      
      w_button_motion = null_handler;
      w_focus_in = null_handler;
      w_focus_out = null_handler;
      w_actions = [];
      w_ipad_x = 0;
      w_ipad_y = 0;
      w_fill_x = false;
      w_fill_y = false;
      w_size_modified = true;
    }
  in
  object (self)
  
  inherit WX_base.t window s

  initializer 
    w.w_button_press <- (fun _ -> self#handle_button ());
    w.w_key_press <- (fun _ -> self#handle_key ());
    Eloop.add_after_events_hook (fun () -> self#update);

  val mutable default_font = font
  method update_top_size = ()
  method wait_refresh clear x y dx dy = ()
  method default_font = default_font
  method wait_resize = ()
  method container_add (o : contained) = ()
  method geometry = w.w_geometry
end

class from_display dpyname num =
  let display = new WX_display.t dpyname in
  object (self)
  inherit t display num
end