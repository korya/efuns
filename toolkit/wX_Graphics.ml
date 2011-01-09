(***********************************************************************)
(*                                                                     *)
(*                             WXlib                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* 
The problem is: the XGraphics module is designed to be used by
sequential applications pulling for events. 
*)

open Xtypes
open WX_types
open XGraphics

class t parent attributes dx dy =
  object (self)
  
  inherit WX_object.t parent ((MinWidth 600)::(MinHeight 400)::(Background "white")::attributes) as super
  
  val mutable gv = None
  
  method active = view := gv
  method realize =
    w.w_mask <- default_event_mask;
    super#realize;
    let display = s.s_display in
    let screen = s.s_screen in
    let font =  X.openFont display default_font in
    let depth = screen.scr_root_depth in
    let root = screen.scr_root in
    let white = screen.scr_white_pixel in
    let black = screen.scr_black_pixel in
    let cmap = screen.scr_default_colormap in
    let font = X.openFont display default_font in
    let gc = X.createGC display root [GCbackground white; 
        GCforeground black;
        GCfont font] in
    let window = w.w_window in
    X.changeWindowAttributes display window [CWEventMask default_event_mask];
    let g = w.w_geometry in
    let store = X.createPixmap display root 
        g.width g.height depth in
    let qf = X.queryFont display font in
    let v = {
        display = display;
        screen = screen;
        white = white;
        black = black;
        root = root;
        gc = gc;
        window = window;
        store = store;
        font = font;
        qf = qf;
        descent = qf.qf_info.Xtypes.font_descent;
        ascent = qf.qf_info.Xtypes.font_ascent;
        size_x = g.width;
        size_y = g.height;
        point_x = 0;
        point_y = 0;
        color = 0;
        pixel = black;
        colors = Hashtbl.create 13;
        cmap = cmap;
        pixels = Hashtbl.create 13;
        s_in_events = [];
        s_out_events = [];
        s_keys = [];
        depth = depth;
      } in
    gv <- Some v;
    let d = Eloop.add_display display (fun _ -> ()) in
    Eloop.add_window d window (window_handler v);
    self#active;
    clear_graph ()

  method refresh = ()
    
  method destroy = 
    super#destroy;
    view := None
    
end