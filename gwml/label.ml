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
open Gwml
open Wob

class label string =
  object (self)
  inherit wob_base
  
  val mutable string = string
  val mutable justified = Left
    
  method set_justified j =
    justified <- j;
    match wob with
      None -> ()
    | Some w -> Wob.send_one w WobRefresh
        
  method string = string
  method set_string s = 
    if s <> string then
      begin
        string <- s; 
        match wob with
          None -> ()
        | Some w -> 
            Wob.send_one w.w_top WobGetSize;
            Wob.send_one w WobRefresh
      end
  
  method set_font f = 
    if f <> font then
      begin
        font <- f;
        match wob with
          None -> ()
        | Some w -> 
            Wob.send_one w.w_top WobGetSize;
            Wob.send_one w WobRefresh
      end

    val mutable max_width = 1000
    method set_max_width n =
      max_width <- n
        
  method  first_hook e =
    let w = self#wob in
    let s = w.w_screen in
    match e with
    | WobGetSize ->
        let (font,qf) = font_make w font in 
        let char_info = Xtext.extents qf string in
        let g = w.w_geometry in
          g.width <- max (char_info.char_width + 4) self#min_width;
          g.width <- min g.width max_width;
        g.height <- max (
          char_info.char_ascent + char_info.char_descent + 4)
        self#min_height;
        g.x <- extensible_width;
        g.y <- extensible_height;
    | WobCreate ->
        self#create true;
        self#update_bg;
        X.mapWindow display w.w_window;
    | WobDestroy -> 
        if w.w_window <> noWindow then
          begin
            Eloop.remove_window s.s_scheduler w.w_window;
            X.destroyWindow display w.w_window;
            w.w_window <- noWindow
          end
    | WobResize _ -> 
        if w.w_window <> noWindow then
          let g = w.w_geometry in
          Xlib.moveResizeWindow display w.w_window g.x g.y
            (max 1 g.width) (max 1 g.height);
          self#resized
    | WobRefresh -> self#update_fg
    | WobKeyPress (e,s,k) -> self#handle_key (e,s,k)
    | WobButtonPress e ->  self#handle_button e
    | _ -> ()
  
  method update_fg =
    let w = self#wob in
    if w.w_window <> noWindow then
      let s = w.w_screen in
      let (font,qf) = font_make w font in
      let gc = X.createGC display s.s_scr.scr_root 
          [GCforeground (color_make w self#fg);
          GCbackground (color_make w self#bg); 
          GCfont font] in
      X.clearArea display w.w_window 0 0 0 0 false;
      let max_bounds =  qf.qf_info.font_max_bounds in
      let height =  max_bounds.char_ascent + max_bounds.char_descent in
      let offset = (w.w_geometry.height - height)/2 in
      let y = max_bounds.char_ascent + offset in
      let width = (Xtext.extents qf string).char_width in
      let g = w.w_geometry in
      let x = match justified with
          Left -> 2
        | Center -> (g.width - width)/2
        | Right -> max (g.width -width -2) 2
      in
      Xlib.drawString display w.w_window gc x y string;
      X.freeGC display gc      
      
end

let make string = new label string
let desc wob = (wob :> wob_desc)  
