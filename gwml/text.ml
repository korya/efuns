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

class text string =
  object (self)
    inherit wob_base
    
    val mutable text = string
    
    val mutable max_ascent = 0
    val mutable max_descent = 0
    val mutable max_width = 0
    
    method size =
      max_width <- 0;
      max_descent <- 0;
      max_ascent <- 0;
      
      let w = self#wob in
      let (font,qf) = font_make w font in 
      Array.iter (fun string ->
          let ci = Xtext.extents qf string in
          max_width <- max max_width ci.char_width;
          max_ascent <- max max_ascent ci.char_ascent;
          max_descent <- max max_descent ci.char_descent;
      ) text
    
    method text = string
    method set_text s = 
      if s <> text then
        begin
          text <- s; 
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
    
    method  first_hook e =
      let w = self#wob in
      let s = w.w_screen in
      match e with
      | WobGetSize ->
          let g = w.w_geometry in
          self#size;
          g.width <- max (max_width + 4) self#min_width;
          g.height <- max (
            Array.length text * (max_ascent + max_descent +2))
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
        (*** Bad: we use two different methods to compute the size needed and
        to display the text ! *)
        let max_bounds =  qf.qf_info.font_max_bounds in
        let height =  max_bounds.char_ascent + max_bounds.char_descent + 2 in
        let y = ref (max_bounds.char_ascent + 2) in
        let x = 2 in
        Array.iter (fun string ->
            Xlib.drawString display w.w_window gc x !y string;
            y := !y + height
        ) text;
        X.freeGC display gc      
        
end

let make string = new text string
let desc wob = (wob :> wob_desc)  
