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

class plug init getsize draw =
  object (self)
  inherit wob_base 
  
  method  first_hook e =
    let w = self#wob in
    let s = w.w_screen in
    match e with
    | WobInit -> init self
    | WobGetSize ->
        let g = w.w_geometry in
        g.width <- self#min_width;
        g.height <- self#min_height;
        g.x <- 0;
        g.y <- 0;
        getsize self
    | WobCreate ->
        let g = w.w_geometry in
        w.w_window <- X.createWindow display w.w_parent.w_window
          w.w_geometry.x w.w_geometry.y
          w.w_geometry.width w.w_geometry.height
          copyDepthFromParent InputOutput copyVisualFromParent 0 
          [CWEventMask self#mask;
          CWOverrideRedirect true;
          CWBackPixel (color_make w self#background)];
        X.mapWindow display w.w_window;
        Eloop.add_window s.s_scheduler w.w_window self#xevents;      
    | WobDestroy -> 
        if w.w_window <> noWindow then
          begin
            Eloop.remove_window s.s_scheduler w.w_window;
            X.destroyWindow display w.w_window;
            w.w_window <- noWindow;
          end
    
    | WobResize _ -> 
        if w.w_window <> noWindow then
          let g = w.w_geometry in
          Xlib.moveWindow display w.w_window g.x g.y;
          Xlib.resizeWindow display w.w_window 
            (max 1 g.width) (max 1 g.height)
    | WobRefresh ->
        draw self
    | _ -> ()
        
end  
  
  