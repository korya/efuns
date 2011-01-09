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
open Options
open Xtypes
open Gwml
open Wob

let set_shape w mask dx dy =
  let module S = Shape in
  let module X = S.X in
  let g = w.w_geometry in
  let x = max ((g.width - dx)/2) 0 in
  let y = max ((g.height - dy)/2) 0 in
  if mask <> noPixmap then
    X.shapeMask display w.w_window S.Bounding x y mask S.Set
  else
    X.shapeRectangles display w.w_window S.Bounding 0 0 [
      x, y, dx, dy] S.Set UnSorted
    
class pixmap s =
  object (self)
    inherit wob_base
    
    val mutable dy = 5
    val mutable dx = 5
    val mutable pixmap = noPixmap
    val mutable mask = noPixmap
    val mutable bitmap = false
    
    method set_pixmap descr =
      ungrab_exec (fun _ ->
          let (dx',dy',depth',pix',mask') = pixmap_make s descr in
          dx <- dx'; dy <- dy'; bitmap <- (depth' = 1);
          pixmap <- pix'; mask <- mask';
          self#set_min_width dx;
          self#set_min_height dy;
          match wob with None -> () | Some w ->
              Wob.send_one w.w_top WobGetSize;
              Wob.send_one w.w_top WobUpdateShape;
              Wob.send_one w WobRefresh)
    
    
    method first_hook e =
      let w = self#wob in
      match e with
      | WobGetSize ->
          let g = w.w_geometry in
          g.x <- 0;
          g.y <- 0;
          g.width <- max dx self#min_width;
          g.height <- max dy self#min_height;
      
      | WobCreate ->
          let g = w.w_geometry in
          self#create true; 
          X.mapWindow display w.w_window;
      
      | WobUpdateShape ->
          if is_shaped then set_shape w mask dx dy
      
      | WobDestroy -> 
          if w.w_window <> noWindow then
            let s = w.w_screen in
            X.destroyWindow display w.w_window;
            Eloop.remove_window s.s_scheduler w.w_window;
            w.w_window <- noWindow
      
      | WobResize _ -> 
          let g = w.w_geometry in
          if w.w_window <> noWindow then begin
              Xlib.moveResizeWindow display w.w_window g.x g.y
                (max 1 g.width) (max 1 g.height);
            end
      
      | WobRefresh ->
          let s = w.w_screen in
          let gc = X.createGC display s.s_scr.scr_root
              [GCforeground (color_make w foreground);
              GCbackground (color_make w self#background)]
          in
          
          let g = w.w_geometry in
          let x = max ((g.width - dx)/2) 0 in
          let y = max ((g.height - dy)/2) 0 in
          
          X.clearArea display w.w_window 0 0 0 0 false;
          if not bitmap then
            begin
              if mask <> noPixmap then
                X.changeGC display gc 
                  [GCclip_mask mask; GCclip_y_origin x; GCclip_x_origin y];
              X.copyArea display gc pixmap 0 0 
                w.w_window x y self#min_width self#min_height
            end
          else
            X.copyPlane display gc pixmap 0 0
              w.w_window x y self#min_width self#min_height 1;
          X.freeGC display gc      
      | WobKeyPress (e,s,k) -> self#handle_key (e,s,k)
      | WobButtonPress e ->  self#handle_button e
      | _ -> ()
end

let make s descr = 
  let pixmap = new pixmap s in
  pixmap#set_pixmap descr;
  pixmap
  