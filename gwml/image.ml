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
open Gwml_args
open Gwml
open Wob
open Imager


class image s filename =
  
  object (self)
  inherit wob_base
  
  val mutable mask = noPixmap
  val mutable pixmap = noPixmap
  val mutable x_ext = 0
  val mutable y_ext = 0
  val mutable filename = filename
  
  method compute_pixmap file =
    filename <- file;
    let w = self#wob in
    ungrab_exec (fun _ ->
        if file = filename then
          let image = image_load filename in
          
          let g = w.w_geometry in
          if pixmap <> noPixmap then
            pixmap_free pixmap;
          let pix = image_pixmap image g.width g.height in
          pixmap <- pix.pixmap;
          mask <- pix.mask;
          if w.w_window <> noWindow then
            X.clearArea display w.w_window 0 0 0 0 true;
          image_destroy image
          )
    
  method set_x_ext bool =
    x_ext <- if bool then 1 else 0
  
  method set_y_ext bool = 
    y_ext <- if bool then 1 else 0
  
  method set_image file =
    filename <- file;
    self#compute_pixmap file;
    if not !server_grabbed then
      match wob with None -> () | Some w ->
          Wob.send_one w.w_top WobGetSize;
          Wob.send_one w WobRefresh        

  method first_hook e =
    let w = self#wob in
    match e with
    | WobGetSize ->
        let g = w.w_geometry in
        g.x <- x_ext;
        g.y <- y_ext;
        g.width <- self#min_width;
        g.height <- self#min_height;
        
      | WobCreate ->
        let g = w.w_geometry in
        self#create true;
        X.mapWindow display w.w_window;
        self#compute_pixmap filename
        
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
            self#compute_pixmap filename
          end
          
    | WobRefresh ->
        let s = w.w_screen in
        let gc = X.createGC display s.s_scr.scr_root
            [GCforeground (color_make w foreground);
            GCbackground (color_make w self#background)]
        in
        
        let g = w.w_geometry in
        
        X.clearArea display w.w_window 0 0 0 0 false;
        if mask <> noPixmap then
          X.changeGC display gc 
          [GCclip_mask mask; GCclip_y_origin 0; GCclip_x_origin 0];
        X.copyArea display gc pixmap 0 0 
          w.w_window 0 0 g.width g.height;
        X.freeGC display gc      
    | WobKeyPress (e,s,k) -> self#handle_key (e,s,k)
    | WobButtonPress e ->  self#handle_button e        
    | _ -> ()
end

let make s file = new image s file
  