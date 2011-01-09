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

class orig parent descr attributes =
  let (dx,dy,depth,pix,mask) = parent#pixmap_make descr in
  
  object (self)
  inherit WX_object.t parent attributes as super
  
  val mutable dy = dy
  val mutable dx = dx
  val mutable pixmap = pix
  val mutable mask = mask
  val mutable bitmap = (depth = 1)
  
  method set_pixmap descr =
    let (dx',dy',depth',pix',mask') = self#pixmap_make descr in
    dx <- dx'; dy <- dy'; bitmap <- (depth' = 1);
    pixmap <- pix'; mask <- mask';
    self#update_top_size;
    self#wait_refresh true 0 0 0 0
  
  method size_request =
    let sz = szhints in
    if not w.w_size_modified || sz.comp_timestamp = s.s_timestamp then sz else
      begin
        sz.comp_timestamp <- s.s_timestamp;
        sz.requested_width <- max (dx+4+2* w.w_ipad_x) sz.min_width;
        sz.requested_height <- max (dy+4+ 2*w.w_ipad_y) sz.min_height;
        sz
      end
      
  method refresh =
    if s.s_timestamp > w.w_refresh_timestamp && not (w.w_window == noWindow) then    
      begin
        super#refresh;
        let gc = X.createGC s.s_display s.s_screen.scr_root
            [GCforeground w.w_foreground.c_pixel;
            GCbackground w.w_background.c_pixel]
        in
        let g = w.w_geometry in
        let sz = szhints in
        let x = max ((g.width - dx)/2) w.w_ipad_x in
        let y = max ((g.height - dy)/2) w.w_ipad_y in
        
        if not bitmap then
          begin
            if mask <> noPixmap then
              X.changeGC s.s_display gc 
                [GCclip_mask mask; GCclip_y_origin x; GCclip_x_origin y];
            X.copyArea s.s_display gc pixmap 0 0 
              w.w_window x y dx dy
          end
        else
          X.copyPlane s.s_display gc pixmap 0 0
            w.w_window x y dx dy 1;
        X.freeGC s.s_display gc;
      end  
  method name = "pixmap"
end

class t = orig