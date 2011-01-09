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

class t parent attributes =
  object (self)
  
  inherit WX_object.t parent attributes as super
  
    val mutable window = noWindow
      
    method container_add (win : Xtypes.window) =
      window <- win;
      if w.w_window = noWindow then
        self#reparent

    method private reparent = 
      let g = w.w_geometry in
      X.reparentWindow s.s_display window w.w_window 2 2;
      Xlib.resizeWindow s.s_display window
        (g.width - 4) (g.height - 4);
      X.mapWindow s.s_display window
        
  method size_request =
    let sz = szhints in
    if not w.w_size_modified || sz.comp_timestamp = s.s_timestamp then sz else
      begin
          sz.comp_timestamp <- s.s_timestamp;
          sz
        end
        
  method size_allocate x y dx dy =
    w.w_size_modified <- false;
    let g = w.w_geometry in
    let modified = not (g.width = dx && g.height = dy) in
    super#size_allocate x y dx dy;
      if modified && not (window == noWindow) then
        let g = w.w_geometry in
        Xlib.moveResizeWindow s.s_display window
        2 2 (g.width - 4) (g.height - 4)

    method destroy =
      if not (window == noWindow) then 
        X.reparentWindow s.s_display window s.s_screen.scr_root 0 0;
      super#destroy
    
  method realize =
      super#realize;
      if not (window == noWindow) then self#reparent
    
  method show = 
      super#show;
      if not (window == noWindow) then
        X.mapWindow s.s_display window

end
    