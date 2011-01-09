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

  (* Verifier que le parent est bien la fenetre racine de l'ecran. *)

class orig (root : WX_root.t) pos attributes =
  
  object (self)
  inherit WX_object.t root#container
    (match pos with
      None -> attributes | Some (x,y) -> (Position (x, y)):: attributes) 
  as super
  
  val mutable resized = false
  
  initializer 
    match pos with
      None -> 
        w.w_mask <- StructureNotifyMask :: w.w_mask;
        w.w_override <- false
    | Some (x,y) ->
        w.w_override <- true;
  
  val mutable widget = None
  
  method top = self#container
  method container_add (wob : contained) = 
    wob#set_parent self#container;
    if w.w_shown then
      begin
        (match widget with None -> () | Some widget -> widget#hide);
        wob#show;
        widget <- Some wob;
        self#wait_resize;
      end
    else
      widget <- Some wob;
  
  method update_top_size = self#update_size
  
  method update_size =
    (* FIRST, compute the size requested by the widget *)
    let g = w.w_geometry in
    let width, height = 
      match widget with
        None -> 1,1
      | Some widget -> 
          let sz = widget#size_request in
          let width = (min  sz.max_width
                (max sz.requested_width sz.min_width))
            + 2* (sz.border_width + w.w_ipad_x) in
          let height = (min sz.max_height
                (max sz.requested_height sz.min_height))
            + 2* (sz.border_width + w.w_ipad_y) in
          width, height
    in
    (* SECOND, conform to own size hints *)
    let tsz = szhints in
    let width = min tsz.max_width (max tsz.min_width width) in
    let height = min tsz.max_height (max tsz.min_height height) in
    
    (* THIRD, conform to screen size *)
    let width = min width s.s_screen.scr_width in
    let height = min height s.s_screen.scr_height in
    
    (* FIFTH, allocate the new size *)
    self#size_allocate 0 0 width height;
  
  method size_allocate x y dx dy =
  (* do not use super#size_allocate to avoid moving the window. *)
    
    let g = w.w_geometry in
    let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
    w.w_size_modified <- false;
    if modified then
      let wg = w.w_geometry in
      let s = self#screen in
      wg.width <- dx;
      wg.height <- dy;
      if not (w.w_window == noWindow) then
        if not resized then
          Xlib.resizeWindow s.s_display w.w_window dx dy
        else
          resized <- false;
    (* Allocate the widget size *)
      match widget with None -> () | Some widget ->
          let sz = widget#size_request in
          let ipx = w.w_ipad_x + sz.border_width in
          let ipy = w.w_ipad_y + sz.border_width in
          widget#size_allocate ipx ipy (dx-2*ipx) (dy-2*ipy)
          
  method realize =
    super#realize;
    match widget with None -> () | Some wob -> wob#realize
  
  method destroy =
    (match widget with None -> () | Some wob -> wob#destroy);
    super#destroy;
  
  method show =
    if not w.w_shown then
      begin
        if w.w_window == noWindow then 
          begin
            self#update_top_size;
            self#realize;
          end;
        (match widget with None -> () | Some wob -> wob#show);
        X.mapWindow s.s_display w.w_window;
        w.w_shown <- true;
      end;
    Xlib.raiseWindow s.s_display w.w_window
  
  method change_position x y  = self#configure [Position (x,y)]
  
  method name = "top"
  
    (* Methode rajoutee juste pour les menus. A generaliser ou supprimer *)
  method grab_pointer = 
    X.grabPointer s.s_display w.w_window true [ButtonReleaseMask]
      GrabModeAsync GrabModeAsync noConfineTo Xtypes.noCursor currentTime
  
  method xevents ev =
    (match ev.ev_event with
      | ConfigureNotifyEvent e when not ev.ev_sent -> 
          (* Here, we must be very careful. Several resize commands may have
          been issued, and we must not change the size unless it is a 
          user choice. *)

          (*let g = w.w_geometry in
          g.x <- -1;
          g.width <- e.Xconfigure.width;
          g.height <- e.Xconfigure.height;
      self#wait_resize*)
          let g = w.w_geometry in
          if g.width <> e.Xconfigure.width ||
            g.height <> e.Xconfigure.height then
            begin
              resized <- true;
              self#size_allocate 0 0 e.Xconfigure.width e.Xconfigure.height;
            end;
      
      | _ -> ());
    super#xevents ev
  
  method wait_resize =
    if not w.w_size_modified then
      begin
        w.w_size_modified <- true;
        s.s_wait_resize <- self#to_resize :: s.s_wait_resize
      end
end

class t = orig
  
class outtop root window_id attributes = object
    inherit orig root None attributes
    
    method realize = ()
end