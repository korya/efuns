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

  
class t parent sens (adj : WX_adjust.t) attributes =
  
  object (self)
  inherit WX_object.t parent 
    ((match sens with
        Horizontal ->  ExpandX  true;
      | Vertical->     ExpandY true) ::
    [ MinWidth 20; MinHeight 20; Relief ReliefSunken] @ attributes) as super

  val mutable activated = false
    
  val mutable button = None
    method timer t b f =
    Eloop.add_timer s.s_eloop t (fun _ ->
        match button with
          Some button when b = button -> 
            self#timer !autorepeat_rate b f; f (); 
        | _ -> ()
    )

  val mutable scale_win = noWindow
    
  initializer 
    adj#add_subject self#update_adjustement;
    self#configure [Bindings [
        Button (1,0), (fun _ -> 
            let adj = adj in
            let g = w.w_geometry in
            self#timer !autorepeat_delay 1 (fun _ -> adj#up);
            button <- Some 1;
            adj#up);
        Button (2,0), (fun _ -> 
            X.changeActivePointerGrab s.s_display 
              [ButtonMotionMask; ButtonReleaseMask;
              ButtonPressMask; OwnerGrabButtonMask]
              Xtypes.noCursor !Eloop.event_time;
            activated <- true;
            let adj = adj in
            let g = w.w_geometry in
            let pos, total = match sens with
                Vertical -> !mouse_y_event, g.height
              | Horizontal -> !mouse_x_event, g.width
            in
            adj#set_pos pos total);
        ButtonMotion, (fun _ -> 
            if activated then
              let adj = adj in
              let g = w.w_geometry in
              let pos, total = match sens with
                  Vertical -> !mouse_y_event, g.height
                | Horizontal -> !mouse_x_event, g.width
              in
              adj#set_pos pos total);
        ButtonReleased, (fun _ -> 
            activated <- false;
            button <- None;);
        Button (3,0), (fun _ -> 
            let adj = adj in
            let g = w.w_geometry in
            self#timer !autorepeat_delay 3 (fun _ -> adj#down);
            button <- Some 3;
            adj#down)
      ]; Cursor (FontCursor XC.xc_arrow)]
  
  val adj = adj
  val sens = sens
  
  method size_request = 
    let sz = szhints in
    if not w.w_size_modified || sz.comp_timestamp = s.s_timestamp then sz else
      begin
        sz.comp_timestamp <- s.s_timestamp;
        sz.requested_height <- max sz.min_height sz.requested_height;
        sz.requested_width <- max sz.min_width sz.requested_width;
        sz
      end
    
  method refresh =
    super#refresh;
    if not (w.w_window == noWindow) then
      let adj = adj in
      if scale_win == noWindow then
        begin
          scale_win <- X.createWindow s.s_display w.w_window
            2 2 16 16 copyDepthFromParent InputOutput copyVisualFromParent
            0 [CWBackPixel w.w_foreground.c_pixel];
          X.mapWindow s.s_display scale_win
        end;
      let g = w.w_geometry in      
      let gc = GCCache.get2 s.s_gcs 
          w.w_foreground.c_pixel w.w_background.c_pixel in
      match sens with   
          Vertical -> 
            let y = adj#get_pos g.height in
            Xlib.moveWindow s.s_display scale_win 2 y
        | Horizontal ->
            let y = adj#get_pos g.width in
            Xlib.moveWindow s.s_display scale_win y 2

  method size_allocate x y dx dy = 
    super#size_allocate x y dx dy
      
  method update_adjustement () = 
    self#wait_refresh false 0 0 0 0
      
  method name = "scale"
end

class h parent adj attributes =
  object
  inherit t parent Horizontal adj attributes
end

class v parent adj attributes =
  object
  inherit t parent Vertical adj attributes 
end
