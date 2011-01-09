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
  
  val mutable subwin = noWindow
  
  val mutable button = None
  method timer t b f =
    Eloop.add_timer s.s_eloop t (fun _ ->
        match button with
          Some button when b = button -> 
            self#timer !autorepeat_rate b f; f ();
        | _ -> ()
    )
  
  val mutable activated = false
  initializer 
    adj#add_subject self#update_adjustement;
    self#configure [Bindings [
        Button (1,0), (fun _ -> 
            let adj = adj in
            let g = w.w_geometry in
            self#timer !autorepeat_delay 1 (fun _ -> adj#page_up);
            button <- Some 1;            
            adj#page_up);
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
            self#timer !autorepeat_delay 3 (fun _ -> adj#page_down);
            button <- Some 3;            
            adj#page_down)
      ]; Cursor (FontCursor 
          (if sens = Vertical then XC.xc_sb_v_double_arrow else
            XC.xc_sb_h_double_arrow))]
  
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
    if s.s_timestamp > w.w_refresh_timestamp && not (w.w_window == noWindow) then
      begin
        super#refresh;
        if not (w.w_window == noWindow) then
          let adj = adj in
          let g = w.w_geometry in
          if subwin == noWindow then
            begin
              subwin <- X.createWindow s.s_display w.w_window
                2 2 16 16 copyDepthFromParent InputOutput copyVisualFromParent
                0 [CWBackPixel w.w_foreground.c_pixel];
              X.mapWindow s.s_display subwin
            end;
          match sens with   
            Vertical -> 
              let y = adj#get_pos g.height in
              let dy = adj#get_page g.height in
              Xlib.moveResizeWindow s.s_display subwin 
                2 y (g.width - 4) dy
          | Horizontal ->
              let y = adj#get_pos g.width in
              let dy = adj#get_page g.width in
              Xlib.moveResizeWindow s.s_display subwin
                y 2 dy (g.height - 4)
      end
  
  method update_adjustement () = 
    self#wait_refresh false 0 0 0 0
    
  method name = "scrollbar"
end

class h parent adj attributes =
  object
  inherit t parent Horizontal adj attributes
end

class v parent adj attributes =
  object
  inherit t parent Vertical adj attributes 
end
