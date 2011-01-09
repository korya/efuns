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

let next_id = ref 0
  
let new_szhints _ =
  {
    border_width = 0;
    requested_width = 1;
    requested_height = 1;
    min_width = 1;
    min_height = 1;
    max_width = max_int;
    max_height = max_int;
    expand_x = false;
    expand_y = false;
    retract_x = false;
    retract_y = false;
    inc_x = 0;
    inc_y = 0;
    comp_timestamp = -1; (* Never computed *)
  } 

class t (parent : container) attributes =
  let window = { 
      w_clear = false;
      w_window = noWindow;
      w_shown = false;
      w_geometry = { x= 0; y= 0; border=0; width = 1; height = 1; } ;
      w_override = true;
      w_background = parent#color_make !default_background true;
      w_foreground = parent#color_make !default_foreground true;
      w_relief_colors = None;
      w_relief = ReliefFlat;
      w_borderpixel = noColor;
      w_cursor = noCursor;
      w_mask =   [
        ExposureMask; FocusChangeMask;
      ];
      w_size_timestamp = -1;
      w_refresh_timestamp = -1;
      w_inverse = false;
      w_enter_window = null_handler;
      w_leave_window = null_handler;
      w_button_released = null_handler;
      w_button_press = null_handler;
      w_key_press = null_handler;
      w_button_motion = null_handler;
      w_focus_out = null_handler;
      w_focus_in = null_handler;
      w_actions = [];
      w_ipad_x = 0;
      w_ipad_y = 0;
      w_fill_x = true;
      w_fill_y = true;
      w_size_modified = true;
    }
  in
  let szhints = new_szhints () in
  object (self)
    
    inherit WX_base.t window parent#screen
    
    initializer 
      w.w_button_press <- (fun _ -> self#handle_button ());
      w.w_key_press <- (fun _ -> self#handle_key ());
      self#configure attributes
    
    val id = incr next_id; !next_id
    val mutable parent = parent
    val mutable szhints = szhints
    method parent = parent
    method size_request = 
      let sz = szhints in
      sz.requested_width <- max sz.requested_width sz.min_width;
      sz.requested_height <- max sz.requested_height sz.min_height;
      sz
    
    method set_parent p = 
      assert (w.w_window == noWindow || parent == p);
      parent <- p
    
    method realize =
      let s = parent#screen in
      let cwa = [CWEventMask w.w_mask;
          CWOverrideRedirect w.w_override] in
      let cwa = let b = w.w_background in
        if b == noColor then cwa else 
          (CWBackPixel b.c_pixel) :: cwa in
      let cwa = let b = w.w_borderpixel in
        if b == noColor then cwa else
          (CWBorderPixel b.c_pixel):: cwa in
      let cwa = let c = w.w_cursor in
        if c == noCursor then cwa else (CWCursor c.curs_id):: cwa 
      in
      w.w_window <- X.createWindow s.s_display parent#window 
        w.w_geometry.x w.w_geometry.y 
        (max w.w_geometry.width 1) (max w.w_geometry.height 1)
      copyDepthFromParent InputOutput copyVisualFromParent 
        w.w_geometry.border cwa;
      Eloop.add_window s.s_eloop w.w_window (self#xevents);
      set_grabs s.s_display w.w_window self#actions;
      self#iter (fun o -> o#realize)
    
    method iter (f : contained -> unit) = ()
    method iter_visible f = self#iter f  
    
    method size_allocate x y dx dy =
      w.w_size_modified <- false;    
      let wg = w.w_geometry in
      let s = self#screen in
      let sz = szhints in
      if not w.w_fill_x && dx > sz.requested_width then
        begin
          let plus = dx - sz.requested_width in
          wg.x <- x + plus/2;
          wg.width <- dx - plus;        
        end
      else 
        begin
          wg.x <- x;
          wg.width <- dx;
        end;
      if not w.w_fill_y && dy > sz.requested_height then
        begin
          let plus = dy - sz.requested_height in
          wg.y <- y + plus/2;
          wg.height <- dy - plus;        
        end
      else 
        begin
          wg.y <- y;
          wg.height <- dy;
        end;
      w.w_size_timestamp <- s.s_timestamp;
      if not (w.w_window == noWindow) then
        Xlib.moveResizeWindow s.s_display w.w_window wg.x wg.y wg.width wg.height
    
    method destroy =
      if not (w.w_window == noWindow) then
        let s = self#screen in
        self#iter (fun o -> o#destroy);
        X.destroyWindow s.s_display w.w_window;
        Eloop.remove_window s.s_eloop w.w_window;      
        w.w_window <- noWindow
    
    method show =
      if not w.w_shown then
        begin
          if w.w_window == noWindow then self#realize;
          X.mapWindow s.s_display w.w_window;
          w.w_shown <- true;
          self#iter_visible (fun o -> o#show)
        end
    
    method hide =
      if not (w.w_window == noWindow) && w.w_shown then
        begin
          w.w_shown <- false;
          X.unmapWindow s.s_display w.w_window;
          self#iter_visible (fun o -> o#hide)
        end
    
    method refresh = 
      if s.s_timestamp > w.w_refresh_timestamp && not (w.w_window == noWindow) then
        let s = self#screen in
        w.w_refresh_timestamp <- s.s_timestamp;
        begin
          match w.w_relief_colors with Some a -> () | None ->
              let bg_color = w.w_background in
              X.changeWindowAttributes s.s_display w.w_window
                [CWBackPixel bg_color.c_pixel];
              let shadow = self#getShadow bg_color in
              let hilite = self#getHilite bg_color in           
              let a = (shadow, hilite) in
              w.w_relief_colors <- Some a; ()
        end;
        if w.w_clear then (X.clearWindow s.s_display w.w_window; 
            w.w_clear <- false);
        self#draw_relief
    
    method draw_relief = 
      let g = w.w_geometry in
      let bg_color = w.w_background in
      let shadow, hilite =
        match w.w_relief_colors with Some a -> a | None ->
            let bg_color = w.w_background in
            X.changeWindowAttributes s.s_display w.w_window
              [CWBackPixel bg_color.c_pixel];
            let shadow = self#getShadow bg_color in
            let hilite = self#getHilite bg_color in           
            let a = (shadow, hilite) in
            w.w_relief_colors <- Some a; a
      in
      match w.w_relief with
        ReliefFlat -> ()
      | r ->
          drawRelief s.s_display w s.s_gcs hilite.c_pixel shadow.c_pixel r
    
    method update_top_size = self#update_size
    method update_size = 
      if s.s_timestamp > w.w_size_timestamp then
        parent#update_top_size
    method contained = (self :> contained)
    
    method configure attributes =
      let sz = szhints in
      let w = w in
    (* The window has already been created *)
      
      (* The problem here is that we don't want to modify several times
      all the toplevels. Instead, such modifications should only be taken into
      account when the root#update function is called. 
    *)
      let old_grabs = w.w_actions in
      let size_changed = ref false in
      let draw_changed = ref false in
      let mask_changed = ref false in
      let grab_changed = ref false in
      List.iter (fun t ->
          match t with
          | MinWidth int -> size_changed := true; sz.min_width <- int
          | MinHeight int -> size_changed := true;sz.min_height <- int
          | MaxWidth int -> size_changed := true; sz.max_width <- int
          | MaxHeight int ->  size_changed := true; sz.max_height <- int
          | ExpandX bool ->  size_changed := true; sz.expand_x <- bool
          | ExpandY bool ->  size_changed := true; sz.expand_y <- bool
          | FillX bool ->  size_changed := true; w.w_fill_x <- bool
          | FillY bool ->  size_changed := true; w.w_fill_y <- bool
          | IncX int ->  size_changed := true; sz.inc_x <- int
          | IncY int ->  size_changed := true; sz.inc_y <- int
          | IpadX int ->  size_changed := true; w.w_ipad_x <- int
          | IpadY int ->  size_changed := true; w.w_ipad_y <- int
          | RetractX bool -> size_changed := true; sz.retract_x <- bool
          | RetractY bool -> size_changed := true; sz.retract_y <- bool
          | Position (x,y) ->
              let g = w.w_geometry in
              g.x <- x; g.y <- y;
              if not (w.w_window == noWindow) then
                X.configureWindow s.s_display w.w_window [CWX x; CWY y]
          | Background bg -> draw_changed := true;
              w.w_background <- parent#color_make bg true;
              w.w_relief_colors <- None;
          | Relief r -> draw_changed := true; 
              w.w_relief <- r
          | Cursor c ->
              w.w_cursor <- parent#cursor_make c true;
              if not (w.w_window == noWindow) then
                X.changeWindowAttributes s.s_display w.w_window 
                  [CWCursor w.w_cursor.curs_id]
          | BorderColor color ->            
              w.w_borderpixel <- parent#color_make color true;
              if not (w.w_window == noWindow) then
                X.changeWindowAttributes self#display w.w_window 
                  [CWBorderPixel w.w_borderpixel.c_pixel]
          | BorderWidth border ->   
              sz.border_width <- border;
              w.w_geometry.border <- border;
              if not (w.w_window == noWindow) then
                X.configureWindow s.s_display w.w_window [CWBorderWidth border];
              size_changed := true
          | EventMask (op,mask) -> 
              w.w_mask <- (match op with
                  Append -> mask @ w.w_mask;
                | Replace -> mask
                | Remove -> List.fold_left (fun mask ev ->
                        Utils.list_removeq mask ev) w.w_mask mask);
              mask_changed := true
          | Foreground fg ->
              w.w_foreground <- parent#color_make fg true;
              draw_changed := true
          | Bindings list ->
              List.fold_right (fun (event, handler) () ->
                  match event with
                    EnterWindow -> 
                      let old = w.w_enter_window in
                      w.w_enter_window <- (fun _ -> old (); handler ());
                      if not (List.memq EnterWindowMask w.w_mask) then
                        w.w_mask <- EnterWindowMask :: w.w_mask
                  | LeaveWindow -> 
                      let old = w.w_leave_window in
                      w.w_leave_window <- (fun _ -> old (); handler ());
                      if not (List.memq LeaveWindowMask w.w_mask) then
                        w.w_mask <- LeaveWindowMask :: w.w_mask
                  | ButtonReleased ->
                      let old = w.w_button_released in
                      w.w_button_released <- (fun _ -> old (); handler ());
                      if not (List.memq ButtonReleaseMask w.w_mask) then
                        w.w_mask <- ButtonReleaseMask :: w.w_mask;
                  | FocusIn ->
                      let old = w.w_focus_in in
                      w.w_focus_in <- (fun _ -> old (); handler ());
                      if not (List.memq FocusChangeMask w.w_mask) then
                        w.w_mask <- FocusChangeMask :: w.w_mask;
                  | FocusOut ->
                      let old = w.w_focus_out in
                      w.w_focus_out <- (fun _ -> old (); handler ());
                      if not (List.memq FocusChangeMask w.w_mask) then
                        w.w_mask <- FocusChangeMask :: w.w_mask;
                  | ButtonMotion ->
                      let old = w.w_button_motion in
                      w.w_button_motion <- (fun _ -> old (); handler ());
                      if not (List.memq ButtonMotionMask w.w_mask) then
                        w.w_mask <- 
                          ButtonMotionMask :: OwnerGrabButtonMask 
                          :: w.w_mask                      
                  | KeyPress ->
                      let old = w.w_key_press in
                      w.w_key_press <- (fun _ -> old (); handler ());
                      if not (List.memq KeyPressMask w.w_mask) then
                        w.w_mask <- KeyPressMask :: KeyReleaseMask :: w.w_mask;
                  | ButtonPress ->
                      let old = w.w_button_press in
                      w.w_button_press <- (fun _ -> old (); handler ());
                      if not (List.memq ButtonPressMask w.w_mask) then
                        w.w_mask <- ButtonPressMask :: ButtonReleaseMask :: w.w_mask;
                  | Key _ ->
                      w.w_actions <- (event, handler) :: w.w_actions;
                      if not (List.memq KeyPressMask w.w_mask) then
                        w.w_mask <- KeyPressMask :: KeyReleaseMask
                          :: w.w_mask                      
                  | Button _ ->
                      w.w_actions <- (event, handler) :: w.w_actions;
                      if not (List.memq ButtonPressMask w.w_mask) then
                        w.w_mask <- ButtonPressMask :: ButtonReleaseMask
                          :: w.w_mask
                  | GrabbedKey _ ->
                      w.w_actions <- (event, handler) :: w.w_actions;
                      if not (List.memq KeyPressMask w.w_mask) then
                        w.w_mask <- KeyPressMask :: KeyReleaseMask
                          :: w.w_mask;
                      grab_changed := true;
                  | GrabbedButton _ ->
                      w.w_actions <- (event, handler) :: w.w_actions;
                      if not (List.memq ButtonPressMask w.w_mask) then
                        w.w_mask <- ButtonPressMask :: ButtonReleaseMask
                          :: w.w_mask;                    
                      grab_changed := true;
                  | _ ->
                      Printf.printf "WARNING: unuse binding"; print_newline () 
              ) list ();
              mask_changed := true;
          | _ -> 
              Printf.printf "WARNING: unuse attribute"; print_newline ();
      ) attributes;
      if not (w.w_window == noWindow) then
        begin
          if !mask_changed then
            X.changeWindowAttributes s.s_display w.w_window
              [CWEventMask w.w_mask];      
          if !grab_changed then
            begin
              unset_grabs s.s_display w.w_window w.w_actions;
              set_grabs s.s_display w.w_window w.w_actions;
            end;
          if !size_changed then self#wait_resize;
          if !draw_changed then self#wait_refresh true 0 0 0 0;
        end
    
    method focus =
      if not (w.w_window == noWindow) then
        X.setInputFocus s.s_display w.w_window RevertToPointerRoot
          currentTime 
(* Don't know why, but specifying a time does not work with WX_filesel ..
      (Eloop.last_time s.s_eloop) 
        *)
    
    method wait_refresh clear x y dx dy = 
      if w.w_shown then
        (w.w_clear <- true;
          s.s_wait_refresh <- self#to_refresh :: s.s_wait_refresh)
    
    method wait_resize =
      w.w_size_modified <- true;
      szhints.comp_timestamp <- 0;
      parent#wait_resize
    
    method to_refresh = (self :> refresh_widget)
    method to_resize = (self :> resize_widget)
    
    
    method reverse =
      w.w_inverse <- not w.w_inverse;
      let bg = w.w_background in
      let fg = w.w_foreground in
      w.w_background <- fg;
      w.w_foreground <- bg;
      w.w_relief_colors <- None;
      self#wait_refresh true 0 0 0 0;
      w.w_inverse
    
    method normal = if w.w_inverse then let _ = self#reverse in ()
    method inverse = if not w.w_inverse then let _ = self#reverse in ()
    method default_font = parent#default_font          
    method geometry = w.w_geometry
    method id = self#name ^ ":" ^(string_of_int id)
end

(* HOW WIDGETS ARE RESIZED:

method update_size : unit ---> 
  Computes the new size of the widget. Call parent#update_top_size in most
  contained widgets.
  
method update_top_size : unit --->
  Computes the new size of the widget when an inferior size changed. Call
  self#update_size in most container widgets.

  In top widgets, call child#sizes for children to compute their sizes,
    and child#resize with the final size.

*)