(***********************************************************************)
(*                                                                     *)
(*                            WXlib                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xtypes
open WX_types

class orig parent attributes =
  object (self)
    
    inherit WX_object.t parent ((Relief ReliefRaised) :: (IpadX 2) :: (IpadY 2) :: attributes) as super
    
    val mutable widget = None
    val mutable action = (fun () -> ())
    val mutable activated = false
    val mutable wait_release = true
    val mutable old_relief = ReliefRaised
    
    method action () = action ()
    
    method iter f = match widget with None -> () | Some o -> f o
    
    method container_add (o : contained) =
      widget <- Some o;
      o#set_parent self#container;
      self#wait_resize
    
    method size_request =
      let sz = szhints in
      if not w.w_size_modified || sz.comp_timestamp = s.s_timestamp then sz else
        begin
          sz.comp_timestamp <- s.s_timestamp;
          match widget with None -> sz | Some o ->
              let wsz = o#size_request in
              sz.min_width <- max wsz.min_width sz.min_width;
              sz.min_height <- max wsz.min_height sz.min_height;
              sz.requested_width <- max (wsz.requested_width + 2 * w.w_ipad_x) sz.min_width;
              sz.requested_height <- max (wsz.requested_height + 2 * w.w_ipad_y) sz.min_height;
              sz.expand_x <- wsz.expand_x || sz.expand_x;
              sz.expand_y <- wsz.expand_y || sz.expand_y;
              sz
        end        
    
    method size_allocate x y dx dy =
      w.w_size_modified <- false;
      let g = w.w_geometry in
      let modified = not (g.width = dx && g.height = dy) in
      super#size_allocate x y dx dy;
      if modified then
        match widget with None -> () | Some o ->
            let g = w.w_geometry in
            o#size_allocate w.w_ipad_x w.w_ipad_y (g.width - 2 * w.w_ipad_x)
            (g.height - 2 * w.w_ipad_y)
    
    method set_action a = action <- a
    
    val mutable buttons = [1]
    method set_buttons list = buttons <- list
    
    initializer
      self#configure [Bindings [
          Button (1,0), (fun _ -> 
              if List.mem 1 buttons then begin
                  self#activate;
                  if not wait_release then 
                    begin
                      button_event := 1;
                      self#action ();
                      button_event := 0;
                      self#desactivate;
                    end
                end
              else self#desactivate);
          Button (2,0), (fun _ -> 
              if List.mem 2 buttons then begin
                  self#activate;
                  if not wait_release then 
                    begin
                      button_event := 2;
                      self#action ();
                      button_event := 0;
                      self#desactivate;
                    end end
                  else self#desactivate);
          Button (3,0), (fun _ -> 
              if List.mem 3 buttons then begin
                  self#activate;
                  if not wait_release then 
                    begin
                      button_event := 3;                      
                      self#action ();
                      button_event := 0;
                      self#desactivate;
                    end end
                  else self#desactivate);   
        ButtonReleased, (fun _ ->
            if activated then begin
                self#desactivate;
                if wait_release then self#action ()
              end)
      ]]
  
  method activate =
    if not activated then
      begin
        old_relief <- w.w_relief;
        self#configure [Relief ReliefSunken];
        activated <- true;
      end
  
  method desactivate =
    if activated then
      begin
        activated <- false;
        self#configure [Relief old_relief];
      end
  
  method destroy =
    (match widget with None -> () | Some o -> o#destroy);
    super#destroy
  
  method realize =
    super#realize;
    match widget with None -> () | Some o -> o#realize
  
  method show = 
    super#show;
    match widget with None -> () | Some o -> o#show
  
  method set_wait_release bool = wait_release <- bool
  method name = "button"
end

class orig_with_label parent string attributes =
  object (self)
  
  val mutable label = new WX_label.t parent "" attributes
  inherit orig parent attributes as super
  
  method realize =
    if w.w_window == noWindow then
      begin
        label <- new WX_label.t self#container string attributes;
        self#container_add label#contained;
        super#realize
      end
      
    method set_string s = label#set_string s
    method label = label
end

class t = orig
class with_label = orig_with_label