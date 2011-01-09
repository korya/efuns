(***********************************************************************)
(*                                                                     *)
(*                             WXlib                                   *)
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
  
  inherit WX_object.t parent ((IpadX 5)::(IpadY 5)::(Relief ReliefInRidge) ::
    attributes) as super
  
  val mutable tab = [||]
  val mutable visible = 0
  
  method container_add (oo : contained) =
    let len = Array.length tab in
    tab <- Array.init (len+1) (fun i -> if i >= len then oo else tab.(i));
    oo#set_parent self#container;
    if w.w_shown && len = 0 then oo#show;
    self#wait_resize;
    len
  
  method container_add_s list = 
    match list with [] -> () | oo :: list -> 
        let _ = self#container_add oo in
        self#container_add_s list
  
  method size_request =
    let sz = szhints in
    if not w.w_size_modified  || sz.comp_timestamp = s.s_timestamp then sz
    else let _ = () in
    sz.comp_timestamp <- s.s_timestamp;
    let dx = ref 0 in
    let dy = ref 0 in
    let expand_x = ref true in
    let expand_y = ref true in
    Array.iter (fun oo ->
        let sz = oo#size_request in
        let borders = sz.border_width in
        dx := max !dx (sz.requested_width + borders);
        dy := max !dy (sz.requested_height + borders);
        expand_x := !expand_x && sz.expand_x;
        expand_y := !expand_y && sz.expand_y;        
    ) tab;
    sz.requested_width <- max (!dx + (2 * w.w_ipad_x)) sz.min_width;
    sz.requested_height <- max (!dy + (2 * w.w_ipad_y)) sz.min_height;
    sz.expand_x <- sz.expand_x && !expand_x;
    sz.expand_y <- sz.expand_y && !expand_y;
    sz

  method iter f = Array.iter f tab
  method iter_visible f =
    if Array.length tab > visible then f tab.(visible)
    
  method size_allocate x y dx dy =
    let g = w.w_geometry in
    let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
    w.w_size_modified <- false;
    super#size_allocate x y dx dy;
    if not modified then () else let _ = () in
    let width = g.width - 2 * w.w_ipad_x in
    let height = g.height - 2 * w.w_ipad_y in
    Array.iter (fun o ->
        let sz = o#size_request in
        let borders = sz.border_width * 2 in
        o#size_allocate (w.w_ipad_x+sz.border_width) (
          w.w_ipad_y+sz.border_width) (
          if width > sz.requested_width + borders && not sz.expand_x then
            sz.requested_width else width - borders) (
          if height > sz.requested_height + borders && not sz.expand_y then
            sz.requested_height else height - borders)
    ) tab

  method set_visible v =
    if v < Array.length tab && v <> visible then
      begin
        if w.w_shown then tab.(visible)#hide;
        visible <- v;
        if w.w_shown then tab.(visible)#show        
      end
      
  method name = "swap"
end
