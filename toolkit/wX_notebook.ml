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

(* TODO: We might want to replace the direct swap by an indirect swap through
  a viewport and scrollbars (scrolled_window). *)

open Xtypes
open WX_types

let inverse = function 
    Vertical -> Horizontal
  | Horizontal -> Vertical
      
class t parent sens attributes =
  object (self)

  val mutable labels = [||]
  val mutable swap = new WX_swap.t parent (* <- NOT USED *) attributes
  val mutable visible = 0
  val mutable bar = new WX_bar.v parent (* <- NOT USED *) attributes
  val mutable topbar = new WX_bar.v parent (* <- NOT USED *) attributes
    
    inherit WX_object.t parent attributes as super
    
  initializer
    topbar <- new WX_bar.t (inverse sens) self#container [];
    swap <- new WX_swap.t topbar#container [];
    bar <- new WX_bar.t sens topbar#container [];
    topbar#container_add_s [bar#contained; swap#contained];
    let eol = new WX_object.t bar#container [Relief ReliefSunken; ExpandX true; ExpandY true] in
    bar#container_add  eol#contained;
    
  method size_request = topbar#size_request
  method size_allocate x y dx dy = 
    super#size_allocate x y dx dy;
    topbar#size_allocate x y dx dy;
    
  method iter f = 
    f topbar#contained
    
  method iter_visible f = 
    f topbar#contained
  
  method container_add (label, wob) =
    let len = Array.length labels in
    let expand = match sens with
        Vertical -> ExpandX true
      | Horizontal -> ExpandY true in
    labels <- Array.init (len+1) (fun i -> if i >= len then 
          let button = new WX_button.t bar#container [expand;Relief 
              (if len = 0 then ReliefFlat else ReliefSunken)] in
          let label = new WX_label.t button#container label [expand;
              IpadX 5; IpadY 5] 
          in
          button#container_add label#contained;
          bar#container_insert len button#contained;
          button#set_action (fun _ -> self#set_visible i);
          button#contained
        else labels.(i));
    let l = swap#container_add wob in
    assert (l=len);
    if w.w_shown && len = 0 then wob#show;
    self#wait_resize;
    len

  method container_add_s wobs = 
    match wobs with
      [] -> () | 
      wob :: wobs -> let _ = self#container_add wob in
        self#container_add_s wobs

  method set_visible v =
    if visible <> v && v < Array.length labels then
      begin
        swap#set_visible v;
        labels.(v)#configure [Relief ReliefFlat];
        labels.(visible)#configure [Relief ReliefSunken];
        visible <- v;
      end

  method name = "notebook"
end


class v parent attributes  =
  object 
  inherit t parent Vertical attributes
  
  method name = "vnotebook"
end

class h parent attributes =
  object 
  inherit t parent Horizontal attributes
  
  method name = "hnotebook"
end

