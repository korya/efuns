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
  
  val mutable widget = None
  
  method container_add (o : contained) =
    o#set_parent self#container;
    widget <- Some o;
    self#wait_resize
  
  method size_request =
    let sz = szhints in
    if not w.w_size_modified || sz.comp_timestamp = s.s_timestamp then sz else
      begin
        sz.comp_timestamp <- s.s_timestamp;
        match widget with None -> sz | Some o ->
            let sz = o#size_request in
            szhints <- { sz with 
              requested_width = sz.requested_width + 4;
              requested_height = sz.requested_height + 4;
              comp_timestamp = s.s_timestamp;
              };
            szhints
      end
  method size_allocate x y dx dy =
    w.w_size_modified <- false;
    let g = w.w_geometry in
    let modified = not (g.width = dx && g.height = dy) in
    super#size_allocate x y dx dy;
    if modified then
    match widget with None -> () | Some o ->
        let g = w.w_geometry in
        o#size_allocate 2 2 (g.width - 4) (g.height - 4)

  method destroy =
    (match widget with None -> () | Some o -> o#destroy);
    super#destroy
    
  method realize =
    super#realize;
    match widget with None -> () | Some o -> o#realize
    
  method show = 
    super#show;
    match widget with None -> () | Some o -> o#show

end
    