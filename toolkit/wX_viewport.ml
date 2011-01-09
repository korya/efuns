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

class t parent  (adx : WX_adjust.t) (ady : WX_adjust.t) attributes = 
  object (self)
  inherit WX_object.t parent 
    ([RetractX true; RetractY true] @ attributes) as super
  
  val view = { x= 0; y = 0; width = 1; height = 1; border=0;}
  val adx = adx
  val ady = ady
  val mutable child = None
  
  initializer 
    adx#add_subject self#update_x_adjustement;
    ady#add_subject self#update_y_adjustement;
  
  method realize =
    super#realize;
    match child with None -> ()
    | Some wob -> wob#realize
  
  method size_request = 
    let tsz = szhints in
    if not w.w_size_modified || tsz.comp_timestamp = s.s_timestamp then tsz
      else
      begin
        tsz.comp_timestamp <- s.s_timestamp;
        tsz.requested_width <- max tsz.requested_width tsz.min_width;
        tsz.requested_height <- max tsz.requested_height tsz.min_height;
        (match child with None -> ()
          | Some wob -> 
              let sz = wob#size_request in
              let wg = w.w_geometry in
              let width =  (max sz.requested_width sz.min_width) + 
                  2 * (sz.border_width + w.w_ipad_x) in
              let height = (max sz.requested_height sz.min_height) + 
                  2 * (sz.border_width + w.w_ipad_y) in
              tsz.requested_width <- max tsz.requested_width width;
              tsz.requested_height <- max tsz.requested_height height;
              tsz.expand_x <- tsz.expand_x || sz.expand_x;
              tsz.expand_y <- tsz.expand_y || sz.expand_y;
        );
        tsz.requested_height <- min tsz.requested_height tsz.max_height;
        tsz.requested_width <- min tsz.requested_width tsz.max_width;
        tsz
      end
      
  method show =
    super#show;
    match child with None -> ()
    | Some wob -> wob#show
  
    method size_allocate x y dx dy =
    let g = w.w_geometry in    
    let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
    w.w_size_modified <- false;
    super#size_allocate x y dx dy;
    if modified then
        self#update_top_size;
  
  method container_add (wob : contained) =
    wob#set_parent self#container;
    child <- Some (if not (w.w_window == noWindow) then (wob#realize; wob) else wob);
    self#update_top_size

  method update_top_size =
    match child with None -> ()
    | Some wob -> 
        let tsz = szhints in
        let sz = wob#size_request in
        let wg = w.w_geometry in
        let borders = 2 * sz.border_width in
        let borders_x = borders + 2 * w.w_ipad_x in
        let borders_y = borders + 2 * w.w_ipad_y in
        let width = (max (max sz.requested_width sz.min_width)
            (if sz.expand_x then wg.width - borders_x else 0))
            + borders_x in
        let height = (max (max sz.requested_height sz.min_height) 
            (if sz.expand_y then wg.height - borders_y else 0))
            + borders_y in
        view.width <- width;
        view.height <- height;
        adx#update_x wg view;
        ady#update_y wg view;
  
  method update_x_adjustement () =
    view.x <- adx#get_pos view.width;
    self#size_allocate_childs
    
  method size_allocate_childs =
    match child with None -> () | Some wob -> 
        let sz = wob#size_request in
        wob#size_allocate
          (-view.x + w.w_ipad_x + sz.border_width)
          (-view.y + w.w_ipad_y + sz.border_width)
          (view.width  - 2 * (w.w_ipad_x + sz.border_width))
          (view.height - 2 * (w.w_ipad_y + sz.border_width))
  
  method update_y_adjustement () =
    view.y <- ady#get_pos view.height;
    self#size_allocate_childs
  
  method refresh = ()
  
  method name = "viewport"
end
