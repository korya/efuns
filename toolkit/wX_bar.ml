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

class orig sens parent attributes =
  object (self)
    
    inherit WX_object.t parent attributes as super
    
    val mutable wobs = [||]
    val mutable sens = sens
    
    method iter f = Array.iter f wobs
    
    method size_request =
      let tsz = szhints in
      if not w.w_size_modified || tsz.comp_timestamp = s.s_timestamp then tsz else
        begin
          tsz.comp_timestamp <- s.s_timestamp;
          Array.iter (fun wob -> let _ = wob#size_request in ()) wobs;
          tsz.requested_width <- w.w_ipad_x;
          tsz.requested_height <- w.w_ipad_y;
          let expand_x, expand_y = 
            match sens with
              Vertical -> ref true, ref false
            | Horizontal -> ref false, ref true
          in
          Array.iter (fun wob ->
              let sz = wob#size_request in
              let borders = sz.border_width * 2 in
              let width = sz.requested_width + borders + w.w_ipad_x in
              let height = sz.requested_height + borders + w.w_ipad_y in
              match sens with
                Vertical -> 
                  tsz.requested_width <- max width tsz.requested_width;
                  expand_y := !expand_y || sz.expand_y;
                  expand_x := !expand_x && sz.expand_x;
                  tsz.requested_height <- tsz.requested_height + height
              | Horizontal ->
                  tsz.requested_height <- max height tsz.requested_height;
                  expand_x := !expand_x || sz.expand_x;
                  expand_y := !expand_y && sz.expand_y;
                  tsz.requested_width <- tsz.requested_width + width
          ) wobs;
          tsz.expand_x <- tsz.expand_x || !expand_x;
          tsz.expand_y <- tsz.expand_y || !expand_y;
          tsz.requested_width <- tsz.requested_width +
            (relief_size w.w_relief w.w_ipad_x);
          tsz.requested_height <- tsz.requested_height +
            (relief_size w.w_relief w.w_ipad_y);
          tsz.requested_width <- min tsz.max_width 
            (max tsz.requested_width tsz.min_width);
          tsz.requested_height <- min tsz.max_height 
            (max tsz.requested_height tsz.min_height);
          (*
          Printf.printf "%s request: %d x %d" self#name
            tsz.requested_width tsz.requested_height;
          print_newline ();
            *)
          tsz
        end;
      
    method size_allocate x y dx dy =
      (*
      Printf.printf "%s receives: %d x %d" self#name dx dy;
      print_newline ();
    *)
      let g = w.w_geometry in
      let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
      super#size_allocate x y dx dy;
      if modified then
        if Array.length  wobs > 0 then
          let tsz = szhints in
          let relief_dx = relief_size w.w_relief w.w_ipad_x in
          let relief_dy = relief_size w.w_relief w.w_ipad_y in
          let holes = ref 0 in
          let retractables = ref 0 in
          let required = ref 0 in
          let g = w.w_geometry in
          begin
            match sens with
              Vertical ->
                let required = ref 0 in
                let requested = ref 0 in
                let holes = ref 0 in
                let nwobs = Array.length wobs in
                for i = 0 to nwobs - 1 do
                  let sz = wobs.(i)#size_request in
                  let borders = 2 * sz.border_width in
                  required := sz.min_height + borders + !required;
                  requested := sz.requested_height + borders + !requested;
                  if sz.expand_y then incr holes
                done;
                
                let offered = g.height - 2 * relief_dy in
                if offered >= !requested then 
                  begin (* We have more place than wanted *)
                    
                    let supplement = offered - !requested in
                    let pad = min w.w_ipad_y (supplement / (nwobs + 1)) in
                    let padding = pad * (nwobs + 1) in
                    let expand = supplement - padding in
                    let y = ref (pad+relief_dy) in
                    for i = 0 to nwobs - 1 do
                      let sz = wobs.(i)#size_request in
                      let borders = 2 * sz.border_width in
                      let height = sz.requested_height in
                      let height = if sz.expand_y then
                          height + expand / !holes else height
                      in
                      let x, width = 
                        if sz.requested_width + borders < g.width then
                          let pad = min (w.w_ipad_x + relief_dx)
                            ((g.width - sz.requested_width - borders) / 2) in
                          (pad + sz.border_width), 
                          (if sz.expand_x then g.width - borders - 2 * pad
                            else sz.requested_width)
                        else
                          sz.border_width, g.width - borders
                      in
                      
                      wobs.(i)#size_allocate (x+relief_dx) (!y + sz.border_width)
                      width height;
                      y := !y + borders + height + pad
                    done
                  end
                else
                if offered > !required then
                  begin
                    let supplement = ref (max 0 (offered - !required)) in
                    
                    let y = ref relief_dy in
                    for i = 0 to nwobs - 1 do
                      let sz = wobs.(i)#size_request in
                      let borders = 2 * sz.border_width in
                      let sup = max 0 (min !supplement 
                            (sz.requested_height - sz.min_height)) in
                      let height = sz.min_height + borders + sup in
                      
                      let x, width = 
                        if sz.requested_width + borders < g.width then
                          let pad = min (w.w_ipad_x + relief_dx) 
                            ((g.width - sz.requested_width - borders) / 2) in
                          (pad + sz.border_width), 
                          (if sz.expand_x then g.width - borders - 2 * pad
                            else sz.requested_width)
                        else
                          sz.border_width, g.width - borders
                      in
                      
                      wobs.(i)#size_allocate (x+relief_dx) (!y + sz.border_width) 
                      width height;
                      y := !y + borders + height;
                      supplement := !supplement - sup
                    done
                  end
                else
                let ratio = (offered lsl 14) / (max 1 !required) in
                let y = ref 0 in
                for i = 0 to nwobs - 1 do
                  let sz = wobs.(i)#size_request in
                  let borders = 2 * sz.border_width in
                  let height = ((sz.min_height+borders) * ratio) lsr 14 in
                  let height = height - borders in
                  
                  let x, width = 
                    if sz.requested_width + borders < g.width then
                      let pad = min (w.w_ipad_x + relief_dx) 
                        ((g.width - sz.requested_width - borders) / 2) in
                      (pad + sz.border_width), 
                      (if sz.expand_x then g.width - borders - 2 * pad
                        else sz.requested_width)
                    else
                      sz.border_width, g.width - borders
                  in
                  wobs.(i)#size_allocate (x+relief_dx) (!y + sz.border_width) 
                  width height;
                  y := !y + borders + height;              
                done
            
            |  Horizontal ->
                let required = ref 0 in
                let requested = ref 0 in
                let holes = ref 0 in
                let nwobs = Array.length wobs in
                for i = 0 to nwobs - 1 do
                  let sz = wobs.(i)#size_request in
                  let borders = 2 * sz.border_width in
                  required := sz.min_width + borders + !required;
                  requested := sz.requested_width + borders + !requested;
                  if sz.expand_x then incr holes
                done;
                let offered = g.width - 2 * relief_dx in
                if offered > !requested then 
                  begin (* We have more place than wanted *)
                    let supplement = max 0 (offered - !requested) in
                    let pad = min w.w_ipad_x (supplement / (nwobs + 1)) in
                    let padding = pad * (nwobs + 1) in
                    let expand = supplement - padding in
                    
                    let x = ref (pad+relief_dx) in
                    for i = 0 to nwobs - 1 do
                      let sz = wobs.(i)#size_request in
                      let borders = 2 * sz.border_width in
                      let width = sz.requested_width in
                      let width = if sz.expand_x then
                          width + expand / !holes else width
                      in
                      
                      let y, height = 
                        if sz.requested_height + borders < g.height then
                          let pad = min (w.w_ipad_y + relief_dy)
                            ((g.height - sz.requested_height - borders) / 2) in
                          (pad + sz.border_width), 
                          (if sz.expand_y then g.height - borders - 2 * pad
                            else sz.requested_height)
                        else
                          sz.border_width, g.height - borders
                      in
                      
                      wobs.(i)#size_allocate  
                        (!x + sz.border_width) (y+ relief_dy) width height;
                      x := !x + borders + width + pad
                    done
                  end
                else
                if offered > !required then
                  begin
                    let supplement = ref (max 0 (offered - !required)) in
                    
                    let x = ref relief_dx in
                    for i = 0 to nwobs - 1 do
                      let sz = wobs.(i)#size_request in
                      let borders = 2 * sz.border_width in
                      let sup = max 0 (min !supplement 
                            (sz.requested_width - sz.min_width)) in
                      let width = sz.min_width + borders + sup in
                      
                      let y, height = 
                        if sz.requested_height + borders < g.height then
                          let pad = min (w.w_ipad_y + relief_dy)
                            ((g.height - sz.requested_height - borders) / 2) in
                          (pad + sz.border_width), 
                          (if sz.expand_y then g.height - borders - 2 * pad
                            else sz.requested_height)
                        else
                          sz.border_width, g.height - borders
                      in
                      
                      wobs.(i)#size_allocate
                        (!x + sz.border_width) (y+relief_dy) width height;
                      x := !x + borders + width;
                      supplement := !supplement - sup
                    done
                  end
                else
                let ratio = (offered lsl 14) / (max 1 !required) in
                let x = ref 2 in
                for i = 0 to nwobs - 1 do
                  let sz = wobs.(i)#size_request in
                  let borders = 2 * sz.border_width in
                  let width = ((sz.min_width+borders) * ratio) lsr 14 in
                  let width = width - borders in
                  
                  let y, height = 
                    if sz.requested_height + borders < g.height then
                      let pad = min (w.w_ipad_y  - 2)
                        ((g.height - sz.requested_height - borders) / 2) in
                      (pad + sz.border_width), 
                      (if sz.expand_y then g.height - borders - 2 * pad
                        else sz.requested_height)
                    else
                      sz.border_width, g.height - borders
                  in
                  
                  wobs.(i)#size_allocate
                    (!x + sz.border_width) y width height;
                  x := !x + borders + width
                done
          end;
          
(*            
  method destroy =
    Array.iter (fun wob -> wob#destroy) wobs;
    super#destroy
        *)
        
  method container_insert i (wob : contained) =
    let size = Array.length wobs in
    if i > size then failwith "Bar: insert_item after end of bar";
    wobs <- Array.init (size+1) (fun j ->
        if i=j then
          if not (w.w_window == noWindow) then (wob#realize; wob) else wob
            else if j < i then wobs.(j) else wobs.(j-1));
    self#wait_resize
  
  method container_remove i =
    let size = Array.length wobs in
    if i >= size then failwith "Bar: remove_item after end of bar";
    wobs <- Array.init (size-1) (fun j ->
        if i = j then wobs.(i)#destroy;
        if j >= i then wobs.(j+1) else wobs.(j));
    self#wait_resize
  
  method container_add wob = 
    wob#set_parent self#container;
    self#container_insert (self#nitems) wob;
    
  method container_add_s wobs = 
    match wobs with
      [] -> () | 
      wob :: wobs -> self#container_add wob; self#container_add_s wobs

  method clear_items =
    Array.iter (fun w -> w#destroy) wobs;
    wobs <- [||];
    self#wait_resize
    
  method set_items ws =
    self#clear_items;
    wobs <- ws;
    if not (w.w_window == noWindow) then Array.iter (fun w -> w#realize) wobs;
    if w.w_shown then  Array.iter (fun w -> w#show) wobs;
    self#wait_resize
        
  method nitems = Array.length wobs
  method items = wobs    
  
  method name = "bar"
end

class t = orig

class v parent attributes  =
  object 
  inherit t Vertical parent attributes
  
  method name = "vbar"
end

class h parent attributes =
  object 
  inherit t Horizontal parent attributes
  
  method name = "hbar"
end

