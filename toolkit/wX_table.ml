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
  
  (* NOT COMPLETELY IMPLEMENTED YET.
  
  The WX_table.t widget will allow to set widgets positions in a table.
  
  Clearly, for all containers, there is a need to separate size hints in two 
  different kinds:
  those which are user defined, and those which are computed from
  contained widgets.
*)

class t parent attributes dx dy homogeneous =
  let _ = assert (dx > 0); assert (dy > 0) in
  object (self)
  
  val wobs = Array.init dy (fun i -> 
        Array.init dx (fun i -> WX_dummy.contained parent,0,0,1,1)
    )
  
  val hints = Array.init (if homogeneous then 1 else
        max dx dy) WX_object.new_szhints
      
      inherit WX_object.t parent attributes as super
  
  method iter f = Array.iter (fun t -> 
        Array.iter (fun (o,x,y,dx,dy) -> f o) t) wobs
  
  method size_request =
    let tsz = szhints in
    if not w.w_size_modified || tsz.comp_timestamp = s.s_timestamp then tsz
    else 
    let _ = () in
    tsz.comp_timestamp <- s.s_timestamp;
        (* First, initialize everything *)
    for i = 1 to if homogeneous then 1 else max dx dy do
      hints.(i-1) <- WX_object.new_szhints ()
    done;
        (* second, computes rows and cols sizes *)
    for i = 0 to dx - 1 do
      for j = 0 to dy - 1 do
        let wob,_,_,ddx,ddy = wobs.(i).(j) in
        let sz = wob#size_request in
        let borders = sz.border_width * 2 in
        let width = sz.requested_width + borders in
        let height = sz.requested_height + borders in
        let szx, szy =
          if homogeneous then hints.(0), hints.(0) else
            hints.(i), hints.(j) in
        szx.requested_width <- max szx.requested_width (
          (sz.requested_width + 2 * sz.border_width) / ddx);
        szx.expand_x <- sz.expand_x && szx.expand_x;
        szx.min_width <- max szx.min_width sz.min_width;
        szy.requested_height <- max szx.requested_height (
          (sz.requested_height + 2 * sz.border_width) / ddy);
        szy.expand_y <- sz.expand_y && szx.expand_y;
        szy.min_height <- max szy.min_height sz.min_height;
      done
    done;
        (* third, compute the complete size *)
    if homogeneous then
      let sz = hints.(0) in
      tsz.requested_width <- 2 * w.w_ipad_x + dx * sz.requested_width;
      tsz.requested_height <- 2 * w.w_ipad_y + dy * sz.requested_height;
      tsz.expand_x <- sz.expand_x;
      tsz.expand_y <- sz.expand_y;
      tsz
    else
    let expand_x = ref true in
    let expand_y = ref true in
    tsz.requested_width <- 2 * w.w_ipad_x;
    tsz.requested_height <- 2 * w.w_ipad_y;
    for i = 0 to dx - 1 do
      tsz.requested_width <- hints.(i).requested_width + tsz.requested_width;
      expand_x := !expand_x && hints.(i).expand_x
    done;
    for i = 0 to dy - 1 do
      tsz.requested_height <- hints.(i).requested_height + tsz.requested_height;
      expand_y := !expand_y && hints.(i).expand_y      
    done;
    tsz.expand_x <- tsz.expand_x || !expand_x;
    tsz.expand_y <- tsz.expand_y || !expand_y;
    tsz
  
  method size_allocate x y width height =
    let g = w.w_geometry in
    let modified = w.w_size_modified || not (
        g.width = width && g.height = height) in
    super#size_allocate x y width height;
    if not modified || Array.length hints < 1 then () else
    let tsz = szhints in
    let relief = match w.w_relief with
        ReliefFlat -> 0
      | ReliefRaised -> 1
      | ReliefSunken -> 1
      | ReliefRaisedN n -> n
      | ReliefSunkenN n -> n
      | ReliefInRaised -> 1
      | ReliefInSunken -> 1
      | ReliefInRaisedN n -> n
      | ReliefInSunkenN n -> n          
      | _ -> 2
    in      
    if homogeneous then
      let sz = hints.(0) in
      let width = g.width - 2 * w.w_ipad_x in
      let height = g.height - 2 * w.w_ipad_y in
      let width = if sz.expand_x then width else
          min width (sz.requested_width * dx) in
      let height = if sz.expand_y then height else
          min height (sz.requested_height * dy) in
      let width = width / dx in
      let height = height / dy in
      for i = 0 to dx - 1 do
        for j = 0 to dy - 1 do
          let wob, x,y, ddx, ddy = wobs.(i).(j) in
          let sz = wob#size_request in
          if x = 0 && y = 0 then
            let borders = sz.border_width * 2 in
            wob#size_allocate (i * width + sz.border_width + w.w_ipad_x) (
              j * height + sz.border_width + w.w_ipad_y) (
              width * ddx - borders) (height * ddy - borders)
        done
      done
    else
    let g = w.w_geometry in
    let width = g.width - 2 * w.w_ipad_x in
    let height = g.height - 2 * w.w_ipad_y in
    let x_holes = ref 0 in
    let y_holes = ref 0 in
    let requested_x = ref 1 in
    let requested_y = ref 1 in
    for i = 0 to dy - 1 do
      let sz = hints.(i) in
      requested_y := sz.requested_height + !requested_y;
      if sz.expand_y then incr y_holes
    done;
    for i = 0 to dx - 1 do
      let sz = hints.(i) in
      requested_x := sz.requested_width + !requested_x;
      if sz.expand_x then incr x_holes
    done;        
    let supplement_x = max 0 (width - !requested_x) in
    let supplement_y = max 0 (height - !requested_y) in
    let x = ref w.w_ipad_x in
    for i = 0 to dx - 1 do
      let y = ref w.w_ipad_y in
      let szx = hints.(i) in
      let width = szx.requested_width + (
          if szx.expand_x then supplement_x / !x_holes else 0) in
      for j = 0 to dy - 1 do
        let wob,xx,yy,dxx,dyy = wobs.(i).(j) in
        let szy = hints.(j) in
        let height = szy.requested_height + (
            if szx.expand_y then supplement_y / !y_holes else 0) in
        begin
          if xx=0 && yy=0 then
            let sz = wob#size_request in
            let borders = 2 * sz.border_width in
            let ww = ref width in
            let hh = ref height in
            for ii = 1 to dxx -1 do
              let szx = hints.(i+ii) in
              let width = szx.requested_width + (
                  if szx.expand_x then supplement_x / !x_holes else 0) in
              ww := !ww + width
            done;
            for jj = 1 to dyy -1 do
              let szy = hints.(j+jj) in
              let height = szy.requested_height + (
                  if szx.expand_y then supplement_y / !y_holes else 0) in
              hh := !hh + height
            done;
            let width = !ww in
            let height = !hh in
            wob#size_allocate (!x + sz.border_width) (!y + sz.border_width) (
              (if sz.expand_x then 
                  width else min width (sz.requested_width + borders))
              - borders) (
              (if sz.expand_y then 
                  height else min height (sz.requested_height + borders))
              - borders);
        end;
        y := !y + height
      done;
      x := !x + width
    done
    
          
  method container_add wob x y dx dy =
    wob#set_parent self#container;
    for i = 0 to dx-1 do
      for j = 0 to dy-1 do
        wobs.(x+i).(y+j) <- (wob, i, j, dx, dy)
      done
    done

  method container = (self :> container)
end
