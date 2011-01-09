(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xlib
open Xtypes
open Gwml
open Stdconfig
open Options

let delay time =
  try ignore (Unix.select [] [] [] (time *. !!animation_delay)) with _ -> ()
  
let draw_box root gc r =
  Xlib.drawRectangle display root gc  r.x r.y r.width r.height

let linear w g1 g2 =
  let s = w.w_screen in
  let scr = s.s_scr in
  let root = scr.scr_root in
  let white = scr.scr_white_pixel in
  let black = scr.scr_black_pixel in
  let gc = X.createGC display root [
      GCfonction GXxor;
      GCforeground white;
      GCbackground black;
      GCsubwindow_mode IncludeInferiors;
    ]
  in
  let x12 = g1.x + g1.width in
  let x22 = g2.x + g2.width in
  let y12 = g1.y + g1.height in
  let y22 = g2.y + g2.height in
  for i = 0 to 100 do
    let x1 = g1.x + ((g2.x - g1.x) * i) / 100 in
    let y1 = g1.x + ((g2.y - g1.y) * i) / 100 in
    let x2 = x12 + ((x22 - x12) * i) / 100 in
    let y2 = y12 + ((y22 - y12) * i) / 100 in
    let g = { x = x1; y = y1; width = x2-x1; height = y2-y1; border = 0 } in
    draw_box root gc g;
  done;
  delay 1.;
  for i = 0 to 100 do
    let x1 = g1.x + ((g2.x - g1.x) * i) / 100 in
    let y1 = g1.x + ((g2.y - g1.y) * i) / 100 in
    let x2 = x12 + ((x22 - x12) * i) / 100 in
    let y2 = y12 + ((y22 - y12) * i) / 100 in
    let g = { x = x1; y = y1; width = x2-x1; height = y2-y1; border = 0 } in
    draw_box root gc g;
  done;
  ()
  
let linear_move w g g2 =
  try
    if !!do_animation && not !prevent_animation then 
      let s = w.w_screen in
      Gwml.grabServer ();
      let scr = s.s_scr in
      let root = scr.scr_root in
      let white = scr.scr_white_pixel in
      let black = scr.scr_black_pixel in
      let gc = X.createGC display root [
          GCfonction GXxor;
          GCforeground white;
          GCbackground black;
          GCsubwindow_mode IncludeInferiors;
        ]
      in
      let width = g.width in
      let height = g.height in
      let x1, x2, dx = 
        if g.x < g2.x then g.x, g2.x, width
        else g.x + width, g2.x + width, - width in
      let y1, y2, dy = if g.y < g2.y then  g.y, g2.y, height
        else g.y + height, g2.y + height, - height in
      g.x <- g2.x;
      g.y <- g2.y;
      Wob.send w WobMove;
      let steps = 20 in
      for i = 0 to steps - 1 do
        let x = x1 + ((x2 - x1) * i) / steps in
        let y = y1 + ((y2 - y1) * i) / steps in
        Xlib.drawLine display root gc x y x (y + dy);
        Xlib.drawLine display root gc x y (x + dx) y;
      done;
      for i = 0 to steps - 1  do          
        delay 100.;
        let x = x1 + ((x2 - x1) * i) / steps in
        let y = y1 + ((y2 - y1) * i) / steps in
        Xlib.drawLine display root gc x y x (y + dy);
        Xlib.drawLine display root gc x y (x + dx) y;
      done; 
      Gwml.ungrabServer ();
    else begin
        g.x <- g2.x;
        g.y <- g2.y;
        Wob.send w WobMove;
      end
  with _ ->    
      Gwml.ungrabServer ();
      g.x <- g2.x;
      g.y <- g2.y;
      Wob.send w WobMove
      
let linear_transform w g g2 =
  try
    if !!do_animation && not !prevent_animation then 
      let s = w.w_screen in
      Gwml.grabServer ();
      let scr = s.s_scr in
      let root = scr.scr_root in
      let white = scr.scr_white_pixel in
      let black = scr.scr_black_pixel in
      let gc = X.createGC display root [
          GCfonction GXxor;
          GCforeground white;
          GCbackground black;
          GCsubwindow_mode IncludeInferiors;
        ]
      in
      let steps = 20 in
      let width = g.width in
      let height = g.height in
      
      let ax1 = g.x in
      let ay1 = g.y in
      let ax2 = g2.x in
      let ay2 = g2.y in
      
      let bx1 = g.x + g.width in
      let by1 = g.y in
      let bx2 = g2.x + g2.width in
      let by2 = g2.y in
      
      let cx1 = g.x in
      let cy1 = g.y + g.height in
      let cx2 = g2.x in
      let cy2 = g2.y + g2.height in
      
      let dx1 = g.x + g.width in
      let dy1 = g.y + g.height in
      let dx2 = g2.x + g2.width in
      let dy2 = g2.y + g2.height in  
      
      for i = 0 to steps - 1 do
        let ax = ax1 + ((ax2 - ax1) * i) / steps in
        let bx = bx1 + ((bx2 - bx1) * i) / steps in
        let cx = cx1 + ((cx2 - cx1) * i) / steps in
        let dx = dx1 + ((dx2 - dx1) * i) / steps in
        let ay = ay1 + ((ay2 - ay1) * i) / steps in
        let by = by1 + ((by2 - by1) * i) / steps in
        let cy = cy1 + ((cy2 - cy1) * i) / steps in
        let dy = dy1 + ((dy2 - dy1) * i) / steps in
        Xlib.drawLine display root gc ax ay bx by;
        Xlib.drawLine display root gc ax ay cx cy;
        Xlib.drawLine display root gc dx dy bx by;
        Xlib.drawLine display root gc dx dy cx cy;
      done;
      
      for i = 0 to steps - 1 do
        delay 1.;
        let ax = ax1 + ((ax2 - ax1) * i) / steps in
        let bx = bx1 + ((bx2 - bx1) * i) / steps in
        let cx = cx1 + ((cx2 - cx1) * i) / steps in
        let dx = dx1 + ((dx2 - dx1) * i) / steps in
        let ay = ay1 + ((ay2 - ay1) * i) / steps in
        let by = by1 + ((by2 - by1) * i) / steps in
        let cy = cy1 + ((cy2 - cy1) * i) / steps in
        let dy = dy1 + ((dy2 - dy1) * i) / steps in
        Xlib.drawLine display root gc ax ay bx by;
        Xlib.drawLine display root gc ax ay cx cy;
        Xlib.drawLine display root gc dx dy bx by;
        Xlib.drawLine display root gc dx dy cx cy;
      done;
      Gwml.ungrabServer ();
      prevent_autoraise := false
  with _ -> 
      Gwml.ungrabServer ();
      prevent_autoraise := false
      
let iterations = ref 12
      
let tv_close w =  
  try
    if !!do_animation && not !prevent_animation then 
      let s = w.w_screen in
      Gwml.grabServer ();
      let scr = s.s_scr in
      let root = scr.scr_root in
      let white = scr.scr_white_pixel in
      let black = scr.scr_black_pixel in
      let gc = X.createGC display root [
          GCfonction GXxor;
          GCforeground white;
          GCbackground black;
          GCsubwindow_mode IncludeInferiors;
        ]
      in
      
      let g = w.w_top.w_geometry in
      let x = g.x in
      let y = g.y in
      let w = g.width in
      let h = g.height in
      
      let step = ref 2 in
      if (h > 4) then begin
          let y = ref y in
          
          step := h * 4 / !iterations;
          if !step = 0 then step := 2;
          let i = ref h in
          while !i >= 2 do
            drawRectangle display root gc x !y w !i;
            delay 10.;
            drawRectangle display root gc x !y w !i;
            y := !y + !step / 2;
            i := !i - !step;
          done;
        end;
      if w >= 2 then begin
          let x = ref x in
          step := w * 4 / !iterations;
          if !step = 0 then
            step := 2;
          let i = ref w in
          while !i>= 0 do
            drawRectangle display root gc !x y !i 2;
            delay 10.;
            drawRectangle display root gc !x y !i 2;
            x := !x + !step / 2;
            i := !i - !step;
          done;
        end;
      Gwml.ungrabServer ()
  with _ -> 
      Gwml.ungrabServer ()
  
      
      (*      
  
let  w =  
  try
    if !!do_animation && not !prevent_animation then 
      let s = w.w_screen in
      Gwml.grabServer ();
      let scr = s.s_scr in
      let root = scr.scr_root in
      let white = scr.scr_white_pixel in
      let black = scr.scr_black_pixel in
      let gc = X.createGC display root [
          GCfonction GXxor;
          GCforeground white;
          GCbackground black;
          GCsubwindow_mode IncludeInferiors;
        ]
      in
      gwml.grabServer ()
  with _ -> 
      
      Gwml.ungrabServer ();
      ()

      *)

let pi = 3.14159265358979323846
let twist = 0.5
    
let twist_resize w g1 g2 =  
  try
    if !!do_animation && not !prevent_animation then 
      let s = w.w_screen in
      Gwml.grabServer ();
      let scr = s.s_scr in
      let root = scr.scr_root in
      let white = scr.scr_white_pixel in
      let black = scr.scr_black_pixel in
      let gc = X.createGC display root [
          GCfonction GXxor;
          GCforeground white;
          GCbackground black;
          GCsubwindow_mode IncludeInferiors;
        ]
      in
      let x = g1.x in
      let y = g1.y in
      let w = g1.width in
      let h = g1.height in
      let fx = g2.x in
      let fy = g2.y in
      let fw = g2.width in
      let fh = g2.height in
      
      let x = x + w / 2 in
      let y = y + h / 2 in
      let fx = fx + fw / 2 in
      let fy = fy + fh / 2 in

      let iterations = float_of_int !iterations in      
      
      let xstep = (float_of_int (fx - x)) /. iterations in
      let ystep = (float_of_int (fy - y)) /. iterations in
      let wstep = (float_of_int (fw - w)) /. iterations in
      let hstep = (float_of_int (fh - h)) /. iterations in
      
      let cx = ref (float_of_int x) in
      let cy = ref (float_of_int y) in
      let cw = ref (float_of_int w) in
      let ch = ref (float_of_int h) in
      let a = ref (atan (!ch /. !cw)) in
      let d = ref (
          sqrt ((!cw /. 2.) *. (!cw /. 2.) +. (!ch /. 2.) *. (!ch /. 2.))) in
      
      let angle_finite = 2. *. pi *. twist in
      let angle = ref 0. in
      while !angle < angle_finite do
        angle := !angle +. 2. *. pi *. twist /. iterations;
        if !angle > angle_finite then angle := angle_finite;
        let points = [ 
            
            int_of_float (!cx +. cos (!angle -. !a) *. !d),
            int_of_float (!cy +. sin (!angle -. !a) *. !d);
            int_of_float (!cx +. cos (!angle +. !a) *. !d),
            int_of_float (!cy +. sin (!angle +. !a) *. !d);
            int_of_float (!cx +. cos (!angle -. !a +. pi) *. !d),
            int_of_float (!cy +. sin (!angle -. !a +. pi) *. !d);
            int_of_float (!cx +. cos (!angle +. !a +. pi) *. !d),
            int_of_float (!cy +. sin (!angle +. !a +. pi) *. !d);
            int_of_float (!cx +. cos (!angle -. !a) *. !d),
            int_of_float (!cy +. sin (!angle -. !a) *. !d) ] in
        X.polyLine display root gc Origin points;
        delay (20.);
        X.polyLine display root gc Origin points;
        cx := !cx +. xstep;
        cy := !cy +. ystep;
        cw := !cw +. wstep;
        ch := !ch +. hstep;
        a := atan (!ch /. !cw);
        d := sqrt ((!cw /. 2.) *. (!cw /. 2.) +. (!ch /. 2.) *. (!ch /. 2.));
      done;
      Gwml.grabServer ()
  with _ ->
      Gwml.ungrabServer ();
      ()

      
let flip_resize w g1 g2 =  
  try
    if !!do_animation && not !prevent_animation then 
      let s = w.w_screen in
      Gwml.grabServer ();
      let scr = s.s_scr in
      let root = scr.scr_root in
      let white = scr.scr_white_pixel in
      let black = scr.scr_black_pixel in
      let gc = X.createGC display root [
          GCfonction GXxor;
          GCforeground white;
          GCbackground black;
          GCsubwindow_mode IncludeInferiors;
        ]
      in
      let x = g1.x in
      let y = g1.y in
      let w = g1.width in
      let h = g1.height in
      let fx = g2.x in
      let fy = g2.y in
      let fw = g2.width in
      let fh = g2.height in
      
      let iterations = float_of_int !iterations in      
      
      let xstep = (float_of_int (fx - x)) /. iterations in
      let ystep = (float_of_int (fy - y)) /. iterations in
      let wstep = (float_of_int (fw - w)) /. iterations in
      let hstep = (float_of_int (fh - h)) /. iterations in
      
      
      let cx = ref (float_of_int x) in
      let cy = ref (float_of_int y) in
      let cw = ref (float_of_int w) in
      let ch = ref (float_of_int h) in
      let angle_finite = 2. *. pi *. twist in
      
      let angle = ref 0. in
      while !angle < angle_finite do
        angle := !angle +. 2. *. pi *. twist /. iterations;
        
        if !angle > angle_finite then angle := angle_finite;
        
        let distortx = (!cw /. 10.) -. ((!cw /. 5.) *. sin !angle) in
        let distortch = (!ch /. 2.) *. cos !angle in
        let midy = !cy +. (!ch /. 2.) in
        let p0x = int_of_float (!cx +. distortx) in
        let p0y = int_of_float (midy -. distortch) in
        let p2y = int_of_float (midy +. distortch) in
        let points = [ 
            p0x, p0y;
            int_of_float (!cx +. !cw -. distortx), p0y;
            int_of_float (!cx +. !cw  +. distortx), p2y;
            int_of_float (!cx -. distortx), p2y;
            p0x, p0y] in
        X.polyLine display root gc Origin points;
        delay (20.);
        X.polyLine display root gc Origin points;
        cx := !cx +. xstep;
        cy := !cy +. ystep;
        cw := !cw +. wstep;
        ch := !ch +. hstep;      
      done;
      Gwml.grabServer ()
  with _ ->
      Gwml.ungrabServer ();
      ()
      
let turn_resize w g1 g2 =  
  try
    if !!do_animation && not !prevent_animation then 
      let s = w.w_screen in
      Gwml.grabServer ();
      let scr = s.s_scr in
      let root = scr.scr_root in
      let white = scr.scr_white_pixel in
      let black = scr.scr_black_pixel in
      let gc = X.createGC display root [
          GCfonction GXxor;
          GCforeground white;
          GCbackground black;
          GCsubwindow_mode IncludeInferiors;
        ]
      in
      let x = g1.x in
      let y = g1.y in
      let w = g1.width in
      let h = g1.height in
      let fx = g2.x in
      let fy = g2.y in
      let fw = g2.width in
      let fh = g2.height in
      
      let iterations = float_of_int !iterations in      
      
      let xstep = (float_of_int (fx - x)) /. iterations in
      let ystep = (float_of_int (fy - y)) /. iterations in
      let wstep = (float_of_int (fw - w)) /. iterations in
      let hstep = (float_of_int (fh - h)) /. iterations in
      
      
      let cx = ref (float_of_int x) in
      let cy = ref (float_of_int y) in
      let cw = ref (float_of_int w) in
      let ch = ref (float_of_int h) in
      let angle_finite = 2. *. pi *. twist in
      
      let angle = ref 0. in
      while !angle < angle_finite do
        angle := !angle +. 2. *. pi *. twist /. iterations;
        
        if !angle > angle_finite then angle := angle_finite;
        (*
  
      distorty = (ch / 10) - ((ch / 5) * sin (angle));
      distortcw = (cw / 2) * cos (angle);
      midx = cx + (cw / 2);

      points[0].x = midx - distortcw;
      points[0].y = cy + distorty;
      points[1].x = midx + distortcw;
      points[1].y = cy - distorty;
      points[2].x = points[1].x;
      points[2].y = cy + ch + distorty;
      points[3].x = points[0].x;
      points[3].y = cy + ch - distorty;
      points[4].x = points[0].x;
      points[4].y = points[0].y;

        *)
        let distorty = (!ch /. 10.) -. ((!ch /. 5.) *. sin !angle) in
        let distortcw = (!cw /. 2.) *. cos !angle in
        let midx = !cx +. (!cw /. 2.) in
        let p0x = int_of_float (midx -. distortcw) in
        let p0y = int_of_float (!cy +. distorty) in
        let p1x = int_of_float (midx +. distortcw) in
        let p1y = int_of_float (!cy -. distorty) in
        let p2x = p1x in
        let p2y = int_of_float (!cy +. !ch +. distorty) in
        let p3x = p0x in
        let p3y = int_of_float (!cy +. !ch -. distorty) in
        let p4x = p0x in
        let p4y = p0y in

        let points = [
            p0x, p0y; 
            p1x, p1y;
            p2x, p2y;
            p3x, p3y;
            p4x, p4y
          ]  in
        
        X.polyLine display root gc Origin points;
        delay (20.);
        X.polyLine display root gc Origin points;
        cx := !cx +. xstep;
        cy := !cy +. ystep;
        cw := !cw +. wstep;
        ch := !ch +. hstep;      
      done;
      Gwml.grabServer ()
  with _ ->
      Gwml.ungrabServer ();
      ()
      
let animation_list = [
    "Linear", linear_transform;
    "Twist", twist_resize;
    "Flip", flip_resize;
    "Turn", turn_resize;
  ]
  
let animation_option = sum_option animation_list
  
let resize_anim = define_option ["resize_anim"]  
  "<resize_anim> is the animation used for iconification if <do_animation>
  is enabled. Current possible values are: <Linear>, <Twist>, <Flip> and
  <Turn>."
    animation_option flip_resize 
  
let animation_menu () =
  List.map (fun (name, f) ->
      name, [], Function (fun _ ->
          resize_anim =:= f
      )
  ) animation_list
  

