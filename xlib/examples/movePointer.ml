open Sys
open Xtypes
open Xlib
open X
  
let d = Xlib.openDisplay ""
let scr = d.dpy_roots.(0)
let root = scr.scr_root;;

let main () =
  if Array.length argv < 3 then
    failwith "2 arguments attendues";
  let dx = int_of_string argv.(1) in  
  let dy = int_of_string argv.(2) in
  let qp = X.queryPointer d root in
  X.warpPointer d root 0 0 scr.scr_width scr.scr_height
    root   (qp.qp_root_x + dx) (qp.qp_root_y + dy);
  closeDisplay d;
  ()
  
let _ = main ()