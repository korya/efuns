open Xtypes
open Xlib

let xbanner_font = "-bitstream-charter-bold-i-normal--125-*-*-*-*-*-iso8859-1"
let xbanner_x = 100
let xbanner_y = 100
let xbanner_delay = 0.5

let delay time =
  try ignore (Unix.select [] [] [] time) with _ -> ()
  
let xbanner dpy s =
  try
    let screen = defaultScreen dpy in
    let root = defaultRoot dpy in
    let white = screen.scr_white_pixel in
    let black = screen.scr_black_pixel in
    let font = X.openFont dpy xbanner_font in
    let gc = X.createGC dpy root [
        GCfonction GXxor;
        GCforeground white;
        GCbackground white;
        GCsubwindow_mode IncludeInferiors;
        GCfont font;
      ]
    in
    let x = xbanner_x in
    let y = xbanner_y in
    try
      X.grabServer dpy;
      Xlib.drawString dpy root gc x y s;
      delay xbanner_delay;
      Xlib.drawString dpy root gc x y s;
      X.freeGC dpy gc;
      X.ungrabServer dpy
    with Failure _ ->
        Printf.printf "Failure"; print_newline ();
        Xlib.drawString dpy root gc x y s;
        delay xbanner_delay;
        Xlib.drawString dpy root gc x y s;
        X.freeGC dpy gc;
    | e -> 
        Printf.printf "ungrab: %s" (Utils.printexn e); print_newline ();
        X.ungrabServer dpy

  with e -> 
      Log.printf "Error in Xbanner: %s\n" (Utils.printexn e)

let _ =
  let dpy = Xlib.openDisplay "" in
  xbanner dpy Sys.argv.(1)