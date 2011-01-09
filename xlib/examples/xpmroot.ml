(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xtypes
open Xbuffer
open Xlib
  
let dpyname = ref ""  

let main filename =
  
  let dpy = openDisplay !dpyname in
  let screen = defaultScreen dpy in
  let root = defaultRoot dpy in
  let cmap = defaultColormap dpy in
  let depth = defaultDepth dpy in
  let (dx,dy,_,pix,_) = Xpm.createPixmapFromFile dpy root cmap depth filename in
  X.changeWindowAttributes dpy root [CWBackPixmap pix];
  X.clearArea dpy root 0 0 0 0 false;
  let prop = try X.internAtom dpy "_XSETROOT_ID" false with Not_found -> failwith "interAtom" in
  let _ =
    try
      let gp = X.getProperty dpy root false prop anyPropertyType 0 4 in
      if gp.gp_type = XA.xa_pixmap && gp.gp_format = 32 &&
        gp.gp_length = 1 && gp.gp_left = 0 then
        let pix = Xbuffer.getCard32 gp.gp_value 0 in
        X.killClient dpy pix
    with
      Not_found -> ()
  in
  X.changeProperty dpy root PropModeReplace
    prop XA.xa_pixmap 1 (let b = newString 1 in setEnum32 b 0 pix; b);
  X.setCloseDownMode dpy RetainPermanent;
  Xlib.closeDisplay dpy
  

let _ = 
  Arg.parse [
    "-d", Arg.String (fun s -> dpyname := s), "display: set display";
    ] main  "Usage: xpmroot filename.xpm"

