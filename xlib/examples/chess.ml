(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* dessiner un echiquier *)

open Xtypes
open Xlib

(* quelques valeurs utiles *)
let dpy = openDisplay "";;
let black= defaultBlack dpy;;
let white= defaultWhite dpy;;
let fixed= X.openFont dpy "fixed";;
let root = defaultRoot dpy;;

(* un rectangle *)
let r= rect 10 10 500 500

(* la fenetre principale *)
let win= createSimpleWindow dpy root 10 10 500 500 1 [CWBackPixel white]

let _ = 
  let _ = selectInput dpy win [KeyPressMask] in
  X.mapWindow dpy win

(* on cree les sous-fenetres *)
let width = 50
let subwindow x y couleur=
  let w = createSimpleWindow dpy win (x*width) (y*width) width width 1 [CWBackPixel couleur]
  in
  X.mapWindow dpy w
    

let defcolor= defaultColormap dpy;;
let { anc_pixel = red } = X.allocNamedColor dpy defcolor "red";;
let { anc_pixel = green } = X.allocNamedColor dpy defcolor "green";;

for i=0 to 7 do
  for j=0 to 7 do
       subwindow i j (if( (i+j) mod 2)=0 then red else green)
    done
  done
;;

let _ =
  while true do
    let ev = Xlib.nextEvent dpy in
    match ev.ev_event with
      KeyPressEvent k when ev.ev_window == win -> Pervasives.exit 1
    | _ -> ()
  done



