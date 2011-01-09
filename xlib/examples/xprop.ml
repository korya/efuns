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

(*
    Le programme xprop en CAML
*)

open Xtypes
open Xlib;;

(* connectons nous au display pour commencer *)
let dpy = openDisplay ""
let root = defaultRoot dpy;;

(* un curseur + *)
let cursor = createFontCursor dpy  XC.xc_crosshair;;

(* xprop dpy;; pour utiliser *)
let xprop dpy =
  List.iter 
    (function (atom,typ,n1,n2,value) ->
       print_string (atom^"("^typ^")= "^
        (
	  if typ = "STRING" then value else ""
	));
       print_newline ()

    )

    (
      listProperties dpy (
    	let win=Xmu.selectWindow dpy root cursor in
	  if win = noWindow then root
	  else
            Xmu.clientWindow dpy win
      ))
 ;;

xprop dpy;;


