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

open WX_types

let filename = Sys.argv.(1)
  
let root = new WX_root.from_display "" 0

let top = new WX_wmtop.t root []
let hbar = new WX_bar.h top#container []

let adx = new WX_adjust.t ()
let ady = new WX_adjust.t ()
let viewport = new WX_viewport.t hbar#container adx ady 
    [MinHeight 50; MinWidth 50;ExpandX true]
let vscroll = new WX_scrollbar.v hbar#container ady []

let filetext = new WX_text.of_file viewport#container filename
    [Relief ReliefRaised; Foreground "red"; Background "yellow"]  
  
let _ =
  hbar#container_add_s [viewport#contained; vscroll#contained];
  viewport#container_add (filetext#contained);
  top#container_add (hbar#contained);  
  top#configure [Bindings [
      Key(XK.xk_Prior,0),(fun _ -> ady#page_up);
      Key(XK.xk_Next,0),(fun _ -> ady#page_down);
      Key(XK.xk_q,0),(fun _ -> exit 0);
    ]];
  top#show;
  loop ()
  