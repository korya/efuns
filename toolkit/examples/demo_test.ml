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

let _ = default_background := "gray51"
  
  
let display = new WX_display.t ""
  
let root = new WX_root.t display 0

let top = new WX_top.t root None [IpadY 20;
    Bindings [Key (XK.xk_q,0), (fun () -> exit 0)]
    ]
  
let vbar = new WX_bar.v top#container [IpadX  10; PadX 10;]
  
let label1 = new WX_label.t vbar#container "Bonjour" [Relief ReliefRaised]
let pixmap = new WX_pixmap.t vbar#container
  ("xv",FromFile "/usr/share/icons/xv.xpm") [Relief ReliefRidge]
let label2 = new WX_label.t vbar#container "Encore ?"  [Relief ReliefSunken]

let hbar = new WX_bar.h vbar#container []

let adx = new WX_adjust.t ()
let ady = new WX_adjust.t ()
let viewport = new WX_viewport.t hbar#container adx ady 
    [MaxHeight 50; ExpandX true]
let vbar_0 = new WX_bar.v viewport#container []
let _ =
  List.iter (fun string ->
      let label = new WX_label.t vbar_0#container string [] in
      vbar_0#container_add label#contained) 
  ["label_1";"label_2";"label_3";"label_4";"label_5";"label_6";"label_7";"label_8"; "label_9"; "label_10"; "label_11"]  
  
let vscroll = new WX_scrollbar.v hbar#container ady []
  
let _ =
  label1#configure [Bindings 
    [EnterWindow, (fun () -> label1#inverse);
      LeaveWindow, (fun () -> label1#normal)]];
  viewport#container_add vbar_0#contained;
  hbar#container_add_s [viewport#contained;  vscroll#contained];
  
  vbar#container_add_s [
    label1#contained;    pixmap#contained;
    label2#contained;    hbar#contained];

  top#container_add (vbar#contained);
  
  top#show;
  loop ()
  