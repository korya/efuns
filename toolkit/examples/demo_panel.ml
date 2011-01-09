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

let _ = if Array.length Sys.argv <> 3 then 
    failwith "Usage: demo_panel file1 file2"
    
let filename1 = Sys.argv.(1)
let filename2 = Sys.argv.(2)

let root = new WX_root.from_display "" 0

let top = new WX_wmtop.t root  
    [Bindings [Key (XK.xk_q,0), (fun () -> exit 0)]]

let vbar = new WX_bar.v top#container []
let adj = new WX_adjust.t ()
let panel = new WX_panel.t Vertical vbar#container adj []
  
let viewtext parent filename =
  let hbar = new WX_bar.h parent#container [] in
  let adx = new WX_adjust.t () in
  let ady = new WX_adjust.t () in
  let viewport = new WX_viewport.t hbar#container adx ady 
      [MinHeight 50; MinWidth 50;ExpandX true] in
  let vscroll = new WX_scrollbar.v hbar#container ady [] in
  let text = new WX_text.of_file viewport#container filename [Background "black"; ExpandX true] in
  hbar#container_add_s [viewport#contained; vscroll#contained];  
  viewport#container_add (text#contained);  
  hbar#contained

let text1 = viewtext panel filename1
let text2 = viewtext panel filename2

let sep = new WX_panel.separator panel adj []
  
let _ =
  panel#set_first text1;
  panel#set_second text2;
  adj#set_pos 1 2; (* initial position is half panel for each window *)
  panel#set_step 5;
  vbar#container_add panel#contained;
  top#container_add (vbar#contained);  
  top#show;
  loop ()
  