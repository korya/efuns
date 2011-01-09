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
  
let display = new WX_display.t ""
let root = new WX_root.t display 0
let top = new WX_wmtop.t root [MinWidth 300; MinHeight 300]
let table = new WX_table.t top#container [] 10 10 true
  
let list = [
    1,1,2,3,"yellow"; 
    2,5,2,2,"red";
    5,2,2,2,"blue";
    6,6,3,3,"pink";
  ]

let _ =
  List.iter (fun (x,y,dx,dy,color) ->
      let button = new WX_button.t table#container [] in
      let obj = new WX_object.t button#container [
          Background color; MinWidth (dx * 50); MinHeight (dy * 50)] in
      button#set_action (fun _ -> Printf.printf "Select: %s" color;
          print_newline ());
      button#container_add obj#contained;
      table#container_add button#contained x y dx dy;      
  ) list;
  top#container_add table#contained;
  top#show;
  loop ()
