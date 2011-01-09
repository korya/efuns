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
open WX_tree
  
let display = new WX_display.t ""
let root = new WX_root.t display 0
let top = new WX_wmtop.t root [MinWidth 10; MinHeight 10]
let tree = new WX_tree.t top#container []
let tree2 = new WX_tree.t tree#container []
let list = [ "Bonjour"; "Hello"; "Gutten tag" ]
let list2 = [ "Salut"; "Coucou"; "Hi"; "Good morning" ]
  
let _ =
  let map = List.map (fun s ->
        leaf 0 (new WX_label.t tree#container s [])#contained
    ) list in
  let map3 = List.map (fun s ->
        leaf 0 (new WX_label.t tree#container s [])#contained
    ) list in
  let map2 = List.map (fun s ->
        leaf 0 (new WX_label.t tree2#container s [])#contained
    ) list2 in
  let map = map @ ((branch false
        (new WX_label.t tree#container "Autres" [])#contained
        tree2#contained) :: map3) in
  tree2#set_desc map2;
  tree#set_desc map;
  top#container_add tree#contained;
  top#show;
  loop ()