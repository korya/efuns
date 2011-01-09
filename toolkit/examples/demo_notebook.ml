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


open Xtypes
open WX_types
  
let display = new WX_display.t ""
let root = new WX_root.t display 0
let top = new WX_wmtop.t root [MinWidth 10; MinHeight 10; MaxWidth 600; MaxHeight 700]
let book = new WX_notebook.v top#container []
  
let files = [ "demo_tree.ml"; "demo_table.ml"; "demo_graphics.ml";
    "demo_filesel.ml"; "demo_calc.ml";"demo_file.ml";"demo_notebook.ml"]
  
let _ =  
  book#container_add_s (List.map (fun name -> name,
        (new WX_text.of_file book#container name [])#contained
        ) files);
  top#container_add book#contained;
  top#show;
  loop ()