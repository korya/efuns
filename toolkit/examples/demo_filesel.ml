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
  
let display = new WX_display.t ""
let root = new WX_root.t display 0
  
open WX_filesel
let info = {
    filter = "/*";
    current_selection= "/";
    predicat = (fun _ -> true);
    action = (fun s -> Printf.printf "Select %s" s; print_newline ();
        exit 0);
    cancel = (fun _ -> exit 1);
  }
  
let filesel = new WX_filesel.t root info []
let _ =
  filesel#show;
  loop ()
  
    