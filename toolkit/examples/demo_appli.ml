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

let appli = new WX_appli.t root  
  [Bindings [Key (XK.xk_q,0), (fun () -> exit 0)]]

let hbar = new WX_bar.h appli#container []

let adx = new WX_adjust.t ()
let ady = new WX_adjust.t ()
let viewport = new WX_viewport.t hbar#container adx ady 
    [MinHeight 50; MinWidth 50;ExpandX true]
  
let vscroll = new WX_scrollbar.v hbar#container ady []

let filetext = new WX_text.of_file viewport#container "demo_appli.ml"
    [Relief ReliefRaised; Foreground "red"; Background "yellow"]
  
let file_menu = [|
    "New", (fun _ -> Printf.printf "NEW FILE"; print_newline ());
    "Open", (fun _ -> Printf.printf "OPEN FILE"; print_newline ());
    "Save", (fun _ -> Printf.printf "SAVE FILE"; print_newline ());
    "Kill", (fun _ ->
        let dialog = new WX_dialog.t root
          "If you kill the buffer, 
last changes will not be saved on disk.

Are you sure you want this ?"
          [] in
        dialog#add_button "OK" (fun _ -> 
            Printf.printf "KILL OK"; print_newline ();
            dialog#destroy);
        dialog#add_button "CANCEL" (fun _ -> 
            Printf.printf "KILL CANCELLED"; print_newline ();
            dialog#destroy);
        dialog#add_button "HELP" (fun _ -> 
            Printf.printf "HELP ON KILL"; print_newline (););
        dialog#show;
        
    );
    "Print", (fun _ -> Printf.printf "PRINT FILE"; print_newline ());
    "Quit", (fun _ -> Printf.printf "QUIT FILE"; print_newline ());
  |]
let edit_menu = [|
    "Undo", (fun _ -> Printf.printf "UNDO EDIT"; print_newline ());
    "Cut", (fun _ -> Printf.printf "CUT EDIT"; print_newline ());
    "Copy", (fun _ -> Printf.printf "COPY EDIT"; print_newline ());
    "Paste", (fun _ -> Printf.printf "PASTE EDIT"; print_newline ());
  |]
let help_menu = [|
    "About", (fun _ -> Printf.printf "ABOUT HELP"; print_newline ());
    "Index", (fun _ -> 
        let dialog = new WX_dialog.t root
            "Sorry,\nNo help available on this topic" [] in
        dialog#add_button "OK" (fun _ -> dialog#destroy);
        dialog#show;
        Printf.printf "INDEX HELP"; print_newline ());
  |]
  
  
let _ =
  hbar#container_add_s [viewport#contained; vscroll#contained];
  viewport#container_add (filetext#contained);
  appli#container_add (hbar#contained);  
  appli#add_menu "File" file_menu;
  appli#add_menu "Edit" edit_menu;
  appli#add_separator;
  appli#add_menu "Help" help_menu;
  appli#show;
  loop ()
  