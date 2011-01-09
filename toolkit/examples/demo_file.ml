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
  

let hbar = new WX_bar.h top#container []

let adx = new WX_adjust.t ()
let ady = new WX_adjust.t ()
let viewport = new WX_viewport.t hbar#container adx ady 
    [MinHeight 50; MinWidth 50;ExpandX true]
  
let vscroll = new WX_scrollbar.v hbar#container ady []

let text = new WX_text.with_widgets viewport#container [||] 
    [Relief ReliefRaised; Foreground "red"; Background "yellow"]

let filetext = new WX_text.of_file viewport#container "demo_file.ml"
    [Relief ReliefRaised; Foreground "red"; Background "yellow"]

let button = new WX_button.t text#container []
    
let pixmap = new WX_pixmap.t button#container
    ("xv",FromFile "/usr/share/icons/xv.xpm") [Relief ReliefRidge]

let selector = new WX_selector.t text#container root
    { WX_selector.labels = [| "France";"Angleterre";"Allemagne" |];
    WX_selector.valides = [||];
    WX_selector.current = 0;
    WX_selector.change_hook = []; } []
  
let simple_text = [|
    [| WX_text.RealString ([], "Bonjour") |];
    [| WX_text.RealString ([], "Je suis un peu compliqué, non ?") |];
    [| WX_text.RealString ([], "") |];
    [| WX_text.RealString ([], "Pourtant, il suffit d'un peu") |];
    [| WX_text.RealString ([], "d'habitude pour arriver à") |];
    [| WX_text.RealString ([], "m'utiliser correctement") |];
    [| WX_text.RealString ([], "") |];
    [| WX_text.RealString ([],"Une pixmap:");WX_text.Widget [|button#contained|] |];
    [| WX_text.RealString ([], "Ici, en plus, je n'utilise pas") |];
    [| WX_text.RealString ([], "toutes mes capacités.") |];
    [| WX_text.RealString ([], "Un selecteur:"); WX_text.Widget [|selector#contained|]|]
  |]
  
let menu_desc = [|
    "File", (fun _ -> Printf.printf "FILE ITEM"; print_newline ());
    "Save", (fun _ -> Printf.printf "SAVE ITEM"; print_newline ());
    "Print", (fun _ -> Printf.printf "PRINT ITEM"; print_newline ());
    "Quit", (fun _ -> Printf.printf "QUIT ITEM"; print_newline ());
    "Delete", (fun _ -> Printf.printf "DELETE ITEM"; print_newline ());
    "New", (fun _ -> Printf.printf "NeW ITEM"; print_newline ());
    |]
let menu = new WX_popup.t root menu_desc
  
let _ =
  
  button#set_action (fun _ -> 
      let (x,y) = button#root_coordinates in
      menu#popup x y (Some !button_event)
      );
  button#container_add pixmap#contained;
  button#set_wait_release false;
  text#set_widgets simple_text;
  hbar#container_add_s [viewport#contained; vscroll#contained];
(*  viewport#container_add (text#contained); *)
  viewport#container_add (filetext#contained);
  top#container_add (hbar#contained);  
  top#show;
  loop ()
  