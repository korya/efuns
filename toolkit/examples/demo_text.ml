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

let top = new WX_top.t root None [IpadY 20;
    Bindings [Key (XK.xk_q,0), (fun () -> exit 0)]
    ]
  

let hbar = new WX_bar.h top#container []

let adx = new WX_adjust.t ()
let ady = new WX_adjust.t ()
let viewport = new WX_viewport.t hbar#container adx ady 
    [MinHeight 50; MinWidth 50;ExpandX true; ExpandY true]
  
let vscroll = new WX_scrollbar.v hbar#container ady []

let text = new WX_text.with_widgets viewport#container [||] 
    [Relief ReliefRaised; Foreground "red"; Background "yellow"; 
    ExpandX true; ExpandY true
    ]

let button = new WX_button.t text#container []
    
let pixmap = new WX_pixmap.t button#container
    ("xv",FromFile "/usr/share/icons/xv.xpm") [Relief ReliefRidge]

let selector = new WX_selector.t text#container root
    { WX_selector.labels = [| "France";"Angleterre";"Allemagne" |];
    WX_selector.valides = [||];
    WX_selector.current = 0;
    WX_selector.change_hook = []; } []
  
let simple_text = [|
    [| WX_text.RealString ([], "Hello") |];
    [| WX_text.RealString ([
          WX_text.Foreground (top#color_make "green" true);
          WX_text.Font (top#font_make "10x20" true);
          
          ], "This window contains a text.") |];
    [| WX_text.RealString ([], "") |];
    [| WX_text.RealString ([], "But, in the text, you can insert") |];
    [| WX_text.RealString ([], "other widgets, with bindings.") |];
    [| WX_text.RealString ([], "For example,") |];
    [| WX_text.RealString ([], "") |];
    [| WX_text.RealString ([],"Here is a pixmap:");WX_text.Widget [|button#contained|] |];
    [| WX_text.RealString ([], "And here is another widget:") |];
    [| WX_text.RealString ([], "") |];
    [| WX_text.RealString ([], "a selector:"); WX_text.Widget [|selector#contained|] |];
    [| WX_text.RealString ([], "and I'm not completely implemented yet ...") |];
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
  viewport#container_add (text#contained); 
  top#container_add (hbar#contained);  
  top#show;
  loop ()
  