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

let appli = new WX_appli.t root  
    [Bindings [Key (XK.xk_q,0), (fun () -> exit 0)]]

let attributes = [Relief ReliefRaised; Foreground "red"; Background "yellow"]

let vbar = new WX_bar.v appli#container []
let adj = new WX_adjust.t ()
let panel = new WX_panel.t Vertical vbar#container adj []
let scale = new WX_scale.h vbar#container adj [Background "black"]
  
let cursors = [
    "xc_x_cursor",XC.xc_x_cursor;
    "xc_arrow",XC.xc_arrow;
    "xc_based_arrow_down",XC.xc_based_arrow_down;
    "xc_based_arrow_up",XC.xc_based_arrow_up;
    "xc_boat",XC.xc_boat;
    "xc_bogosity",XC.xc_bogosity;
    "xc_bottom_left_corner",XC.xc_bottom_left_corner;
    "xc_bottom_right_corner",XC.xc_bottom_right_corner;
    "xc_bottom_side",XC.xc_bottom_side;
    "xc_bottom_tee",XC.xc_bottom_tee;
    "xc_box_spiral",XC.xc_box_spiral;
    "xc_center_ptr",XC.xc_center_ptr;
    "xc_circle",XC.xc_circle;
    "xc_clock",XC.xc_clock;
    "xc_coffee_mug",XC.xc_coffee_mug;
    "xc_cross",XC.xc_cross;
    "xc_cross_reverse",XC.xc_cross_reverse;
    "xc_crosshair",XC.xc_crosshair;
    "xc_diamond_cross",XC.xc_diamond_cross;
    "xc_dot",XC.xc_dot;
    "xc_dotbox",XC.xc_dotbox;
    "xc_double_arrow",XC.xc_double_arrow;
    "xc_draft_large",XC.xc_draft_large;
    "xc_draft_small",XC.xc_draft_small;
    "xc_draped_box",XC.xc_draped_box;
    "xc_exchange",XC.xc_exchange;
    "xc_fleur",XC.xc_fleur;
    "xc_gobbler",XC.xc_gobbler;
    "xc_gumby",XC.xc_gumby;
    "xc_hand1",XC.xc_hand1;
    "xc_hand2",XC.xc_hand2;
    "xc_heart",XC.xc_heart;
    "xc_icon",XC.xc_icon;
    "xc_iron_cross",XC.xc_iron_cross;
    "xc_left_ptr",XC.xc_left_ptr;
    "xc_left_side",XC.xc_left_side;
    "xc_left_tee",XC.xc_left_tee;
    "xc_leftbutton",XC.xc_leftbutton;
    "xc_ll_angle",XC.xc_ll_angle;
    "xc_lr_angle",XC.xc_lr_angle;
    "xc_man",XC.xc_man;
    "xc_middlebutton",XC.xc_middlebutton;
    "xc_mouse",XC.xc_mouse;
    "xc_pencil",XC.xc_pencil;
    "xc_pirate",XC.xc_pirate;
    "xc_plus",XC.xc_plus;
    "xc_question_arrow",XC.xc_question_arrow;
    "xc_right_ptr",XC.xc_right_ptr;
    "xc_right_side",XC.xc_right_side;
    "xc_right_tee",XC.xc_right_tee;
    "xc_rightbutton",XC.xc_rightbutton;
    "xc_rtl_logo",XC.xc_rtl_logo;
    "xc_sailboat",XC.xc_sailboat;
    "xc_sb_down_arrow",XC.xc_sb_down_arrow;
    "xc_sb_h_double_arrow",XC.xc_sb_h_double_arrow;
    "xc_sb_left_arrow",XC.xc_sb_left_arrow;
    "xc_sb_right_arrow",XC.xc_sb_right_arrow;
    "xc_sb_up_arrow",XC.xc_sb_up_arrow;
    "xc_sb_v_double_arrow",XC.xc_sb_v_double_arrow;
    "xc_shuttle",XC.xc_shuttle;
    "xc_sizing",XC.xc_sizing;
    "xc_spider",XC.xc_spider;
    "xc_spraycan",XC.xc_spraycan;
    "xc_star",XC.xc_star;
    "xc_target",XC.xc_target;
    "xc_tcross",XC.xc_tcross;
    "xc_top_left_arrow",XC.xc_top_left_arrow;
    "xc_top_left_corner",XC.xc_top_left_corner;
    "xc_top_right_corner",XC.xc_top_right_corner;
    "xc_top_side",XC.xc_top_side;
    "xc_top_tee",XC.xc_top_tee;
    "xc_trek",XC.xc_trek;
    "xc_ul_angle",XC.xc_ul_angle;
    "xc_umbrella",XC.xc_umbrella;
    "xc_ur_angle",XC.xc_ur_angle;
    "xc_watch",XC.xc_watch;
    "xc_xterm",XC.xc_xterm]

let cursors_make top =  
  let hbar = new WX_bar.h top#container [MaxHeight 400] in
  let adx = new WX_adjust.t () in
  let ady = new WX_adjust.t () in
  let viewport = new WX_viewport.t hbar#container adx ady [] in
  let scrollbar = new WX_scrollbar.v hbar#container ady [] in
  let vbar = new WX_bar.v viewport#container [] in  
  hbar#container_add_s [viewport#contained; scrollbar#contained];
  viewport#container_add vbar#contained;
  List.iter (fun (name,curs) -> 
      let label = new WX_label.t vbar#container name 
          [Cursor (FontCursor curs);Relief ReliefRaised; ExpandX true] in
      label#configure [Bindings
          [EnterWindow, (fun () -> label#inverse);
          LeaveWindow, (fun () -> label#normal)]];
      vbar#container_add label#contained
  ) cursors;
  top#configure [Bindings [
      Key(XK.xk_Prior,0),(fun _ -> ady#page_up);
      Key(XK.xk_Next,0),(fun _ -> ady#page_down);
      Key(XK.xk_q,0),(fun _ -> exit 0);
    ]];
  viewport#configure [MinHeight 500; MinWidth 100];
  hbar
  
let viewtext parent constructor =
  let hbar = new WX_bar.h parent#container [] in
  let adx = new WX_adjust.t () in
  let ady = new WX_adjust.t () in
  let viewport = new WX_viewport.t hbar#container adx ady 
      [MinHeight 50; MinWidth 50;ExpandX true] in
  let vscroll = new WX_scrollbar.v hbar#container ady [] in
  let text = constructor viewport in
  hbar#container_add_s [viewport#contained; vscroll#contained];  
  viewport#container_add (text#contained);  
  hbar#contained


let menu_desc = [|
    "File", (fun _ -> Printf.printf "FILE ITEM"; print_newline ());
    "Save", (fun _ -> Printf.printf "SAVE ITEM"; print_newline ());
    "Print", (fun _ -> Printf.printf "PRINT ITEM"; print_newline ());
    "Quit", (fun _ -> Printf.printf "QUIT ITEM"; print_newline ());
    "Delete", (fun _ -> Printf.printf "DELETE ITEM"; print_newline ());
    "New", (fun _ -> Printf.printf "NeW ITEM"; print_newline ());
  |]
let menu = new WX_popup.t root menu_desc

let text1 = viewtext panel (fun parent -> 
      let hbar = new WX_bar.h parent#container [] in
      let text = new WX_text.with_widgets hbar#container
          [||] attributes in
      let pixmap = new WX_pixmap.t text#container
          ("xv",FromFile "/usr/share/icons/xv.xpm") [Relief ReliefRidge]
      in
      
      let button = new WX_button.t text#container [] in

      let compteur = new WX_label.t text#container "" [] in
      
      let pixmap = new WX_pixmap.t button#container
          ("xv",FromFile "/usr/share/icons/xv.xpm") [Relief ReliefRidge]
      in  
      let selector = new WX_selector.t text#container root
          { WX_selector.labels = [| "France";"Angleterre";"Allemagne" |];
          WX_selector.valides = [||];
          WX_selector.current = 0;
          WX_selector.change_hook = []; } []
      in 
      let ledit = new WX_ledit.t text#container "Modifier ici" [] in
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
          [| WX_text.RealString ([], "Un selecteur:"); WX_text.Widget [|selector#contained|]|];
          [| WX_text.RealString ([], "Un label editable:"); WX_text.Widget [|ledit#contained|]|];          
          [| WX_text.RealString ([], "Un compteur:"); WX_text.Widget [|compteur#contained|]|];          
        |]
      in  
      
      button#set_action (fun _ -> 
          let (x,y) = button#root_coordinates in
          menu#popup x y (Some !button_event)
      );
      button#container_add pixmap#contained;
      button#set_wait_release false;
      text#set_widgets simple_text;
      let cur = cursors_make hbar in
      
      let vbar = new WX_bar.v hbar#container [] in
      let adj = new WX_adjust.t () in
      adj#add_subject (fun _ ->
          let v = adj#get_pos WX_adjust.v_total in  
          let s = string_of_int v in
          compteur#set_string s
      );
      
      Array.iteri (fun i s ->
          let button = new WX_radiobutton.t vbar#container adj i [ExpandX true] in
          let label = new WX_label.t button#container s [ExpandX true] in
          button#container_add label#contained;
          vbar#container_add button#contained;
          button#select
      ) [| "First"; "Second"; "Third";"Forth";"Fifth";"Sixth" |];      
      hbar#container_add_s [text#contained; cur#contained; vbar#contained];
      hbar
  )

let text2 = viewtext panel (fun parent -> 
      new WX_text.of_file parent#container 
        "demo_complex.ml" attributes)


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
    "Quit", (fun _ -> exit 0);
  |]
let edit_menu = [|
    "Undo", (fun _ -> Printf.printf "UNDO EDIT"; print_newline ());
    "Cut", (fun _ -> Printf.printf "CUT EDIT"; print_newline ());
    "Copy", (fun _ -> Printf.printf "COPY EDIT"; print_newline ());
    "Paste", (fun _ -> Printf.printf "PASTE EDIT"; print_newline ());
  |]
let help_menu = [|
    "About", (fun _ -> 
        let dialog = new WX_dialog.t root
          "Some help on this demo:
          The top frame is a WX_appli.t object,
          which automatically creates the top bar of menus, and the
          vertical bar below. This bar contains two widgets:
          - a WX_panel.t, which is divided in two parts
          - a WX_scale.t (in black) which controls the sizes of the
          two parts of the panel (use it like a scrollbar).

          In the panel, the bottom side contains a text, loaded
          from a file with WX_text.file, and its scrollbar, of
          class wX_scrollbar.v. Above an horizontal bar contains
          a text with active widgets (a WX_pixmap.t in a WX_button.t)
          and a WX_selector.t, and a vertical bar displaying all
          sorts of X predefined cursors (you can use its scrollbar,
          or use Prior and Next keys). To be enable displaying only
          parts of some windows (in combinaison with a scrollbar for
          example), the widgets are included in WX_viewport.t.          

          Finally, the File/Kill menu displays a simple WX_dialog.t 
          box with three choices.
    " [] in
        dialog#add_button "OK" (fun _ -> dialog#destroy);
        dialog#show;
        Printf.printf "ABOUT HELP"; print_newline ());
    "Index", (fun _ -> 
        let dialog = new WX_dialog.t root
            "Sorry,\nNo help available on this topic" [] in
        dialog#add_button "OK" (fun _ -> dialog#destroy);
        dialog#show;
        Printf.printf "INDEX HELP"; print_newline ());
  |]

let sep = new WX_panel.separator panel adj []
  
let _ =
  panel#set_first text1;
  panel#set_second text2;
  adj#set_pos 1 2;
  panel#set_step 5;
  vbar#container_add_s [panel#contained; scale#contained];
  appli#container_add (vbar#contained);  
  appli#add_menu "File" file_menu;
  appli#add_menu "Edit" edit_menu;
  appli#add_separator;
  appli#add_menu "Help" help_menu;
  appli#show;
  loop ()
  