(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


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
  
open WX_types

let _ = default_background := "gray51"
  
  
let display = new WX_display.t ""
  
let root = new WX_root.t display 0

let top = new WX_top.t root None [MaxWidth 500; MaxHeight 500]  
let hbar = new WX_bar.h top#container []
let adx = new WX_adjust.t ()
let ady = new WX_adjust.t ()
let viewport = new WX_viewport.t hbar#container adx ady []
let scrollbar = new WX_scrollbar.v hbar#container ady []
let _ =
  hbar#container_add_s [viewport#contained; scrollbar#contained]
  
let vbar = new WX_bar.v viewport#container []
let _ = viewport#container_add vbar#contained

let _ =
  List.iter (fun (name,curs) -> 
      let label = new WX_label.t vbar#container name 
          [Cursor (FontCursor curs);Relief ReliefRaised; ExpandX true] in
      label#configure [Bindings
          [EnterWindow, (fun () -> label#inverse);
          LeaveWindow, (fun () -> label#normal)]];
      vbar#container_add label#contained
  ) cursors
      
let _ =
  top#configure [Bindings [
      Key(XK.xk_Prior,0),(fun _ -> ady#page_up);
      Key(XK.xk_Next,0),(fun _ -> ady#page_down);
      Key(XK.xk_q,0),(fun _ -> exit 0);
      ]];
    
  top#container_add (hbar#contained);
  viewport#configure [MinHeight 500; MinWidth 100];
 
  top#show;
  loop ()
  
  
