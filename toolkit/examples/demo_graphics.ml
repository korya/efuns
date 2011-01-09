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

let help_menu = [|
    "About", (fun _ -> Printf.printf "ABOUT HELP"; print_newline ());
    "Index", (fun _ -> 
        let dialog = new WX_dialog.t root
            "Sorry,\nNo help available on this topic" [] in
        dialog#add_button "OK" (fun _ -> dialog#destroy);
        dialog#show;
        Printf.printf "INDEX HELP"; print_newline ());
  |]

  (* This replace the open_graph call. *)
let graphics = new WX_Graphics.t appli#container [] 600 400
  
let _ =
  appli#container_add (graphics#contained);  
  appli#add_menu "Help" help_menu;
  appli#show
  
open XGraphics (* instead of Graphics *)

let redraw x y =
  clear_graph();
  set_color blue;
  fill_poly [|100,100; 100,200; 200,200; 200,100; 100,100 |];
  set_color red;
  fill_poly [|x,y; x,y+100; x+100,y+100; x+100,y; x,y |];
  update()
;;


begin
  set_update_style FlushAll;
  let _ = wait_next_event[Button_down] in
  redraw 0 0;
  while 
    let ev = wait_next_event[Button_up;Button_down;Mouse_motion;Key_pressed] in
    if ev.button then redraw ev.mouse_x ev.mouse_y;
    ev.key <> 'q'
  do () done;
  close_graph()
end;;
