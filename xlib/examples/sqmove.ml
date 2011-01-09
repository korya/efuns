(*
Code from Michel Quercia
*)

open XGraphics;;

let redraw x y =
  clear_graph();
  set_color blue;
  fill_poly [|100,100; 100,200; 200,200; 200,100; 100,100 |];
  set_color red;
  fill_poly [|x,y; x,y+100; x+100,y+100; x+100,y; x,y |];
  update()
;;


begin
  open_graph "" 600 400;
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
