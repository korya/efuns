(***********************************************************************)
(*                                                                     *)
(*                           Gwml                                      *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Options
open Xtypes
open X
open Xlib  
open Stdconfig
open Gwml  

let move_pointer w x y =
  let sw = w.w_top.w_parent in
  let sg = sw.w_geometry in
  X.warpPointer display sw.w_window 0 0 sg.width sg.height sw.w_window x y

let user_vmove_allowed = define_option ["user_vmove_allowed"] 
    "" bool_option true
let user_vmove_delay = define_option ["user_vmove_delay"]
    "" float_option 2.0
  
let noOwnerEvents = false
  
let peek_window action s () =
  let screen = s.s_scr in
  let root = screen.scr_root in
  let rec iter () = 
    let ev = Xlib.peekEvent display in
    match ev.ev_event with
      ButtonPressEvent e -> e
    | _ -> 
        Xlib.putBackEvent display ev;
        iter ()
  in
  let e = iter () in
  let button = e.Xbutton.detail in
  let qp = X.queryPointer display s.s_scr.scr_root in
  let win = if qp.qp_win = noWindow then qp.qp_root else qp.qp_win in
  action display win;
  let rec iter () = 
    let ev = Xlib.peekEvent display in
    match ev.ev_event with
      ButtonReleaseEvent e when e.Xbutton.detail = button -> ()
    | _ -> 
        Xlib.putBackEvent display ev;
        iter ()
  in
  iter () 

type move_resize = Place | Move | Resize

let time = ref 0
let motion_event = ref 0  
let aborted = ref false

let move_mode mode = match mode with
    Resize -> false
  | _ -> true
  
let move_resize w mode from_event =
  aborted := false;
  let vmove_time = ref 0.0 in
  let s = w.w_screen in
  let (root_x,root_y,button) =
    if from_event then
      match Eloop.last_event s.s_scheduler with
      | ButtonPressEvent e ->  
          e.Xbutton.x_root, e.Xbutton.y_root, e.Xbutton.detail
      | ButtonReleaseEvent e ->  
          e.Xbutton.x_root, e.Xbutton.y_root, e.Xbutton.detail
      | KeyPressEvent e ->  
          e.Xkey.x_root, e.Xkey.y_root, 0
      | KeyReleaseEvent e ->  
          e.Xkey.x_root, e.Xkey.y_root, 0
      | _ -> 
          let qp = X.queryPointer display w.w_screen.s_scr.scr_root in
          qp.qp_root_x, qp.qp_root_y, 0
    else
    let qp = X.queryPointer display w.w_screen.s_scr.scr_root in
    qp.qp_root_x, qp.qp_root_y, 0
  in
  let t = w.w_top in
  let s = w.w_screen in
  let sw = t.w_parent in
  let sg = sw.w_geometry in
  let screen = s.s_scr in
  let root = screen.scr_root in
  let white = screen.scr_white_pixel in
  let black = screen.scr_black_pixel in
  let hand_cursor = createFontCursor display XC.xc_hand2 in
  let mgc = X.createGC display root [
      GCfonction GXxor;
      GCforeground white;
      GCbackground black;
      GCsubwindow_mode IncludeInferiors;
    ]
  in
  let g = t.w_geometry in
  if mode = Place then begin
      g.x <- root_x;
      g.y <- root_y;
    end;  
  let gx = g.x in
  let gy = g.y in
  let min_width, min_height, inc_x, inc_y =
    try
      let (c,cw) = Wintbl.find s.s_clients t.w_window in 
      let nh = c.c_size_hints in
      let win = t.w_window in
      let min_width, min_height = match nh.min_size with
          None -> 0,0
        | Some (w,h) -> w, h in
      let inc_x,inc_y = match nh.resize_inc with
          None -> 1,1
        | Some (w,h) -> w,h in
      min_width, min_height, inc_x, inc_y
    with _ -> 1,1,1,1
  in
  let box_drawn = ref false in
  let r = { x= g.x; y=g.y; width=g.width + 2 * g.border; 
      height=g.height + 2 * g.border; border = 0;} in
  let draw_box () = 
    if move_mode mode  && !!opaque_move then
      moveWindow display t.w_window r.x r.y;
    Xlib.drawRectangle display root mgc  r.x r.y r.width r.height in
  let unbox f arg =
    if !box_drawn then draw_box ();
    f arg;
    draw_box ();
    box_drawn := true;    
  in
  let motion () = 
    let qp = X.queryPointer display root in 
    if (!!user_vmove_allowed) &&
      (qp.qp_root_x < 3) || (qp.qp_root_x > sg.width - 3) ||
      (qp.qp_root_y < 3) || (qp.qp_root_y > sg.height - 3)
    then
      if !vmove_time <> 0.0 then begin
          let curtime = real_time !Eloop.event_time in
          if curtime -. !vmove_time > !!user_vmove_delay *. 1000.0 then begin
              if qp.qp_root_x < 3 then begin
                  (!virtual_manager)#move sw sg.width 0;
                  move_pointer sw !!warp_offset qp.qp_root_y
                end;
              if qp.qp_root_x > sg.width - 3 then begin
                  (!virtual_manager)#move sw (-sg.width) 0;
                  move_pointer sw (sg.width - !!warp_offset) qp.qp_root_y
                end;
              if qp.qp_root_y < 3 then begin
                  (!virtual_manager)#move sw 0 sg.height;
                  move_pointer sw qp.qp_root_x !!warp_offset 
                end;
              if qp.qp_root_y > sg.height - 3 then begin
                  (!virtual_manager)#move sw 0 (-sg.height);
                  move_pointer sw qp.qp_root_x (sg.height - !!warp_offset)
                end;
              g.x <- gx;
              g.y <- gy;
            end
        end 
      else vmove_time := real_time !Eloop.event_time
        else
      vmove_time := 0.0;
    if move_mode mode then
      begin
        r.x <- g.x + (qp.qp_root_x - root_x);
        r.y <- g.y + (qp.qp_root_y - root_y);
        if !!confine_move then
          begin
            if r.x + g.width > sg.width then r.x <- sg.width - g.width;
            if r.y + g.height > sg.height then r.y <- sg.height - g.height;
            if r.x < 0 then r.x <- 0;
            if r.y < 0 then r.y <- 0;
          end;
      end
    else
      begin
        r.width <- g.width  +  (qp.qp_root_x - root_x);
        r.height <- g.height + (qp.qp_root_y - root_y);
        if r.width < min_width then r.width <- min_width;
        if r.height < min_height then r.height <- min_height;
        r.width <- ((r.width - min_width) / inc_x) * inc_x
          + min_width;
        r.height <- ((r.height - min_height) / inc_y) * inc_y
          + min_height;
      end;
  in
  let var_x = ref 0 in
  let var_y = ref 0 in
  let key_motion (k,m) =
    incr motion_event;
    if move_mode mode then begin
        var_x := r.x;
        var_y := r.y;
      end else begin
        var_x := r.width;
        var_y := r.height;
      end;
    if m land shiftMask <> 0 then
      if k = XK.xk_Left then var_x := !var_x - 1 else
      if k = XK.xk_Right then var_x := !var_x + 1 else
      if k = XK.xk_Up then var_y := !var_y - 1 else
      if k = XK.xk_Down then var_y := !var_y + 1 else
        decr motion_event
    else
    if (m land controlMask <> 0) && (!!wm_modifiers land controlMask = 0) then
      if k = XK.xk_Left then var_x := !var_x - 5 else
      if k = XK.xk_Right then var_x := !var_x + 5 else
      if k = XK.xk_Up then var_y := !var_y - 5 else
      if k = XK.xk_Down then var_y := !var_y + 5 else
        decr motion_event
    else
    if k = XK.xk_Left then var_x := !var_x - 15 else
    if k = XK.xk_Right then var_x := !var_x + 15 else
    if k = XK.xk_Up then var_y := !var_y - 15 else
    if k = XK.xk_Down then var_y := !var_y + 15 else
      decr motion_event;
    
    if move_mode mode then begin
        r.x <- !var_x;
        r.y <- !var_y;
      end else begin
        r.width <- !var_x;
        r.height <- !var_y;
      end;
  in
  let motion = unbox motion in
  let key_motion = unbox key_motion in
  let finish () =
    (* motion (); *)
    if !box_drawn then draw_box (); box_drawn := false;
    if move_mode mode then begin
        r.width <- g.width;
        r.height <- g.height;
        Animate.linear_move t g r;
        g.x <- r.x;
        g.y <- r.y;
        Wob.send t WobMove
      end else 
      Top.resize_top t r.width r.height;
  in
  if (
      try
        if button=0 then begin
            (* why should we keep the position given by root ? *)
            try
              X.grabPointer display screen.scr_root noOwnerEvents
                [PointerMotionMask; ButtonMotionMask; ButtonReleaseMask;
                ButtonPressMask; OwnerGrabButtonMask; 
              ]
                GrabModeAsync GrabModeAsync
                noConfineTo hand_cursor currentTime;
              X.grabKeyboard display screen.scr_root noOwnerEvents
                GrabModeAsync GrabModeAsync currentTime;
              false
            with
              GrabError AlreadyGrabbed -> 
  (* The pointer might be grabbed by another client. On my alpha linux,
  the XV dialog window is mapped before the ButtonRelease, preventing
  the wm from correctly grabbing the pointer. So, we don't try to
  move the window.
  *)            
                true
          end
        else begin
            X.changeActivePointerGrab display 
              [PointerMotionHintMask; ButtonMotionMask; ButtonReleaseMask;
              ButtonPressMask; OwnerGrabButtonMask;]
              hand_cursor !Eloop.event_time;
            false
          end
      with
        XError e -> Printf.printf "Error in Grab Pointer: "; Xlib.printXError e;
          print_newline (); false
      | e -> Printf.printf "Exception in Grab Pointer %s" (Printexc.to_string e);
          print_newline (); false
            ) then
    begin (* pointer already grabbed by another client *)
      finish ();
      Wob.send w WobRaiseWindow;
      X.freeGC display mgc;
    end
  else
    begin
      if !!grab_server then Gwml.grabServer ();
      draw_box (); box_drawn := true;
      try
        while true do
          let ev = Xlib.peekEvent display in
          match ev.ev_event, button with
          | ButtonPressEvent e, 0  ->
              vmove_time := 0.0;
              finish ();
              raise Exit
          | ButtonReleaseEvent e, _ ->
              vmove_time := 0.0;
              finish ();
              raise Exit
          | ButtonPressEvent _, _ ->
              vmove_time := 0.0;
              raise Exit
          | MotionNotifyEvent _,_ -> 
              incr motion_event;
              Eloop.update_event_time ev;
              motion ()
          | KeyPressEvent e, _ ->
              vmove_time := 0.0;
              let modifiers = e.Xkey.state in
              let (s,keysym,m) = KeyBind.lookupString display ev.ev_event in
              if keysym = XK.xk_Return then begin
                  finish ();
                  raise Exit;
                end else 
              if keysym = XK.xk_BackSpace then begin
                  finish ();
                  raise Exit
                end else
                key_motion (keysym, modifiers)
          | ExposeEvent e,_ -> 
              Eloop.handle_event s.s_scheduler ev
          | _ -> Xlib.putBackEvent display ev
      done
    with
      _ -> ()
    end;
  X.ungrabPointer display currentTime;
  X.ungrabKeyboard display currentTime;
  incr time;
  if !box_drawn then draw_box ();
  if !!grab_server then Gwml.ungrabServer ();
  X.freeGC display mgc

let abort w = 
  if not !aborted then
    begin (* Send a synthetic event to stop the loop *)
      aborted := true;
      X.sendEvent display w.w_window true [ButtonPressMask] (ButtonPressEvent
          {  
          Xbutton.detail = 0;
          Xbutton.time = !Eloop.event_time;
          Xbutton.root = w.w_top.w_parent.w_window;
          Xbutton.event = w.w_window;
          Xbutton.child = w.w_window;
          Xbutton.x_event = 0;
          Xbutton.y_event = 0;
          Xbutton.x_root = 0;
          Xbutton.y_root = 0;
          Xbutton.state = 0;
          Xbutton.same_screen = false;
        })
    end

let move w = move_resize w Move
let place w = move_resize w Place
let resize w = move_resize w Resize
  
let select w =
  let t = w.w_top in
  let s = w.w_screen in
  let sw = t.w_parent in
  let sg = sw.w_geometry in
  let screen = s.s_scr in
  let root = screen.scr_root in
  let white = screen.scr_white_pixel in
  let black = screen.scr_black_pixel in
  let hand_cursor = createFontCursor display XC.xc_hand2 in
  
  let rec iter () = 
    let ev = Xlib.peekEvent display in
    match ev.ev_event with
    | ButtonPressEvent _ 
    | KeyPressEvent _ 
    | ButtonReleaseEvent _ 
    | KeyReleaseEvent _ 
    | MotionNotifyEvent _ ->
        let qp = X.queryPointer display root in
        if qp.qp_modifiers <> 0 then
          iter ()
    | _ -> 
        Xlib.putBackEvent display ev;
        iter ()
  in
  let qp = X.queryPointer display root in
  if qp.qp_modifiers <> 0 then iter ();
  
  X.grabPointer display root noOwnerEvents
    [PointerMotionMask; ButtonMotionMask; ButtonReleaseMask;
    ButtonPressMask; OwnerGrabButtonMask; KeyPressMask; KeyReleaseMask;]
    GrabModeAsync GrabModeAsync
    noConfineTo hand_cursor currentTime;

  let rec iter () = 
    let ev = Xlib.peekEvent display in
    match ev.ev_event with
    | ButtonPressEvent e  ->
        Eloop.update_time s.s_scheduler ev;
        let qp = X.queryPointer display s.s_scr.scr_root in
        let win = if qp.qp_win = noWindow then qp.qp_root else qp.qp_win in
        win
    | KeyPressEvent _
    | ButtonReleaseEvent _ 
    | KeyReleaseEvent _ 
    | MotionNotifyEvent _ -> iter ()
    | ExposeEvent e -> Eloop.handle_event s.s_scheduler ev; iter ()
    | _ -> 
        Xlib.putBackEvent display ev;
        iter ()
  in
  let win = iter () in
  X.ungrabPointer display currentTime;
  if win = root then raise Not_found;
  let rec iter win =
    let qt = X.queryTree display win in
    if qt.qt_parent = qt.qt_root then win else iter qt.qt_parent
  in
  iter win
  
    
let client_top w =
  try
    let _ = w.w_oo#client in
    w.w_top
  with _ ->
      let win = select w in
      let (c,w) = Wintbl.find w.w_screen.s_clients win in
      w.w_top

let twm_ResizeCursor = ref (FontCursor XC.xc_fleur)          
let twm_resize w from_event =
  let s = w.w_screen in
  let (root_x,root_y,button) =
    if from_event then
      match Eloop.last_event s.s_scheduler with
      | ButtonPressEvent e ->  
          e.Xbutton.x_root, e.Xbutton.y_root, e.Xbutton.detail
      | ButtonReleaseEvent e ->  
          e.Xbutton.x_root, e.Xbutton.y_root, e.Xbutton.detail
      | KeyPressEvent e ->  
          e.Xkey.x_root, e.Xkey.y_root, 0
      | KeyReleaseEvent e ->  
          e.Xkey.x_root, e.Xkey.y_root, 0
      | _ -> 
          let qp = X.queryPointer display w.w_screen.s_scr.scr_root in
          qp.qp_root_x, qp.qp_root_y, 0
    else
    let qp = X.queryPointer display w.w_screen.s_scr.scr_root in
    qp.qp_root_x, qp.qp_root_y, 0
  in
  let mode_x = ref 0 in
  let mode_y = ref 0 in
  let t = w.w_top in
  let s = w.w_screen in
  let sw = t.w_parent in
  let sg = sw.w_geometry in
  let screen = s.s_scr in
  let root = screen.scr_root in
  let white = screen.scr_white_pixel in
  let black = screen.scr_black_pixel in
  let hand_cursor = cursor_make w !twm_ResizeCursor in
  let mgc = X.createGC display root [
      GCfonction GXxor;
      GCforeground white;
      GCbackground black;
      GCsubwindow_mode IncludeInferiors;
    ]
  in
  let g = t.w_geometry in
  let min_width, min_height, inc_x, inc_y =
    try
      let (c,cw) = Wintbl.find s.s_clients t.w_window in 
      let nh = c.c_size_hints in
      let win = t.w_window in
      let min_width, min_height = match nh.min_size with
          None -> 0,0
        | Some (w,h) -> w, h in
      let inc_x,inc_y = match nh.resize_inc with
          None -> 1,1
        | Some (w,h) -> w,h in
      min_width, min_height, inc_x, inc_y
    with _ -> 1,1,1,1
  in
  let box_drawn = ref false in
  let r = { x= g.x; y=g.y; width=g.width + 2 * g.border; 
      height=g.height + 2 * g.border; border = g.border;} in
  let draw_box () = 
    let wt = r.width / 3 in
    let ht = r.height / 3 in
    Xlib.drawRectangle display root mgc  r.x r.y r.width r.height;
    Xlib.drawRectangle display root mgc  (r.x+wt) (r.y+1) (r.width-2*wt) (r.height-2);
    Xlib.drawRectangle display root mgc  (r.x+1) (r.y+ht) (r.width-2) (r.height-2*ht);
  in
  let motion () = 
    if !box_drawn then draw_box ();
    let qp = X.queryPointer display root in
    (* X modification *)
    if !mode_x = 0 then
      if r.x > qp.qp_root_x then
        ( mode_x := -1; 
          r.width <- r.width + (r.x - qp.qp_root_x);
          r.x <- qp.qp_root_x)
      else
      if r.x + r.width < qp.qp_root_x then
        ( mode_x := 1;
          r.width <- qp.qp_root_x - r.x)
      else ()
    else
    if !mode_x = 1 then
      if qp.qp_root_x > g.x then r.width <- qp.qp_root_x - r.x else ()
    else
    if qp.qp_root_x < g.x + g.width then 
      ( r.width <- r.width + (r.x - qp.qp_root_x);
        r.x <- qp.qp_root_x);
        (* Y modification *)
    if !mode_y = 0 then
      if r.y > qp.qp_root_y then
        ( mode_y := -1; 
          r.height <- r.height + (r.y - qp.qp_root_y);
          r.y <- qp.qp_root_y)
      else
      if r.y + r.height < qp.qp_root_y then
        ( mode_y := 1;
          r.height <- qp.qp_root_y - r.y)
      else ()
    else
    if !mode_y = 1 then
      if qp.qp_root_y > g.y then r.height <- qp.qp_root_y - r.y else ()
    else
    if qp.qp_root_y < g.y + g.height then 
      ( r.height <- r.height + (r.y - qp.qp_root_y);
        r.y <- qp.qp_root_y);
    draw_box ();
    box_drawn := true;
  in
  let finish () =
    motion ();
    if !box_drawn then draw_box (); box_drawn := false;
    g.x <- r.x; g.y <- r.y;
    Top.resize_top t r.width r.height;
  in
  if (match button with
        0 ->
          r.x <- root_x; g.x <- root_x;
          r.y <- root_y; g.y <- root_y;
          draw_box (); box_drawn := true;
          (try
              X.grabPointer display screen.scr_root false
                [PointerMotionMask; ButtonMotionMask; ButtonReleaseMask;
                ButtonPressMask; OwnerGrabButtonMask]
                GrabModeAsync GrabModeAsync
                noConfineTo hand_cursor currentTime;
              false
            with
              GrabError AlreadyGrabbed -> 
  (* The pointer might be grabbed by another client. On my alpha linux,
  the XV dialog window is mapped before the ButtonRelease, preventing
  the wm from correctly grabbing the pointer. So, we don't try to
  move the window.
  *)
                true
          )
      | _ -> 
          X.changeActivePointerGrab display 
            [PointerMotionHintMask; ButtonMotionMask; ButtonReleaseMask;
            ButtonPressMask; OwnerGrabButtonMask]
            hand_cursor !Eloop.event_time;
          false) then
    begin (* pointer already grabbed by another client *)
      finish ();
      Wob.send w WobRaiseWindow;
      X.freeGC display mgc;
    end
  else
    begin
      if !!grab_server then Gwml.grabServer ();
      try
        while true do
          let ev = Xlib.peekEvent display in
          match ev.ev_event, button with
        | ButtonPressEvent e, 0  ->
            finish ();
            X.ungrabPointer display currentTime;
            raise Exit
        | ButtonReleaseEvent e, _ ->
            finish ();
            raise Exit
        | ButtonPressEvent _, _ ->
            raise Exit
        | MotionNotifyEvent _,_ -> 
            motion ()
        | _ -> Xlib.putBackEvent display ev
      done
    with
      _ -> ()
  end;
  if !!grab_server then Gwml.ungrabServer ();
  if !box_drawn then draw_box ();
  X.freeGC display mgc

let _ = resize_mode := twm_resize