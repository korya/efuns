(***********************************************************************)
(*                                                                     *)
(*                               GwML                                  *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Options
open Xtypes
open Xlib
open Gwml
open Stdconfig
  
let pager_size = define_option ["pager_size"] "" int_option 150
let pager_offset = define_option ["pager_offset"] "" int_option 10
let default_pos = (- !!pager_size - 2 * !!pager_offset)
let pager_x = define_option ["pager_x"] "" int_option default_pos
let pager_y = define_option ["pager_y"] "" int_option default_pos
let virtual_warp_to_window = 
  define_option ["virtual_warp_to_window"] "" bool_option false
  
let omit_move = Wobenv.new_var ()
let omit_draw = Wobenv.new_var ()

type info = {
    pager : pager;
    mutable foreground : string;
    mutable min_x : int;
    mutable min_y : int;
    mutable max_x : int;
    mutable max_y : int;
    mutable sz : int;
    mutable curr_x : int;
    mutable curr_y : int;
    mutable hooks : (wob -> unit) list;
  }
  
let info = Wobenv.new_var ()
          
let movep w = 
  try
    not !(Wob.getenv w.w_top omit_move)
  with Not_found -> true

let drawp w =
  try
    not !(Wob.getenv w.w_top omit_draw)
  with Not_found -> true
    
let viewablep w = 
  let s = w.w_screen.s_scr in
  let g = w.w_geometry in
  g.x + g.width > 0 && g.x < s.scr_width &&
  g.y + g.height > 0 && g.y < s.scr_height

let add_hook sw hook =
  let info =  Wob.getenv sw info  in
  info.hooks <- hook :: info.hooks
  
let update sw =
  let info =  Wob.getenv sw info  in
  let s = sw.w_screen in
  let scr = s.s_scr in
  let width = scr.scr_width in
  let height = scr.scr_height in
  let min_x = ref 0 in
  let max_x = ref width in
  let min_y = ref 0 in
  let max_y = ref height in
  Wintbl.iter (fun win (c,w) ->
      let w = w.w_top in
      let g = w.w_geometry in
      if is_viewable w (*c.c_wm_state = NormalState*) && drawp w then
        begin
          if g.x < !min_x then min_x := g.x;
          if g.x + g.width > !max_x then max_x := g.x + g.width;
          if g.y < !min_y then min_y := g.y;
          if g.y + g.height > !max_y then max_y := g.y + g.height;
        end
  ) s.s_clients;
  X.clearArea display info.pager.pager_window 0 0 0 0 false;
  let min_x = !min_x in
  let max_x = !max_x in
  let min_y = !min_y in
  let max_y = !max_y in
  let sz = max (max_x - min_x) (max_y - min_y) in
  let f g min = ((g - min) * !!pager_size) / sz in
  let gc = X.createGC display scr.scr_root 
      [GCforeground (color_make sw info.foreground)] in
  X.polyFillRectangle display info.pager.pager_window gc 
    [!!pager_offset + f 0 min_x,!!pager_offset + f 0 min_y,
    f width 0,f height 0];
  X.changeGC display gc [
    GCfonction GXxor;
    GCforeground (color_make sw "white" (* !!panner_foreground *));
    GCbackground (color_make sw "white" (* !!panner_foreground *));
    ];  
  Wintbl.iter (fun win (c,w) ->
      let w = w.w_top in
      if win = c.c_window && (*c.c_wm_state = NormalState*) is_viewable w && drawp w then
        let g = w.w_geometry in
        Xlib.drawRectangle display info.pager.pager_window gc 
        (!!pager_offset + f g.x min_x) (!!pager_offset + f g.y min_y)
        (f g.width 0) (f g.height 0);
  ) s.s_clients;
  info.min_x <- min_x;
  info.min_y <- min_y;
  info.max_x <- max_x;
  info.max_y <- max_y;
  info.sz <- sz;
  X.freeGC display gc;
  exec_hooks sw info.hooks

let sw_list = ref []
  
let update () =
  let list = !sw_list in
  sw_list := [];
  List.iter update list

let update sw =
  if not (List.memq sw !sw_list) then
    sw_list := sw :: !sw_list;
  Wob.after_events update
  
let warp_to w =
  let w = w.w_top in
  let s = w.w_screen.s_scr in
  (!virtual_manager)#goto w;
  Wob.send w WobRaiseWindow;
  X.warpPointer display s.scr_root 0 0 s.scr_width s.scr_height 
    w.w_window 20 20

let move w dx dy =
  let sw = w.w_top.w_parent in
  let s = sw.w_screen in
  let scr = s.s_scr in
  let width = scr.scr_width in
  let height = scr.scr_height in  
  let info =  Wob.getenv sw info  in
(*
  Wintbl.iter (fun win (c,w) ->
      let tw = w.w_top in
      if win = c.c_window && movep tw then
        let g = tw.w_geometry in
        begin
          g.x <- g.x + dx;
          g.y <- g.y + dy;
          if (* c.c_wm_state = NormalState*) is_viewable w && viewablep w then
            Wob.send tw WobMap
          else
            Wob.send tw (WobUnmap true)
        end
  ) s.s_clients;
  *)
  List.iter (fun tw ->
      if movep tw then
        let g = tw.w_geometry in
        begin
          g.x <- g.x + dx;
          g.y <- g.y + dy;
          if is_viewable tw && viewablep tw then
            Wob.send tw WobMap
          else
            Wob.send tw (WobUnmap true)
        end
  ) (List.rev !stack);
  info.curr_x <- info.curr_x - dx;
  info.curr_y <- info.curr_y - dy;
  Utils.catchexn "Virtual:" (fun _ -> update sw);
  if !!virtual_warp_to_window then 
    goto_closest sw    
    
let current_position w =
  let sw = w.w_top.w_parent in  
  let info =  Wob.getenv sw info  in info.curr_x , info.curr_y
    
let place_on_screen tw =
  let sw = tw.w_parent in
  let sg = sw.w_geometry in
  let g = tw.w_geometry in
  if g.x + g.width < 10 || g.x + 10 > sg.width then
    g.x <- modulo g.x sg.width;
  if g.y + g.height < 10 || g.y + 10 > sg.height then
    g.y <- modulo g.y sg.height;
  Wob.send_one tw WobMove

exception PagerWindow of wob
  
let pager_xevents sw ev =
  match ev.ev_event with
    ExposeEvent e -> 
      Wob.remove_expose ev;
      update sw
  | ButtonPressEvent e ->
      let s = sw.w_screen in
      let scr = s.s_scr in
      let xx = e.Xbutton.x_event in
      let yy = e.Xbutton.y_event in
      let info = Wob.getenv sw info in
      let min_x = info.min_x in
      let min_y = info.min_y in
      let sz = info.sz in
      let x = (xx - !!pager_offset) * sz / !!pager_size + min_x in
      let y = (yy - !!pager_offset) * sz / !!pager_size + min_y in
      if e.Xbutton.detail = 1 then
        let dx = (div x scr.scr_width) * scr.scr_width in
        let dy = (div y scr.scr_height) * scr.scr_height in
        move sw (-dx) (-dy)
      else
      if e.Xbutton.detail = 2 then
        (* find the window under the mouse *)
        try
          Wintbl.iter (fun win (c,w) ->
              let tw = w.w_top in
              if win = c.c_window then
                let g = tw.w_geometry in
                if drawp w && g.x <= x && g.y <= y &&
                  x - g.x <= g.width && y - g.y <= g.height &&
                  Wob.sgetenv tw is_in_dvroom_var false
                then
                (* maybe this one *)
                  raise (PagerWindow w)
          ) s.s_clients;        
        with
          PagerWindow w ->
        (* grab the pointer *)
            raiseWindow display info.pager.pager_window;
            X.changeActivePointerGrab display 
              [PointerMotionMask; ButtonMotionMask; ButtonReleaseMask;
              ButtonPressMask; OwnerGrabButtonMask]
              (cursor_make sw (FontCursor XC.xc_cross)) !Eloop.event_time;
            let root = sw.w_window in
            let sz = info.sz in
            let f g min = ((g - min) * !!pager_size) / sz in
            let fg = color_make sw "white" (*!panner_foreground*) in
            let bg = color_make sw "black" (*!panner_background*) in
            let gc = X.createGC display root [
                GCfonction GXxor;
                GCforeground fg;  
                GCbackground bg;
                ] in
            let g = w.w_top.w_geometry in
            let old_x = ref xx in
            let old_y = ref yy in
            let motion x y =         
              let ax = !!pager_offset + f g.x min_x + !old_x - xx in
              let ay = !!pager_offset + f g.y min_y + !old_y - yy in
              let dx = f g.width 0 in
              let dy = f g.height 0 in
              Xlib.drawRectangle display info.pager.pager_window gc ax ay dx dy;
              let ax = !!pager_offset + f g.x min_x + x - xx in
              let ay = !!pager_offset + f g.y min_y + y - yy in
              let dx = f g.width 0 in
              let dy = f g.height 0 in
              Xlib.drawRectangle display info.pager.pager_window gc ax ay dx dy;
              old_x := x;
              old_y := y;
            in
            let finish x y =
              g.x <- g.x + (x - xx) * sz / !!pager_size;
              g.y <- g.y + (y - yy) * sz / !!pager_size;
              Wob.send w.w_top WobMove
            in
            try
              while true do
                let ev = Xlib.peekEvent display in
                match ev.ev_event with
                | ButtonPressEvent e  -> raise Exit
                | ButtonReleaseEvent e ->
                    finish e.Xbutton.x_event e.Xbutton.y_event; 
                    raise Exit
                | MotionNotifyEvent e -> 
                    motion e.Xmotion.x_event e.Xmotion.y_event
                | _ -> Xlib.putBackEvent display ev
              done
            with
              _ -> 
                X.ungrabPointer display currentTime;
                move sw 0 0
      else
      if e.Xbutton.detail = 3 then 
        begin
          try
            Wintbl.iter (fun win (c,w) ->
                let tw = w.w_top in
                if win = c.c_window then
                  let g = tw.w_geometry in
                  if drawp w && g.x <= x && g.y <= y &&
                    x - g.x <= g.width && y - g.y <= g.height then
                (* maybe this one *)
                    raise (PagerWindow w)
            ) s.s_clients;        
          with
            PagerWindow w -> warp_to w
        end
  | EnterNotifyEvent _ -> 
      let info = Wob.getenv  sw info in
      raiseWindow display info.pager.pager_window;
      Wob.send sw (WobInstallColormap false)
  | _ -> ()

let leave_pane = ref true

let pane_xevents sw dx dy w ev =
  if !!Gwml.debug_events then (Log.printf "Pane: %s\n" 
        (event_to_string ev));
  match ev.ev_event with
    EnterNotifyEvent e ->
      leave_pane := false;
      if not !!pan_on_click then
        Concur.Thread.add_timer 
          ((!!edge_moving_resist) *. 0.001) (fun _ ->
            if not !leave_pane then
              let s = sw.w_screen in
              let scr = s.s_scr in
              leave_pane := true;
              X.warpPointer display scr.scr_root 
                0 0
                scr.scr_width scr.scr_height
                scr.scr_root      
                (if dx = 0 then e.Xcrossing.x_root else
                if dx > 0 then  scr.scr_width - !!warp_offset else
                  !!warp_offset)
              (if dy = 0 then e.Xcrossing.y_root else
                if dy > 0 then  scr.scr_height - !!warp_offset else
                  !!warp_offset);
              move sw dx dy)
        
  | LeaveNotifyEvent _ ->
      leave_pane := true
      
  | ButtonPressEvent e ->
      if !!pan_on_click then
        let s = sw.w_screen in
        move sw dx dy;
        let scr = s.s_scr in
        X.warpPointer display scr.scr_root 
          0 0
          scr.scr_width scr.scr_height
          scr.scr_root      
          (if dx = 0 then e.Xbutton.x_root else
          if dx > 0 then  scr.scr_width - !!warp_offset else !!warp_offset)
        (if dy = 0 then e.Xbutton.y_root else
          if dy > 0 then  scr.scr_height - !!warp_offset else !!warp_offset)
  | VisibilityNotifyEvent e ->
      if e.Xvisibility.state <> VisibilityUnobscured then
        raiseWindow display w
  | CirculateNotifyEvent _ -> 
      raiseWindow display w        
  | ConfigureNotifyEvent _ -> ()
  | _ -> 
      raiseWindow display w
      
let pane sw cursor dx dy x y w h =
  let s = sw.w_screen in
  let scr = s.s_scr in
  let win = X.createWindow display scr.scr_root
      x y w h 
      copyDepthFromParent InputOnly copyVisualFromParent
      0 [      
      CWEventMask [ButtonPressMask; EnterWindowMask; LeaveWindowMask;
        VisibilityChangeMask; StructureNotifyMask];
      CWOverrideRedirect true;CWCursor cursor]
  in
  gwml_ontop_windows := win :: !gwml_ontop_windows;
  let f = pane_xevents sw dx dy win in
  Eloop.add_window s.s_scheduler win f; 
  X.mapWindow display win

let w_hook desc e =
  match e with
    WobMap | WobUnmap _ | WobDestroy | WobResize _ | WobMove -> 
      update desc#wob.w_parent
  | _ -> ()
  
let t_hook w = w.w_oo#add_hook (w_hook w.w_oo)
  
let start sw =
  let s = sw.w_screen in
  let scr = s.s_scr in
  let page = createSimpleWindow display scr.scr_root 
      (if !!pager_x >= 0 then !!pager_x else
        scr.scr_width + !!pager_x) (if !!pager_y >= 0 then !!pager_y else 
        scr.scr_height + !!pager_y)
    (!!pager_size+ 2 * !!pager_offset) (!!pager_size+ 2 * !!pager_offset)
    0 [
      CWBackPixel (color_make sw !!panner_background);
      CWEventMask (ExposureMask :: ButtonPressMask :: EnterWindowMask::
          Top.client_mask); (* We must also receive client window events *)
      ]
  in
  Icccm.setWM_NAME display page "pager";
  Icccm.setWM_CLASS display page ["gwml_pager";"GwML"];  
  Eloop.add_window s.s_scheduler page (pager_xevents sw);
  X.mapWindow display page;
  (* horizontal *)
  pane sw (cursor_make sw (FontCursor XC.xc_top_side))
  0  scr.scr_height       0 0  (scr.scr_width-3) 3;
  pane sw (cursor_make sw (FontCursor XC.xc_bottom_side))
  0 (-scr.scr_height) 0 (scr.scr_height-3) (scr.scr_width-3) 3;
  (* vertical *)  
  pane sw (cursor_make sw (FontCursor XC.xc_left_side))
  scr.scr_width 0     0 0                 3 scr.scr_height;
  pane sw (cursor_make sw (FontCursor XC.xc_right_side))
  (-scr.scr_width) 0  (scr.scr_width-3) 0 3 scr.scr_height;
  (* corners *)

  let pager = { 
      pager_window = page;
      pager_width = (!!pager_size + 2 * !!pager_offset);
      pager_height = (!!pager_size + 2 * !!pager_offset);
    } in
  let v_info =   {
    pager = pager;
    foreground = "CadetBlue";
    min_x = 0;
      min_y = 0;
      max_x = 0;
      max_y = 0;
    sz = 0;
      curr_x = 0;
      curr_y = 0;
      hooks = [];
  } in
  Wob.setenv sw info v_info;
  update sw;
  s.s_top_opening_hooks <- t_hook :: s.s_top_opening_hooks;
  ()

let omit_move w bool =
  let w = w.w_top in
  (if not (viewablep w) then
      let w = w.w_top in
      let g = w.w_geometry in
      let s = w.w_screen.s_scr in
      g.x <- modulo g.x s.scr_width;
      g.y <- modulo g.y s.scr_height;
      Wob.send w WobMap
  ); 
  try (Wob.getenv w omit_move):= bool with _ -> 
      Wob.setenv w omit_move (ref bool)

let omit_draw w bool =
  let w = w.w_top in  
  try (Wob.getenv w omit_draw):= bool with _ -> 
      Wob.setenv w omit_draw (ref bool)
      
let goto w =
  let w = w.w_top in
  let g = w.w_geometry in
  let s = w.w_screen.s_scr in
  let width = s.scr_width in
  let height = s.scr_height in
  let dx = div g.x width in  
  let dy = div g.y height in
  move w.w_parent (- dx * width) (- dy * height);
  if !!virtual_warp_to_window then begin
      let tw = w.w_top in
      X.warpPointer display s.scr_root 0 0 s.scr_width s.scr_height
        tw.w_window 10 10;
      Wob.send tw WobRaiseWindow
    end

class manager = (object
  method pager w = 
    let sw = w.w_top.w_parent in
    let info =  Wob.getenv sw info  in
    info.pager
  method update = update
  method move = move
  method start = start
  method omit_move = omit_move
  method omit_draw = omit_draw
  method movep = movep
  method drawp = drawp
  method goto = goto
  method place_on_screen = place_on_screen
  method current_position = current_position
  method add_hook = add_hook
end : Stdconfig.virtual_manager)

let manager = new manager