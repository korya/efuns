(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
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

  (* Do not ask the user to place a window if the window is not mapped. *)
let interactive_place_p c = 
  match c.c_wm_state with
    IconicState | WithdrawnState -> false
  | _ -> true
  
let place_window c = 
  c.c_new_window && c.c_transient_for = noWindow && 
  not c.c_size_hints.user_position
  
let user_placement_delay = 
  define_option ["user_placement_delay"] 
    "How long the system waits before automatic placement"
  float_option 5.0

let no_placement _ _ = ()
  
let user_placement_failure_ref = ref no_placement
  
let rec may_abort tw = 
  let last = !User.motion_event in
  let current = !User.time in
  Concur.Thread.add_timer (!!user_placement_delay /. 2.) (fun _ ->
      if not !User.aborted  && !User.time = current then
      if !User.motion_event > last then
        may_abort tw
      else
          begin
            User.abort tw;
          end)  
  
let user_placement c tw =
  if interactive_place_p c then
    if place_window c then
      begin
        may_abort tw;
        User.place tw false;
        if !User.aborted then
          (!user_placement_failure_ref) c tw;
        User.aborted := true;
      end
    else
      !user_placement_failure_ref c tw
      
let near_mouse c tw =
  (* we can place transient windows with near_mouse ... *)
  if place_window c || not (c.c_transient_for = noWindow) then
    let qp = X.queryPointer display tw.w_screen.s_scr.scr_root in
    let g = tw.w_geometry in
    g.x <- max (qp.qp_root_x - 50) 0;
    g.y <- max (qp.qp_root_y - 50) 0;
    Wob.send tw WobMove

let print_rect r =
  (*
  Printf.printf "{ x = %d, y = %d, dx = %d, dy = %d }"
    r.x r.y r.width r.height;
  print_newline ()
*)
  ()
  
let intersect rw rs =
  if rw.x > rs.x + rs.width || rw.x + rw.width <= 0 ||
    rw.y > rs.y + rs.height || rw.y + rw.width <= 0 then [rs] else
  let rwxx = rw.x + rw.width in
  let rwyy = rw.y + rw.height in
  let rsxx = rs.x + rs.width in
  let rsyy = rs.y + rs.height in
  let r1 = {  (* left *)
      x = rs.x;
      y = rs.y;
      width = max 0 (rw.x - rs.x);
      height = rs.height;
      border = 0;
    } in
  
  let r2 = { (* top *)
      x = rs.x;
      y = rs.y;
      width = rs.width;
      height = max 0 (rw.y - rs.y);
      border = 0;
    } in
  
  let x = max (min rwxx rsxx) rs.x in
  let r3 = { (* right *)
      x = x;
      y = rs.y;
      width = max 0 (rsxx - x);
      height = rs.height;
      border = 0;
    } in

  let y = max rs.y (min rwyy rsyy) in
  let r4 = { (* bottom *)
      x = rs.x;
      y = y;
      width = rs.width;
      height = max 0 (rsyy - y);
      border = 0;
    } in
  let add r l =
    print_rect r;
    if
      r.x >= rs.x && r.y >= rs.y && r.width > 0 && r.height > 0
    then r :: l else l
  in
  add r1 (add r2 (add r3 (add r4 [])))

let auto_placement_avoid = ref ["GwML"]

let rec included r list =
  match list with
    [] -> false
  | rr :: tail ->
      if r.x >= rr.x && r.x+r.width <= rr.x + rr.width &&
        r.y >= rr.y && r.y + r.height <= rr.y + rr.height then 
        true
      else included r tail
  
let free_space w =
  let clients = list_clients w in
  let s = w.w_screen in
  let scr = s.s_scr in
  let r = { x = 0; y = 0; width = scr.scr_width; height = scr.scr_height;
      border = 0;}
  in
  let list = List.fold_left (fun list (c,w) ->
        if c.c_wm_state = NormalState && (not c.c_new_window) &&
          not (List.mem (fst c.c_class) !auto_placement_avoid) &&
          not (List.mem (snd c.c_class) !auto_placement_avoid)
          then
          let g = w.w_top.w_geometry in
          let list = 
            List.flatten (List.map (intersect g) list)
          in
          let list = simplify_list list [] in
          (*
          List.iter (fun r ->
              print_rect r;
              if r.x + r.width > scr.scr_width then begin
                  Printf.printf "TOO LARGE"; print_newline ();
                end;
              if r.y + r.height > scr.scr_height then begin
                  Printf.printf "TOO TALL"; print_newline ();
                end;
          ) list;
          *)
          list
        else list
    ) [r] clients in
  List.iter (fun r -> r.border <- r.width * r.height) list;
  let list = Sort.list (fun r1 r2 -> r1.border > r2.border) list in
  let rec remove_dup list =
    match list with
      [] -> []
    | r ::  tail ->
        if included r tail then begin
            remove_dup tail
          end else
          r :: remove_dup tail
  in
  let list = remove_dup list in
  list
    
let random_placement c tw = 
  if place_window c then
    let g = tw.w_geometry in
    let s = tw.w_screen in
    let scr = s.s_scr in
    let max_x = max 0 (scr.scr_width - g.width) in
    let max_y = max 0 (scr.scr_height - g.height) in
    g.x <- Random.int max_x;
    g.y <- Random.int max_y;
    Wob.send tw WobMove
  
let auto_placement_failure_ref = ref user_placement

let auto_min_size = define_option ["auto_min_size"] "" int_option 100000
  
let auto_placement c tw =
  if place_window c then
    let g = tw.w_geometry in
    if g.width * g.height < !!auto_min_size then
      !auto_placement_failure_ref c tw
    else
    let list = free_space tw in
    let best = ref None in
    List.iter (fun r ->
        if g.width <= r.width && g.height <= r.height then
          match !best with
            None -> best := Some r
          | Some rr ->
          (* best match is biggest area *)
              if rr.width * rr.height < r.width * r.height then
                best := Some r
    ) list;
    match !best with
    | Some r ->
        g.x <- r.x;
        g.y <- r.y;
        Wob.send tw WobMove
    | None -> 
    (* with a 10 % margin now ... *)
        List.iter (fun r ->
            if g.width * 10 <= r.width * 11 && g.height * 10 <= r.height * 11
            then
              match !best with
                None -> best := Some r
              | Some rr ->
          (* best match is biggest area *)
                  if rr.width * rr.height < r.width * r.height then
                    best := Some r
        ) list;        
        match !best with
        | Some r ->
            g.x <- r.x;
            g.y <- r.y;
            let scr = tw.w_screen.s_scr in
            if g.x + g.width > scr.scr_width + 5 then
              g.x <- scr.scr_width - g.width;
            if g.y + g.height > scr.scr_height + 5 then
              g.y <- scr.scr_height - g.height;
            Wob.send tw WobMove
        | None -> 
            !auto_placement_failure_ref c tw
          
  (* A cascade is a list of points to place windows in. *)
type cascade = {
    mutable cascade_x : int;
    mutable cascade_y : int;
    mutable cascade_dx : int;
    mutable cascade_dy : int;
  }

let cascade_var = Wobenv.new_var ()
  
let cascade_placement cascade c w =
  if place_window c then begin
  (* Find free slots in the cascade *)
      let busy = Array.create 100 false in
      let dx = if cascade.cascade_dx = 0 then 1 else cascade.cascade_dx in
      let dy = if cascade.cascade_dy = 0 then 1 else cascade.cascade_dy in
      List.iter  (fun (c,w) ->
          try
            let tw = w.w_top in
            let casc = Wob.getenv tw cascade_var in
            if casc == cascade then
              let tg = tw.w_geometry in 
              let vx = (tg.x - cascade.cascade_x) / dx in
              let vy = (tg.y - cascade.cascade_y) / dy in
              if vx <= 99 && vx >= 0 && vy <= 99 && vy >= 0 then
                begin
                  busy.(vx) <- true;
                  busy.(vy) <- true
                end
          with _ -> ()
      ) (list_clients w);
      try
        for i = 0 to 99 do
          if not busy.(i) then begin
              let tw = w.w_top in
              let g = tw.w_geometry in
              g.x <- cascade.cascade_x + i * cascade.cascade_dx;
              g.y <- cascade.cascade_y + i * cascade.cascade_dy;
              Wob.setenv tw cascade_var cascade;
              Wob.send tw WobMove;
              raise Exit
            end
        done;
        random_placement c w
      with _ -> 
          ()
    end
    
let icon_placement cascade c tw = 
  if place_window c || not (c.c_transient_for == noWindow) then begin
      cascade_placement cascade c tw;
      Wob.send tw (WobIconifyRequest true);
      c.c_wm_state <- IconicState;
      set_stay_iconified tw true
    end

let default_cascade = {
    cascade_x = 10;
    cascade_y = 10;
    cascade_dx = 50;
    cascade_dy = 50;
  }
  
let icon_position = icon_placement default_cascade
let cascade_position = cascade_placement default_cascade

let xy_position_var = Wobenv.new_var ()  
  
let xy_placement c tw =
  try
    let (x,y) = find_option c xy_position_var in
    let g = tw.w_top.w_geometry in 
    g.x <- x;
    g.y <- y
  with _ -> auto_placement c tw
        
let add_xy_position (classe, name) coords =
  let name = 
    if name = "" then  classe else
    if classe = "" then name else
      classe^"."^name
  in
  add_option name xy_position_var coords

let placement_list = 
  [
    "auto", auto_placement;
    "random", random_placement;
    "user", user_placement;
    "near_mouse", near_mouse;
    "icon", icon_position;
    "cascade", cascade_position;
  ]

let synonymes_list = 
  [
    "no_placement", no_placement;
    "auto_placement", auto_placement;
    "random_placement", random_placement;
    "user_placement", user_placement;
    "icon_placement", icon_position;
    "cascade_placement", cascade_position;
  ]
  
let placement_option = sum_option [
    "auto", auto_placement;
    "random", random_placement;
    "user", user_placement;
    "near_mouse", near_mouse;
    "icon", icon_position;
    "cascade", cascade_position;
    "no_placement", no_placement;
    "auto_placement", auto_placement;
    "random_placement", random_placement;
    "user_placement", user_placement;
    "icon_placement", icon_position;
    "cascade_placement", cascade_position;
  ]
  
let default_placement = define_option ["default_placement"]
  "The default placement used for all windows"
    placement_option auto_placement

let user_placement_failure = define_option ["user_placement_failure"]
    "The placement used if the user doesn't place the window"
    placement_option !user_placement_failure_ref

let auto_placement_failure = define_option ["auto_placement_failure"]
    "The placement used if no place is left for the window for automatic placement"
    placement_option !auto_placement_failure_ref
  
let _ =
  auto_placement_failure_ref := !!auto_placement_failure;
  user_placement_failure_ref := !!user_placement_failure;
  option_hook user_placement_failure (fun _ ->
      user_placement_failure_ref := !!user_placement_failure);
  option_hook auto_placement_failure (fun _ ->
      auto_placement_failure_ref := !!auto_placement_failure)
  
  
let placement_menu () =
  List.map (fun (name, f) ->
      name, [], Function (fun _ ->
            default_placement =:= f
        )
  ) placement_list
  