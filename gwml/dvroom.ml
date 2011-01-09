(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Options
open Xtypes
open Stdconfig
open Gwml

  (****  
  We may have 3 reasons for a window not to be viewable:
  - it is iconified (IconicState)
  - it is in another room (WithdrawnState)
  - it is in another vscreen (WithdrawnState)
  ***)

let change_room_hooks = ref []
  
type room = {
    mutable room_name : string;
  }

let current_room_prop = "_GWML_CURRENT_ROOM"
let window_rooms_prop = "_GWML_WINDOWS_ROOM"
  
let current_room_atom = X.internAtom display current_room_prop false
let window_rooms_atom = X.internAtom display window_rooms_prop false
  
(* A room is a set of windows. Each window belongs to at least one room.
If no room is specified for a window, the window belongs to all rooms. *)

(* the list of valid rooms in a screen *)
let screen_rooms_var = Wobenv.new_var ()
  
(* the list of rooms a window belongs to *)
let window_rooms_var = Wobenv.new_var ()
  
(* the current active room *)
let screen_room_var = Wobenv.new_var ()

let default_room_var = Wobenv.new_var ()
  
let all_rooms w =
  let sw = w.w_top.w_parent in
  try Wob.getenv sw screen_rooms_var with _ -> []
  
let get_room w room_name =
  let rec iter list =
    match list with 
      [] -> raise Not_found
    | room :: tail ->
        if room.room_name = room_name then room else iter tail
  in
  iter (all_rooms w)

  
let get_client_rooms w c =
  List.map (fun name -> get_room w name) (
    Marshal.from_string (
      Icccm.getStringProperty display c.c_window window_rooms_atom) 0)
    
let set_client_rooms c w rooms =
  let tw = w.w_top in
  Wob.setenv tw window_rooms_var rooms;  
  let s = Marshal.to_string (List.map (fun room -> room.room_name) rooms) []
  in
  Icccm.setStringProperty display c.c_window window_rooms_atom s 

let set_window_rooms w rooms =
  let c = w.w_oo#client in
  set_client_rooms c w rooms
  
let create_room w room_name =
  try
    get_room w room_name
  with _ ->
      let sw = w.w_top.w_parent in
      let rooms = Wob.sgetenv sw screen_rooms_var [] in
      let new_room = { room_name = room_name } in
      Wob.setenv sw screen_rooms_var (rooms @ [new_room]);
      new_room

let def_room w = 
  let sw = w.w_top.w_parent in
  try Wob.getenv sw default_room_var with _ ->
      let room = create_room sw "Default" in
      Wob.setenv sw default_room_var room;
      room
        
let cur_room w =
  let tw = w.w_top in
  let sw = tw.w_parent in  
  try Wob.getenv sw screen_room_var with _ ->
      let room = def_room sw in
      Wob.setenv sw screen_room_var room;
      room
  
let win_rooms w =
  let tw = w.w_top in
  let sw = tw.w_parent in  
  try Wob.getenv tw window_rooms_var with _ -> 
      let rooms = [def_room w] in
      set_window_rooms tw rooms;
      rooms
      
let add_to_room w room =
  let tw = w.w_top in
  let sw = tw.w_parent in  
  let room = match room with None -> cur_room w | Some r -> r
  in
  let rooms = win_rooms w in
  let rooms = if List.mem room rooms then rooms else room :: rooms in
  set_window_rooms tw rooms;
  if cur_room w = room  then Wob.send tw WobMap

let remove_from_room w room =
  let tw = w.w_top in
  let sw = tw.w_parent in  
  let room = match room with 
      None -> cur_room w | Some r -> r
  in
  let rooms = win_rooms w in
  let rooms = Utils.list_remove rooms room in
  set_window_rooms tw rooms;
  if Wob.getenv sw screen_room_var = room then begin
      Wob.setenv tw is_in_dvroom_var false;
      Wob.send tw (WobUnmap false)
    end

let xbanner_font = define_option ["xbanner_font"] "" font_option "-bitstream-charter-bold-i-normal--125-*-*-*-*-*-iso8859-1"
let xbanner_x = define_option ["xbanner_x"] 
  "<xbanner_x> is the x coordinate of the banner printing the name of
  the current room/workspace." int_option 100
let xbanner_y = define_option ["xbanner_y"] 
  "<xbanner_y> is the y coordinate of the banner printing the name of
  the current room/workspace." int_option 100
let xbanner_delay = define_option ["xbanner_delay"] 
    "<xbanner_delay> is time during which the banner appears on the scrren." 
  float_option 0.5

let delay time =
  try ignore (Unix.select [] [] [] time) with _ -> ()
  
let xbanner sw name =
  try
    let sw = sw.w_top.w_parent in
    let root = sw.w_window in
    let screen = sw.w_screen.s_scr in
    let white = screen.scr_white_pixel in
    let black = screen.scr_black_pixel in
    let sg = sw.w_geometry in
    let font,_ = font_make sw !!xbanner_font in
    let gc = X.createGC display root [
        GCfonction GXxor;
        GCforeground white;
        GCbackground white;
        GCsubwindow_mode IncludeInferiors;
        GCfont font;
      ]
    in
    let x = !!xbanner_x in
    let y = !!xbanner_y in
    try
      Gwml.grabServer ();
      Xlib.drawString display root gc x y name;
      delay !!xbanner_delay;
      Xlib.drawString display root gc x y name;
      X.freeGC display gc;
      Gwml.ungrabServer ()
    with Failure _ ->
        Printf.printf "Failure"; print_newline ();
        Xlib.drawString display root gc x y name;
        delay !!xbanner_delay;
        Xlib.drawString display root gc x y name;
        X.freeGC display gc;
    | e -> 
        Printf.printf "ungrab: %s" (Utils.printexn e); print_newline ();
        Gwml.ungrabServer ()

  with e -> 
      Log.printf "Error in Xbanner: %s\n" (Utils.printexn e)

let room_history_var = Wobenv.new_var ()
      
let set_room w room = 
  let croom = cur_room w in
  if not (croom == room) && List.mem room (all_rooms w) then
    begin
      let sw = w.w_top.w_parent in
      Wob.setenv sw screen_room_var room;
      Icccm.setStringProperty display sw.w_window current_room_atom 
        room.room_name;
      let history = Wob.sgetenv sw room_history_var [] in
      Wob.setenv sw room_history_var (room.room_name :: 
        (Utils.list_remove history room.room_name));
      List.iter (fun tw ->
          try
            let w = 0 in
            let rooms = win_rooms tw in
            let is_iconified = Wob.sgetenv tw is_iconified_var false in
            let is_group_iconified = is_iconified &&
              (Wob.sgetenv tw is_group_iconified_var false) in
            if (List.mem room rooms) || (
                Wob.sgetenv tw sticky_var false) then
              begin
                Wob.setenv tw is_in_dvroom_var true;
                if not is_iconified then
                  Wob.send tw (WobMap)
                else
                if is_group_iconified then () else
                let (icon, group) = Wob.getenv tw icon_var in
                Wob.send icon#wob (WobMap)
              end
            else begin
                Wob.setenv tw is_in_dvroom_var false;
                if is_iconified then
                  if is_group_iconified then () else
                  let (icon, group) = Wob.getenv tw icon_var in
                  Wob.send icon#wob (WobUnmap false)                  
                else
                  Wob.send tw (WobUnmap false)
              end
          with _ -> 
              Log.printf "Exception in set_room %s\n" ""
              (* belongs to all rooms *)
      ) (List.rev !stack);
      exec_hooks sw !change_room_hooks;
      xbanner sw room.room_name;
    end

let backward_room w =
  let sw = w.w_top.w_parent in
  let history = Wob.getenv sw room_history_var in
  match history with
    [] -> ()
  | [cur] -> ()
  | cur :: prev :: tail ->
      Wob.setenv sw room_history_var (tail@[cur]);
      set_room w (get_room w prev)

let rec list_last list = match list with
    [] -> raise Not_found
  | [v] -> v
  | _ :: tail -> list_last tail
  
let forward_room w =
  let sw = w.w_top.w_parent in
  let history = Wob.getenv sw room_history_var in
  set_room w (get_room w (list_last history))
    
let rooms = ref 0

let clear_rooms w =
  let sw = w.w_top.w_parent in
  let def_room = def_room sw in
  Wob.setenv sw screen_room_var def_room;
  Wob.setenv sw screen_rooms_var [def_room];
  List.iter (fun (c,w) ->
      let tw = w.w_top in
      set_window_rooms tw [def_room];
      Wob.setenv tw is_in_dvroom_var true;
      if c.c_wm_state = NormalState then Wob.send tw (WobMap)
  ) (list_clients w)
      
let edit_room_menu sw =
  (try
      let rooms = all_rooms sw in
      List.map (fun room -> 
          room.room_name, [], Function (fun w -> 
              question sw "Enter new name : " room.room_name 
                (fun s -> room.room_name <- s))) rooms
    with _ -> [])

let copy_to_room w room =
  let cur_room = cur_room w in
  if cur_room == room then () else begin
      List.iter (fun (c,w) ->
          let sticky = Wob.sgetenv w.w_top sticky_var false in
          let in_room = Wob.sgetenv w.w_top is_in_dvroom_var false in
          if (not sticky) && in_room then begin
              add_to_room w (Some room);
              remove_from_room w None
            end
      ) (list_clients w);
      set_room w room
    end

let select_room_menu f w =
  ("Current room", [], Function (fun w ->
        let cur_room = cur_room w in
        f w cur_room
    )) ::
  ("New room", [], Function (fun w ->
        incr rooms;
        let room_name = Printf.sprintf "Room %d" !rooms in
        let room = create_room w room_name in
        f w room
    )) ::
  (try
      let rooms = all_rooms w in
      List.map (fun room -> 
          room.room_name, [], Function (fun w -> 
              f w room)) 
      rooms
    with _ -> []) 
  
let set_room_menu sw =
  (try
      let rooms = all_rooms sw in
      let cur_room = cur_room sw in
      List.map (fun room -> 
          let name = if room == cur_room then 
              room.room_name ^ " (CURRENT)" else
              room.room_name in
          name, [], Function (fun w -> set_room w room)) rooms
    with _ -> []) @
    [
    ("Clear rooms", [], Function (fun sw -> clear_rooms sw));
    ("Copy to:", [submenu_item], 
      ActiveMenu (select_room_menu copy_to_room));
    ("Create room", [], Function (fun sw ->
          incr rooms;
          ignore (create_room sw (Printf.sprintf "Room %d" !rooms))));
    ("Edit Room", [submenu_item], ActiveMenu edit_room_menu) ]

      
let add_to_room_menu w =
  ("Current room", [], Function (fun w ->
        add_to_room w None
    )) ::
  ("Create room", [], Function (fun sw ->
        incr rooms;
        let room_name = Printf.sprintf "Room %d" !rooms in
        let room = create_room sw room_name in
        add_to_room w (Some room)
    )) ::
  (try
      let rooms = all_rooms w in
      List.map (fun room -> 
          room.room_name, [], Function (fun w -> 
              add_to_room w (Some room))) 
      rooms
    with _ -> [])
      
let move_to_room_menu w =
  ("New room", [], Function (fun sw ->
        incr rooms;
        let room_name = Printf.sprintf "Room %d" !rooms in
        let room = create_room sw room_name in
        add_to_room w (Some room);
        remove_from_room w None;
    )) ::
  (try
      let rooms = all_rooms w in
      List.map (fun room -> 
          room.room_name, [], Function (fun w -> 
              add_to_room w (Some room);
              let cur_room = cur_room w in
              if not (cur_room == room) then remove_from_room w None;
              )) 
      rooms
    with _ -> [])
        
let remove_from_room_menu w =
  ("Current room", [], Function (fun w ->
        remove_from_room w None
    )) ::
  ("Create room", [], Function (fun sw ->
        incr rooms;
        let room_name = Printf.sprintf "Room %d" !rooms in
        let room = create_room sw room_name in
        remove_from_room w (Some room)
    )) ::
  (try
      let rooms = win_rooms w in
      List.map (fun room -> 
          room.room_name, [], Function (fun w -> 
              remove_from_room w (Some room))) 
      rooms
    with _ -> [])
  
let window_rooms_menu w =
  try
    let rooms = win_rooms w in
    List.map (fun room -> 
        room.room_name, [], Function (fun w -> ())) 
    rooms
  with _ -> []

let desks_var, find_desks, add_desks, desks_list = 
  define_class_option "desks_list" ""
  (smalllist_option string_option) 
  [["Workspace"], ["",""]]

  (*
let startondesk_var = Wobenv.new_var ()
let find_desk w c =
  let desk = find_option c startondesk_var in
(*  Printf.printf "Desk found %s" desk; print_newline (); *)
  get_room w desk
  
let add_startondesk (classe, name) desk =
  let name = 
    if name = "" then  classe else
    if classe = "" then name else
      classe^"."^name
  in
  add_option name startondesk_var desk
    *)

let jump_to_room = define_option ["jump_to_room"] "" bool_option false
  
let client_hook c wob e =
  let w = wob#wob in
  let tw = w.w_top in
  match e with
    WobCreate ->
      let cur_room = cur_room w in
      let rooms = try
          get_client_rooms w c
        with _ ->
            Log.printf "No room property for %s\n" c.c_name;
            (List.map (fun name -> get_room w name)
              (let cur_room = cur_room.room_name in
                let desks = try find_desks c with _ -> [cur_room] in
                let def_room = (def_room w).room_name in
                let desks = 
                  if not (List.mem def_room desks) then desks@[def_room] else
                    desks in
                if !!jump_to_room && not (List.mem cur_room desks) 
                then begin
                    set_room w (get_room w (List.hd desks)); desks
                  end else
                if not (List.mem cur_room desks) && c.c_new_window then
                  cur_room :: desks else desks))@(
              if c.c_transient_for == noWindow then [] else
                try
                  let s = w.w_screen in
                  let (cc,ww) = Wintbl.find s.s_clients c.c_transient_for in
                  get_client_rooms ww cc
                with _ -> []
            )
            in
      set_client_rooms c tw rooms;
      let is_in_dvroom = List.mem cur_room rooms in
      (* If this is a transient window for another window which is
      on this desk, add the current desk ... *)
      let is_in_dvroom = is_in_dvroom ||
        (if c.c_transient_for == noWindow then is_in_dvroom else
          try
            let s = w.w_screen in
            let (c,w) = Wintbl.find s.s_clients c.c_transient_for in
            let w = w.w_top in
            Wob.getenv w is_in_dvroom_var
          with _ -> is_in_dvroom)
      in
      Wob.setenv tw is_in_dvroom_var is_in_dvroom;
      if not is_in_dvroom && c.c_wm_state = NormalState then begin
          c.c_wm_state <- WithdrawnState
        end
        
  | WobDeiconifyRequest _ ->
      let desk = cur_room w in
      let win_rooms = win_rooms w in
      if not (List.memq desk win_rooms) then begin
            Wob.setenv tw is_in_dvroom_var true;
            Wob.send_one tw WobMap
        end
  |  _ -> ()

let goto w =
  if not (Wob.sgetenv w is_in_dvroom_var true) then
    let rooms = win_rooms w in
    let target = 
      match rooms with 
        [] -> def_room w 
      | room1 :: room2 :: _ -> 
          if room1 = def_room w then room2 else room1
      | [r] -> r
    in
    set_room w target
    
class dvroom_manager = (object
      method goto = goto
      method hook = client_hook
      method new_desk w s = ignore (create_room w s)
      method to_desk w s = 
        let room = get_room w s in
          set_room w room
      method rename_desk w s1 s2 =
        let room = get_room w s1 in
          room.room_name <- s2
      method add_to_desk w s =
        let room = get_room w s in
          add_to_room w (Some room)
end : desk_manager)

let init () =
  desk_manager := new dvroom_manager;
  if not !Gwml_args.batch_mode then
    screen_opening_hooks := (fun sw -> 
        let room = def_room sw in ()
    ) :: !screen_opening_hooks
    
