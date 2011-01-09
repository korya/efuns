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

(*
What has still to be done: 
- goto_window: should change the desktop
- goto_desk: if no window is mapped in the current v-screen of the desktop,
  try to switch to a v-screen with mapped windows.
- stdvirtual: should not change v-screens in all desks at once 
- Give names to v-screens (use a dummy window ?).

Requests from users:

Alan:
- Stacking order should be kept during virtual. (DONE)
- Workspace name should be displayed in Icon Manager (instead of DESK).  
  
*)

open Themes
open Gwml_args
open Options
open Xtypes
open Xlib
open Gwml
open Wob
open Stdconfig
open Stdmenus
open Stddeco
open Stdplacement

let _ = 
(* These are here to force them to be loaded ... *)
  Themes.init ();
  Wmaker.init ();
  Afterstep.init ();
  Stdmenus.init ();
  Undo.init ();
  Dvroom.init ()

let broadcast_to_gnome (c,w,ev) =
  let s = c.c_screen in
  match ev with
    AddClient | RemoveClient ->
      Gnome.gnome_SetClientList s
  | _ -> ()

type arrow_meaning = VirtualMove | GotoWin
  
let arrow_option = sum_option ["VirtualMove", VirtualMove; "GotoWin", GotoWin]
let arrow_meaning = define_option ["arrow_meaning"] 
  "<arrow_meaning> specifies whether wm-modifier+arrows is a virtual move
    (<VirtualMove> value) or a move between client windows (<GotoWin> value)" 
  arrow_option VirtualMove

let goto_scr_left sw = 
  let dx = sw.w_screen.s_scr.scr_width in 
  (!virtual_manager)#move sw dx 0 
let goto_scr_right sw = 
  let dx = sw.w_screen.s_scr.scr_width in 
  (!virtual_manager)#move sw (-dx) 0 
let goto_scr_up sw =
  let dy = sw.w_screen.s_scr.scr_height in 
  (!virtual_manager)#move sw 0 dy   
let goto_scr_down sw =
  let dy = sw.w_screen.s_scr.scr_height in 
  (!virtual_manager)#move sw 0 (-dy)

let left_win tw qp best = 
  let g = tw.w_geometry in
  if g.x + 11 < qp.qp_root_x then
    match !best with
      None -> best := Some (tw, g.x, g.y)
    | Some (tw2, x2, y2) ->
        if x2 < g.x then best := Some (tw, g.x, g.y)

let right_win tw qp best = 
  let g = tw.w_geometry in
  if g.x > qp.qp_root_x - 2 then
    match !best with
      None -> best := Some (tw, g.x, g.y)
    | Some (tw2, x2, y2) ->
        if x2 > g.x then best := Some (tw, g.x, g.y)

let up_win tw qp best = 
  let g = tw.w_geometry in
  if g.y + 11 < qp.qp_root_y then
    match !best with
      None -> best := Some (tw, g.x, g.y)
    | Some (tw2, x2, y2) ->
        if y2 < g.y then best := Some (tw, g.x, g.y)

let down_win tw qp best = 
  let g = tw.w_geometry in
  if g.y > qp.qp_root_y - 2 then
    match !best with
      None -> best := Some (tw, g.x, g.y)
    | Some (tw2, x2, y2) ->
        if y2 > g.y then best := Some (tw, g.x, g.y)

        
let iconify_window w = Wob.send w.w_top (WobIconifyRequest true)
let deiconify_window w = Wob.send w.w_top (WobDeiconifyRequest true)
let delete_window w = Wob.send w.w_top WobDeleteWindow
let destroy_window w = Wob.send w.w_top WobDestroy
let lower_window w = Wob.send w.w_top WobLowerWindow  
let raise_window w = Wob.send  w.w_top WobRaiseWindow  

let apply_client f c sw =
  let (c, w) = Wintbl.find sw.w_screen.s_clients c.c_window in
  f w  
  
let apply_top f c sw =
  let (c, w) = Wintbl.find sw.w_screen.s_clients c.c_window in
  f w.w_top  

let apply_group group f sw =
  let group = Group.find_group group in
  List.iter (fun (c,w) ->
      let groups = Group.find_groups w in
      if List.memq group groups then f w
  ) (list_clients sw)

let move_group group sw = ()
  
let group_menu group sw =
  [
    "Iconify Group", [], Function (apply_group group iconify_window);
    "Deiconify Group", [], Function (apply_group  group deiconify_window);
    "Move Group", [], Function (move_group group);
    "Clear Group", [], Function (Group.clear_group group);
    "Delete Group", [], Function (apply_group group delete_window);
    "Destroy Group", [], Function (apply_group group destroy_window);
  ]

let left_arrow sw =
  match !!arrow_meaning with
    VirtualMove -> goto_scr_left sw
  | GotoWin -> goto_win left_win sw
      
let right_arrow sw =        
  match !!arrow_meaning with
    VirtualMove -> goto_scr_right sw
  | GotoWin -> goto_win right_win sw

let up_arrow sw =
  match !!arrow_meaning with
    VirtualMove -> goto_scr_up sw
  | GotoWin -> goto_win up_win sw

let down_arrow sw =
  match !!arrow_meaning with
    VirtualMove -> goto_scr_down sw
  | GotoWin -> goto_win down_win sw
      
let left_inv_arrow sw =
  match !!arrow_meaning with
    GotoWin -> goto_scr_left sw
  | VirtualMove -> goto_win left_win sw
      
let right_inv_arrow sw =
  match !!arrow_meaning with
    GotoWin -> goto_scr_right sw
  | VirtualMove -> goto_win right_win sw
      
let up_inv_arrow sw = 
  match !!arrow_meaning with
    GotoWin -> goto_scr_up sw
  | VirtualMove -> goto_win up_win sw
      
let down_inv_arrow sw =
  match !!arrow_meaning with
    GotoWin -> goto_scr_down sw
  | VirtualMove -> goto_win down_win sw

let inv_arrow sw =
  arrow_meaning =:= (match !!arrow_meaning with
      VirtualMove -> GotoWin
    | GotoWin -> VirtualMove);
  message sw (Printf.sprintf "Arrow meaning: %s"
      (match !!arrow_meaning with
        VirtualMove -> "VirtualMove"
      | GotoWin -> "GotoWin"))

let restart_gwml _ = 
  restart_cmd := (if !gwml_talk then 
      [| !restart_cmd.(0); "-talk" |] else [| !restart_cmd.(0) |]);
  Wob.restart ()


let shade_window w = Wob.send w.w_top (WobMessage "minimize")
  
  
  
let win_menu = winmenu

let resize_window w = !resize_mode w true

let raise_on_move = define_option ["raise_on_move"] 
  "<raise_on_move> is true if windows should be raised before being moved,
  and false otherwise." bool_option true
  
let move_window w = 
  if !!raise_on_move then Wob.send w.w_top WobRaiseWindow;
  User.move w.w_top true
  
let toggle_sticky = toggle_virtual_move
let kill_window w =         
  X.killClient display (window_to_id w.w_oo#client.c_window)
let info_window w = message w (info w.w_oo#client w)
  

  
  

let twm_config _ =
  let rc = 
    try
      Twm.load (Filename.concat Utils.homedir ".twmrc")
    with
      _ -> Twm.load "/etc/X11/twm/system.twmrc"
  in
  Twm.use rc;
  Twm.install_bindings (); 
  add_decoration Twm.twm_window ("","")  

let tvtwm_config _ =  
  let rc = 
    try
      Twm.load (Filename.concat Utils.homedir ".tvtwmrc") 
    with
      _ -> Twm.load "/etc/X11/tvtwm/system.tvtwmrc"          
  in
  Twm.twm_TitleForeground =:= "black";
  Twm.twm_TitleBackground =:= "white";      
  Twm.use rc;
  Twm.install_bindings (); 
  add_decoration Twm.twm_window ("","")

let fvwm_config _ =
  let proto = Modules.Fvwm1 in
  let rc = 
    try
      Fvwm.load proto (Filename.concat Utils.homedir ".fvwmrc") 
    with
      _ -> Fvwm.load proto "/etc/X11/fvwm/system.fvwmrc"
  in
  Fvwm.fvwm_version := 1;
  Fvwm.use proto [Fvwm.TakeFeel; Fvwm.TakeKeys; Fvwm.TakeTheme] rc;
  Fvwm.install_bindings (); 
  add_decoration Fvwm.fvwm_window ("","")

let fvwm2_config _ =
  let proto = Modules.Fvwm2 in
  let rc = try
      Fvwm.load proto (Filename.concat Utils.homedir ".fvwm2rc") 
    with
      _ -> Fvwm.load proto "/etc/X11/fvwm2/system.fvwm2rc"
  in
  Fvwm.use proto [Fvwm.TakeFeel; Fvwm.TakeKeys; Fvwm.TakeTheme] rc;
  Fvwm.install_bindings (); 
  add_decoration Fvwm.fvwm_window ("","")

let foreign_mode name option =
  name, [], Function (fun w -> 
      if !gwml_talk then restart_cmd := [| "gwml"; option; "-talk"|]
      else restart_cmd := [| "gwml"; option|];
      restart ())
  
let _ = 
  define_action "exit" (Function Wob.exit_gwml)
  
  
let exit_menu = 
  [
    "Exit", [], Function Wob.exit_gwml;
    "Restart", [], Menu (fun _ ->
        [
          "Gwml", [],Function (fun w -> restart ());
          foreign_mode "Twm" "twm";
          foreign_mode "Tvtwm" "tvtwm";
          foreign_mode "Fvwm" "fvwm";
          foreign_mode "Fvwm2" "fvwm2"
        ]);
  ]

let _ =
  if !!decorate = [] then
    decorate =:= [
      ("standard", ["",""]);
      ("no_deco",   [
          "XBuffy","";          (* The XBuffy new mail watcher *)
          "CPUStateMonitor",""; (* xcpustate *)
          "Clock","";           (* for Oclock *)
          "XConsole","";        (* xconsole *)
          "XApm","xapm";        (* Power-management tool *)
          "XLoad","";           (* xload *)
          "GwML", "";           (* GwML client windows (pager) *)
          "gmc", "desktop_icon";(* Gnome icons *)
          "Panel","panel";      (* Gnome panel *)
          "XClock", "";         (* xclock *)
        ])]
        
let no_deco_list = define_option [ "no_deco_list" ] ""
    (list_option string2_option) []
let _ = option_hook no_deco_list 
    (fun _ -> 
      List.iter (add_decoration no_deco) !!no_deco_list)
  
let ontop_list = define_option [ "ontop_list" ] 
  "<ontop_list> is the list"
    (list_option string2_option)
  []
  
let _ =
  List.iter (fun pair -> add_is_on_top pair) !!ontop_list

let onbottom_list = define_option [ "onbottom_list" ] ""
    (list_option string2_option)
  []
  
let _ =
  List.iter (fun pair -> add_is_on_bottom pair) !!onbottom_list

let xy_placement_list = define_option ["xy_placement_list"] ""
    (list_option (tuple2_option (string2_option, 
        tuple2_option (int_option, int_option))))
  []
  
let _ =
  List.iter (fun ( pair, (x,y) ) ->
      add_placement Stdplacement.xy_placement pair;
      Stdplacement.add_xy_position pair (x,y)) !!xy_placement_list
  
let near_mouse_list = define_option [ "near_mouse_list" ] ""
    (list_option string2_option)
  [
    "Netscape","findDialog_popup";
    "Netscape","openURLDialog_popup";
    "Netscape","error_popup";
    "Netscape","question_popup";
    "Netscape","fileSelector_popup";
    "Netscape","printSetup_popup";
    "Emacs","dialog";
    "Ghostview","popup";
    "TransientShell","selFile";
    "client.name","Tgif_-_Please_select_a_file_to_OPEN____";
  ] 


let _ = option_hook near_mouse_list 
    (fun _ -> List.iter (add_placement Stdplacement.near_mouse) 
      !!near_mouse_list)  
  
let iconified_list = define_option ["iconified_list"] "" 
    (list_option string2_option) ["Netscape","Download"]

let _ = option_hook iconified_list 
    (fun _ -> List.iter (add_placement Stdplacement.icon_position)
      !!iconified_list)

let use_icon_manager = define_option ["use_icon_manager"] "" bool_option true

let icon_manager_var = Wobenv.new_var ()
  
let icon_manager_pred name c =
  try (find_option c icon_manager_var) = name with _ -> false
  
let add_icon_manager manager c =
  let (classe,name) = c in
  let name = 
    if name = "" then  classe else
    if classe = "" then name else
      classe^"."^name
  in
  add_option name icon_manager_var manager

let icon_managers = define_option ["icon_managers"] "" 
    (list_option (tuple3_option
        (string_option, tuple2_option (int_option, int_option),
        list_option string2_option)))
  [ "Windows", (-1,0), ["", "" ];
    "Xterms", (-1,200), ["XTerm",""; "NXterm", ""];
    "Editors", (-1,400), ["Efuns",""; "Emacs", ""] ]
  
let _ = List.iter (fun (name, _, tail) ->
      List.iter (fun c -> add_icon_manager name c) tail)
  !!icon_managers


let _ =
  let old_decorate_client = !decorate_client in
  decorate_client := (fun sw cwob ->
      old_decorate_client sw cwob;
      let c = cwob#client in
      let cw = cwob#wob in
      Group.add_to_group (fst c.c_class) cw
  )

let initial_desks = define_option ["initial_desks"] 
  "The names of the desks which are created at startup."
    (list_option string_option) 
  ["Workspace"; "Mail"; "Netscape"; "JoCaml"; "GwML"; "Admin"; "Games"]

let init_desk = define_option ["init_desk"] 
  "The name of the desk on which the window-manager should start." 
  string_option "Workspace"
    

  

  (*************
  An option menu  to modify confine_move, pan_on_click, etc from a menu
  *************)
  
let option format ref =
  let value = !!ref in
  Printf.sprintf format (
    if value then "YES" else "NO"), [],
  Function (fun _ -> ref =:= not value)
  
let option_menu _ = 
  [ option "Confine move:   %s" confine_move;
    option "Pan on click:   %s" pan_on_click;
    option "Opaque move:    %s" opaque_move;
    option "Debug command:  %s" debug_command;
    option "Auto raise:     %s" auto_raise;
    option "Auto colormap:  %s" auto_colormap;
    option "Grab server:    %s" grab_server;
    option "Group iconify:  %s" group_iconification;
    option "Edit title:     %s" editable_title;
    option "Deiconify here: %s" Stdicon.deiconify_in_vscreen;
    option "Use imlib:      %s" use_imlib;
  ]

let preferred_hosts = define_option ["preferred_hosts"] "" 
    (list_option string_option) []

let fast_hosts_menu _ =
  List.map (fun host ->
      host, [],Function (rlogin host)
  ) !!preferred_hosts


let hosts = ref []  
let hosts_menu _ =
  List.map (fun (projet, hosts) ->
      let menu = List.map (fun machine ->
            machine, [],Function (rlogin machine)
        ) hosts in
      projet, [submenu_item], Menu (fun () -> menu)) !hosts

  
  (************
  Add some commands to the window popup menu
  ************)
  
let move_left tw =
  let sw = tw.w_top.w_parent in
  let dx = sw.w_screen.s_scr.scr_width in 
  let g = tw.w_geometry in
  g.x <- g.x - dx;
  Wob.send tw WobMap;
  (!virtual_manager)#update sw

let move_right tw =
  let sw = tw.w_top.w_parent in
  let dx = sw.w_screen.s_scr.scr_width in 
  let g = tw.w_geometry in
  g.x <- g.x + dx;
  Wob.send tw WobMap;
  (!virtual_manager)#update sw

let move_up tw =
  let sw = tw.w_top.w_parent in
  let dy = sw.w_screen.s_scr.scr_height in 
  let g = tw.w_geometry in
  g.y <- g.y - dy;
  Wob.send tw WobMap;
  (!virtual_manager)#update sw

let move_down tw =
  let sw = tw.w_top.w_parent in
  let dy = sw.w_screen.s_scr.scr_height in 
  let g = tw.w_geometry in
  g.y <- g.y + dy;
  Wob.send tw WobMap;
  (!virtual_manager)#update sw

let move_top tw = User.move tw true
let resize_top tw = User.resize tw true
  
let add_to_group c group sw =
  Group.add_to_group group (Group.find_wob sw c)

let find_wob c =
  let s = c.c_screen in
  let (c,w) = Wintbl.find s.s_clients c.c_window in w

let geometry_menu w = 
  let w = w.w_top in
  let c = w.w_oo#client in
  let g = w.w_geometry in
  [Printf.sprintf "WINDOW %s %dx%d+%d+%d" 
      c.c_name g.width g.height g.x g.y, [], 
    Function (fun _ -> ());
    "Groups", [submenu_item], ActiveMenu (fun _ ->
        List.map (fun name ->
            name, [], Function (fun _ -> ())) 
        (Group.find_groups w))]

let virtual_move_menu _ =
  [ "Left screen", [], Function move_left;
    "Right screen", [], Function move_right;
    "Up screen", [], Function move_up;
    "Down screen", [], Function move_down;
  ]

let win_config_menu w = 
  let c = w.w_oo#client in
  let name = Printf.sprintf "CLASS: %s.%s" (fst c.c_class) (snd c.c_class)
  in
  [
    name, [], NoAction;
    "Decoration", [submenu_item], NamedAction "win_deco_menu";
    "Redecorate", [], NamedAction "redecorate";
    "Save Options", [], NamedAction "save_options";
  ]
  
let _ = 
  define_action "win_config_menu" (ActiveMenu win_config_menu);
  define_action "toggle_reduce" (Function (fun w ->
        let w = w.w_top in
        let c = w.w_oo#client in
        let (dir, size) = get_reduced_window c w in
        Stddeco.set_temp_reduced_window c w (if size = 0 then
            (dir, 20) else (dir, 0))
        ))  
  
let _  =
  if !!smart_install || !!window_popup = [] then
    window_popup =:= 
      ("Geometry", [submenu_item], NamedAction "geometry_menu") ::
    ("Iconify", [], NamedAction "iconify_window") ::
    ("Lower", [], NamedAction "lower_window") ::
    ("Raise", [], NamedAction "raise_window") ::
    ("Maximize X", [], NamedAction "maximize_x") ::
    ("Maximize Y", [], NamedAction "maximize_y") ::
    ("Move", [], NamedAction "move_window") ::
    ("Resize", [], NamedAction "resize_window") ::
    ("Togle Sticky", [], NamedAction "toggle_sticky") ::
    ("Move To", [submenu_item], NamedAction "virtual_move_menu")::
    ("Belongs to room", [submenu_item],
      NamedAction "window_rooms_menu") ::
    ("Move to room", [submenu_item],
      NamedAction "move_to_room_menu") ::
    ("Add to room", [submenu_item],
      NamedAction "add_to_room_menu") ::
    ("Remove from room", [submenu_item],
      NamedAction "remove_from_room_menu") ::
    ("Toggle reduce", [], NamedAction "toggle_reduce") ::
    ("Delete", [], NamedAction "delete_window") ::
    ("Destroy", [], NamedAction "destroy_window") ::
    ("Configure", [], NamedAction "win_config_menu") ::
    !!window_popup

    
  (******** Load the Fvwm config file generated (au RedHat Linux) by 
  wmconfig --output fvwm2 > ~/.fvwm2_menus
  *********)

let popup3 = ref []
  

let wmaker_menu = 
  try Wmaker.wmaker_menu !!Wmaker.wmaker_file with _ -> []

      
let use_wmaker_menu () = 
  try popup3 := Wmaker.wmaker_menu !!Wmaker.wmaker_file with _ -> ()

let fvwm_menu = ref []
      
let use_fvwm_menu () = 
  if !fvwm_menu = [] then 
    try
      Fvwm.reset ();
      let proto = Modules.Fvwm2 in
      let rc = Fvwm.load proto (
          Filename.concat Utils.homedir !!Fvwm.fvwm_file) in
      Fvwm.use proto [] rc;
      fvwm_menu := Fvwm.fvwm_menu !!Fvwm.fvwm_main_menu ();
      popup3 := !fvwm_menu;
    with _ -> ()
        
let foreign_menu = define_option ["foreign_menu"] ""
    (sum_option [
      "Fvwm2", use_fvwm_menu; "Wmaker", use_wmaker_menu]) use_fvwm_menu
  
let _ = option_hook foreign_menu (fun _ -> !!foreign_menu ())
  

  (**************
  Add some special decorations ... 
  ***************)  

let backgrounds sw =
  let asthemes_path = !!Themes.asthemes_path in
  let asimages = 
    List.map (fun dir ->
        try
          let files = Utils.list_dir_normal dir in
        List.map (fun file -> 
              let dir = Filename.concat dir file in
              Filename.concat dir ("background." ^ file)
        ) files
            with _ -> []
    ) asthemes_path in
  let wmaker_path = !!Wmaker.wmaker_path in
  let wmimages = List.map (fun dir ->
        try
          let dir = Filename.concat dir "Backgrounds" in
          List.map (fun file ->
              Filename.concat dir file
          ) (if Sys.file_exists dir then Utils.list_dir_normal dir else [])
        with _ -> []
    ) wmaker_path in
  List.map (fun file ->
      Filename.basename file, [], Function (fun sw ->
          root_image =:= (ScaleImage file))
  ) (List.flatten (wmimages @ asimages))

let autoload action mod_name w =
  let action = 
    match get_action action with
      NoAction ->
        Dyneval.load mod_name;
        get_action action
    | action -> action
  in
  execute_action action w

let autoload_configurator w =  
  ungrab_exec (fun _ ->
      autoload (NamedAction "configurator") "Configurator" w)
  
let config_menu = [
    "Options", [submenu_item], NamedAction "option_menu";
    "GwML Themes", [submenu_item], NamedAction "gwml_themes";
    "WindowMaker Themes", [submenu_item], NamedAction "wmaker_themes";
    "AfterStep Themes", [submenu_item], NamedAction "afterstep_themes";
    "Backgrounds", [submenu_item], NamedAction "backgrounds";
    "WindowMaker Menu", [], Function (fun _ -> 
        foreign_menu =:= use_wmaker_menu);
    "Fvwm2 Menu", [], Function (fun _ -> foreign_menu =:= use_fvwm_menu);
    "Placements", [submenu_item], NamedAction "placements";
    "Animations", [submenu_item], NamedAction "animations";
    "Reload Options", [], NamedAction "reload_options";
    "Save Options", [], NamedAction "save_options";
    "Save Options+Help", [], NamedAction "save_options_with_help";
    "Configurator", [], NamedAction "autoload_configurator";
  ]

let control_icon_manager w = !icon_manager#message w "key-control"
  
let exec args =
  let cmd = List.fold_right (fun arg cmd -> Printf.sprintf "%s %s" arg cmd)
    args "" in
  Function (commandw cmd)

let goto_desk args =
  let name = match args with name :: _ -> name | _ -> raise Not_found in
  Function (fun w -> Dvroom.set_room w (Dvroom.get_room w name))
  
let _ =
  define_xaction "exec" exec;
  define_xaction "goto_desk" goto_desk;
  define_xaction "apply_icon" Stdicon.apply_icon;
  define_action "popup3_menu" (Menu (fun _ -> !popup3));
  define_action "wmaker_menu" (Menu (fun _ -> wmaker_menu));
  define_action "undo" (Function (fun sw -> (!undo_manager)#undo));
  define_action "window_menu" (ActiveMenu
    (fun w -> !!window_popup));
  define_action "group_menu" (ActiveMenu (fun w -> 
        group_menu (Group.main_group w) w));
  define_action "geometry_menu" (ActiveMenu geometry_menu);
  define_action "virtual_move_menu" (Menu virtual_move_menu);
  define_action "window_rooms_menu" (ActiveMenu Dvroom.window_rooms_menu);
  define_action "move_to_room_menu" (ActiveMenu Dvroom.move_to_room_menu);
  define_action "add_to_room_menu" (ActiveMenu Dvroom.add_to_room_menu);
  define_action "remove_from_room_menu" (ActiveMenu Dvroom.remove_from_room_menu);
  define_action "netscape" (Function (commandw "netscape"));
  define_action "exit_menu" (Menu (fun _ -> exit_menu));
  define_action "restart" (Function Wob.restart);
  define_action "restart_gwml" (Function restart);
  define_action "mal_all" (Function map_all);
  define_action "xterm" (Function (commandw "xterm"));
  define_action "efuns" (Function (commandw "efuns"));
  define_action "print_all" (Function print_all);
  define_action "deiconify_last" (Function deiconify_last);
  define_action "left_arrow" (Function left_arrow);
  define_action "right_arrow" (Function right_arrow);
  define_action "up_arrow" (Function up_arrow);
  define_action "down_arrow" (Function down_arrow);
  define_action "left_inv_arrow" (Function left_inv_arrow);
  define_action "right_inv_arrow" (Function right_inv_arrow);
  define_action "up_inv_arrow" (Function up_inv_arrow);
  define_action "down_inv_arrow" (Function down_inv_arrow);
  define_action "popup1_menu" (Menu (fun _ -> !popup1));
  define_action "inv_arrow" (Function inv_arrow);
  define_action "win_menu" (ActiveMenu win_menu);
  define_action "move_window" (Function move_window);
  define_action "iconify_window" (Function iconify_window);
  define_action "deiconify_window" (Function deiconify_window);
  define_action "resize_window" (Function resize_window);
  define_action "lower_window" (Function lower_window);
  define_action "raise_window" (Function raise_window);
  define_action "toggle_sticky" (Function toggle_sticky);
  define_action "maximize_x" (Function maximize_x);
  define_action "maximize_y" (Function maximize_y);
  define_action "kill_window" (Function kill_window);
  define_action "info_window" (Function info_window);
  define_action "option_menu" (ActiveMenu option_menu);
  define_action "wmaker_themes" (ActiveMenu Wmaker.menu);
  define_action "afterstep_themes" (ActiveMenu Afterstep.menu);
  define_action "backgrounds" (ActiveMenu backgrounds);
  define_action "placements" (Menu (Stdplacement.placement_menu));
  define_action "animations" (Menu (Animate.animation_menu));
  define_action "reload_options" (Function (fun _ -> Options.load ()));
  define_action "save_options" (Function (fun _ -> Options.save ()));
  define_action "save_options_with_help" (
    Function (fun _ -> Options.save_with_help ()));
  define_action "fast_hosts_menu" (Menu fast_hosts_menu);
  define_action "hosts_menu" (Menu hosts_menu);
  define_action "config_menu" (Menu (fun _ -> config_menu));
  define_action "set_room_menu" (ActiveMenu Dvroom.set_room_menu);
  define_action "control_icon_manager" (Function control_icon_manager);
  define_action "backward_room" (Function Dvroom.backward_room);
  define_action "forward_room" (Function Dvroom.forward_room);
  define_action "shade_window" (Function shade_window);
  define_action "delete_window" (Function delete_window);
  define_action "destroy_window" (Function destroy_window);
  define_action "autoload_configurator" (Function autoload_configurator);    
  define_action "twm" (Function twm_config);
  define_action "tvtwm" (Function tvtwm_config);
  define_action "fvwm" (Function fvwm_config);
  define_action "fvwm2" (Function fvwm2_config);
  define_action "gc_compact" (Function (fun _ -> Gc.compact ()));
  define_action "redecorate" (Function redecorate);
  ()
  
let _ =  
  !!foreign_menu ()
  
let default_popup = define_option ["default_popup"] "" menu_option []

let help w =
  commandw (Printf.sprintf "efuns %s/FAQ.gwml &" Version.gwml_lib) w
  
let _ =
  if !!smart_install || !!default_popup = [] then
    default_popup =:=
      [
      "ROOT MENU", [], NoAction;
      "New Xterm",[], NamedAction "xterm";
      "Netscape", [], NamedAction  "netscape";
      "Rlogin 1", [submenu_item], NamedAction "fast_hosts_menu";
      "Rlogin 2", [submenu_item], NamedAction "hosts_menu";
      "Configure", [submenu_item], NamedAction "config_menu";
      "Rooms", [submenu_item], NamedAction "set_room_menu";
      "Help", [], NamedAction "help";
      "Exit", [ submenu_item], NamedAction "exit_menu";
    ] 
    
  
let default_config () =
  if !!smart_install || !!window_actions = [] then 
    add_actions window_actions [
      Button (1, !!wm_modifiers), NamedAction "move_window", true;
      Button (2, !!wm_modifiers), NamedAction "iconify_window", true;
      Button (3, !!wm_modifiers), NamedAction "resize_window", true;
      Key (XK.xk_i, !!wm_modifiers), NamedAction "iconify_window", true;
      Key (XK.xk_l, !!wm_modifiers), NamedAction "lower_window", true;
      Key (XK.xk_t, !!wm_modifiers), NamedAction "toggle_sticky", true;
      Key (XK.xk_w, !!wm_modifiers), NamedAction "maximize_x", true;
      Key (XK.xk_space, !!wm_modifiers), NamedAction "maximize_y", true;
      Key (XK.xk_BackSpace, !!wm_modifiers), NamedAction "kill_window", true;
      Key (XK.xk_Delete, !!wm_modifiers), NamedAction "destroy_window", true;
      Key (XK.xk_j, !!wm_modifiers), NamedAction "info_window", true;
      
      Key (XK.xk_F5, !!wm_modifiers),NamedAction "window_menu", true;
      Key (XK.xk_5, !!wm_modifiers), NamedAction "window_menu", true;
      Key (XK.xk_g, !!wm_modifiers), NamedAction "group_menu", true;
    ];
  if !!smart_install || !!screen_actions = [] then
      add_actions screen_actions [
        Button (1,0), NamedAction "popup1_menu", false;
        Button (3,0), NamedAction "popup3_menu", false;
        Key(XK.xk_F1, !!wm_modifiers), NamedAction "popup1_menu", true;
        Key(XK.xk_F2, !!wm_modifiers), NamedAction "win_menu", true;
        Key(XK.xk_F3, !!wm_modifiers), NamedAction "popup3_menu", true;
        Key(XK.xk_1, !!wm_modifiers), NamedAction "popup1_menu", true;
        Key(XK.xk_2, !!wm_modifiers), NamedAction "win_menu", true;
        Key(XK.xk_3, !!wm_modifiers), NamedAction "popup3_menu", true;
        Key(XK.xk_4, !!wm_modifiers), NamedAction "wmaker_menu", true;
        Key (XK.xk_minus, !!wm_modifiers), NamedAction "undo", true;
        Key (XK.xk_Tab, !!wm_modifiers), NamedAction "control_icon_manager", true;
        Key (XK.xk_r, !!wm_modifiers), (NamedAction "restart"), true;
        Key (XK.xk_m, !!wm_modifiers), NamedAction "map_all", true;
        Key (XK.xk_x, !!wm_modifiers), NamedAction "xterm", true;
        Key (XK.xk_e, !!wm_modifiers), NamedAction "efuns", true;
        Key (XK.xk_p, !!wm_modifiers), NamedAction "print_all", true;
        Key (XK.xk_z, !!wm_modifiers), NamedAction "deiconify_last", true;
        Key (XK.xk_Left, !!wm_modifiers), NamedAction "left_arrow", true;
        Key (XK.xk_Right, !!wm_modifiers), NamedAction "right_arrow", true;
        Key (XK.xk_Up, !!wm_modifiers), NamedAction "up_arrow", true;
        Key (XK.xk_Down, !!wm_modifiers), NamedAction "down_arrow", true;
        Key (XK.xk_Left, !!wm_modifiers lor shiftMask), 
        NamedAction "left_inv_arrow", true;
        Key (XK.xk_Right, !!wm_modifiers lor shiftMask), 
        NamedAction "right_inv_arrow", true;
        Key (XK.xk_Up, !!wm_modifiers lor shiftMask), 
        NamedAction "up_inv_arrow", true;
        Key (XK.xk_Down, !!wm_modifiers lor shiftMask), 
        NamedAction "down_inv_arrow", true;        
        Key (XK.xk_F1, !!wm_modifiers), NamedAction "popup1_menu", true;
        Key (XK.xk_1, !!wm_modifiers), NamedAction "popup1_menu", true;
        Key (XK.xk_n, !!wm_modifiers), NamedAction "inv_arrow", true;
        Button (1,0), NamedAction "popup1", false;
        Button (2,0), NamedAction "win_menu", false;
        Key (XK.xk_r, !!wm_modifiers), NamedAction "restart_gwml", true;
      ];
  add_decoration simple_window ("","") ; (* standard window decoration *)
(*   List.iter (add_decoration no_deco) !!no_deco_list;  *)
  popup1 := !!default_popup;
  List.iter (fun (name, list) ->
      List.iter (add_decoration (generic_window name)) list) !!decorate

  
let init () =
  
  virtual_manager := Stdvirtual.manager;
  icon_manager := AppMgr.manager;
  
  add_placement !!default_placement ("","");
  option_hook default_placement 
    (fun _ -> add_placement !!default_placement ("",""));
  let netscape_cascade = {
      cascade_x = 10;
      cascade_y = 10;
      cascade_dx = 5;
      cascade_dy = 25;
    } in
  List.iter (add_placement (icon_placement netscape_cascade)) 
  !!iconified_list;
  List.iter (add_placement near_mouse) !!near_mouse_list;
  
  broadcast_targets := broadcast_to_gnome :: !broadcast_targets;
  screen_opening_hooks := (fun sw -> 
      if not !batch_mode then begin
          sw.w_oo#set_actions (convert_bindings !!screen_actions);
          (!virtual_manager)#start sw;
          Fvwm.module_listen sw;
          Gnome.setHints sw;
          Gnome.gnome_SetClientList sw.w_screen;
          (!virtual_manager)#add_hook sw (fun sw ->
              Gnome.gnome_updateArea sw);
          List.iter (fun name ->
              (!desk_manager)#new_desk sw name) !!initial_desks;
          (!desk_manager)#to_desk sw !!init_desk;
          if !!use_icon_manager then
            let sg = sw.w_geometry in
            List.iter (fun (name, (x,y), _) ->
                try 
                  let x = if x < 0 then sg.width else x in
                  (!icon_manager)#create sw x y name (-1) 
                  (icon_manager_pred name)
                with _ -> ()) !!icon_managers;
        end;
      List.iter (fun name -> execute_action (NamedAction name) sw)
      !Gwml_args.args;
      
  ) :: !screen_opening_hooks; 
  
  default_config ();

  if !!Sound.use_sound then begin
      Sound.init_sound "";
      Sound.file_play !!startup_sound;
    end;
  
  config_loaded := true
  