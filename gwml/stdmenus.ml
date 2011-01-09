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
open Wob
open Stdconfig
  
let select_msg = WobMessage "menu:selected"

let menu_min_width = define_option ["menu_min_width"] "" int_option 70
let menu_min_height = define_option ["menu_min_height"] "" int_option 13 
  
let menus = ref 0
  
type stdmenu = {
    name : string;
    mutable target : wob;
    mutable button : int;
    mutable topmenu : stdmenu option;
    mutable top: wob_desc;
    mutable labels : menu_label array;
  }

and menu_label = {
    bar: wob_desc;
    label : Label.label;
    menu : stdmenu;
    mutable selected : bool;
    mutable submenu : stdmenu option;
    cmd : action;
  }

let forward_make_menu = ref (fun _ _ _ -> assert false)
  
let rec unselect menu =
  Array.iter (fun menu_label ->
      if menu_label.selected then begin
          menu_label.label#unhilite;
          menu_label.selected <- false;
          match menu_label.submenu with
            None -> () (* Should not append *)
          | Some submenu ->
              unselect submenu;
              Wob.send submenu.top#wob (WobUnmap false);
        end
  ) menu.labels
  
let preselect menu_label =
  if not menu_label.selected then
    let w = menu_label.bar#wob in
    let menu = menu_label.menu in
    unselect menu;
    let label = menu_label.label in
    label#hilite;
    menu_label.selected <- true;
    Wob.send w.w_top.w_parent (WobInstallColormap false);
    try
      let submenu =
        match get_action menu_label.cmd, menu_label.submenu with
          Menu m, Some submenu -> 
            submenu
            (* Unless the submenu can change, don't change the submenu *)
        | ActiveMenu m, Some submenu -> 
            Wob.send submenu.top#wob WobDestroy;
            let submenu = 
              !forward_make_menu menu.target (m menu.target) menu.button in
            submenu.topmenu <- Some menu;
            menu_label.submenu <- Some submenu;
            submenu
        | ActiveMenu m, None ->
            let submenu = 
              !forward_make_menu menu.target (m menu.target) menu.button in
            submenu.topmenu <- Some menu;
            menu_label.submenu <- Some submenu;
            submenu            
        | Menu m, None ->
            let submenu = 
              !forward_make_menu menu.target (m ()) menu.button in
            submenu.topmenu <- Some menu;
            menu_label.submenu <- Some submenu;
            submenu
        | _ -> raise Exit
      in
      let bg = w.w_geometry in
      let x,y = root_position w in
      let tw = submenu.top#wob in
      let g = tw.w_geometry in
      let sw = w.w_top.w_parent in
      let sg = sw.w_geometry in
      g.x <- x+ bg.width;
      g.x <- min g.x (sg.width - g.width);      
      g.y <- min y (sg.height - g.height);
      Wob.send tw WobMap;
    with _ -> ()

let preselect_open menu_label =
  preselect menu_label;
  match menu_label.submenu with
    None -> ()
  | Some submenu ->
      if Array.length submenu.labels > 0 then
        preselect submenu.labels.(0)
        
let rec popdown_menu menu =
  unselect menu;
  Wob.send menu.top#wob (WobUnmap false);
  match menu.topmenu with
    None -> 
      X.ungrabKeyboard display currentTime;
      prevent_autoraise := false
  | Some menu -> popdown_menu menu
        
let select menu_label =
  match get_action menu_label.cmd  with 
    NoAction -> ()
  | Function f -> 
      popdown_menu menu_label.menu;
      f menu_label.menu.target 
  | Menu m -> 
      preselect menu_label
  | ActiveMenu m -> 
      preselect menu_label
  | NamedAction _ -> assert false
  | NamedActionX _ -> assert false
  
let select_open menu_label =
  match get_action menu_label.cmd with 
    NoAction -> ()
  | NamedAction _ -> assert false
  | NamedActionX _ -> assert false
  | Function f -> 
      popdown_menu menu_label.menu;
      f menu_label.menu.target 
  | Menu m -> 
      preselect menu_label;
      begin
        match menu_label.submenu with
          None -> ()
        | Some submenu ->
            if Array.length submenu.labels > 0 then
              preselect submenu.labels.(0)
      end
  | ActiveMenu m -> 
      preselect menu_label;
      begin
        match menu_label.submenu with
          None -> ()
        | Some submenu ->
            if Array.length submenu.labels > 0 then
              preselect submenu.labels.(0) 
      end
      

let rec raise_menu menu =
  Xlib.raiseWindow display menu.top#wob.w_window;
  match menu.topmenu with
    None -> ()
  | Some menu -> raise_menu menu
      
let menu_hook menu_label e = 
  let menu = menu_label.menu in
  match e with
    WobEnter ->
      if menu.button <> 0 then preselect menu_label
        
  | WobButtonPress e ->
      if e.Xbutton.detail = menu.button || 
        (menu.button = 0 && e.Xbutton.detail = 1) 
      then 
        select menu_label
      else
        popdown_menu menu_label.menu
  
  | WobButtonRelease e ->
      if e.Xbutton.detail = menu.button then begin
          match get_action menu_label.cmd with 
            NoAction -> ()
          | NamedAction _ -> ()
          | NamedActionX _ -> ()
          | Function f -> 
              popdown_menu menu;
              f menu.target
          | Menu m ->
              popdown_menu menu
          | ActiveMenu m ->
              popdown_menu menu
        end
  
  | WobDestroy -> 
      begin
        match menu_label.submenu with None -> () | Some menu -> 
            menu_label.submenu <- None;
            Wob.send menu.top#wob WobDestroy
      end
  | _ -> ()

let rec find_select_aux labels i len =
  if i=len then raise Exit
  else
  if labels.(i).selected then i else
    find_select_aux labels (i+1) len
  
let rec find_select menu list =
  try
    let labels = menu.labels in
    let i = find_select_aux labels 0 (Array.length labels) in
    let list = (menu, i) :: list in
    match labels.(i).submenu with
      None -> list
    | Some menu ->
        find_select menu list
  with _ -> list

let key_up menu =
  let list = find_select menu [] in
  match list with
  | (menu, i) :: _ ->
      unselect menu;
      let len = Array.length menu.labels in
      if i > 0 then
        preselect menu.labels.(i-1)
      else
        preselect menu.labels.(len-1)
  | [] -> 
      let len = Array.length menu.labels in
      if len>0 then
        preselect menu.labels.(len-1)

let key_down menu =
  let list = find_select menu [] in
  match list with
  | (menu, i) :: _ ->
      unselect menu;
      let len = Array.length menu.labels in
      if i + 1 < len then
        preselect menu.labels.(i+1)
      else
        preselect menu.labels.(0)
  | [] -> 
      let len = Array.length menu.labels in
      if len>0 then
        preselect menu.labels.(0)
      
let key_right menu =
  let list = find_select menu [] in
  match list with
    [] -> key_down menu
  | (menu, i) :: _ ->
      let menu_label = menu.labels.(i) in
      match menu_label.submenu with
        None -> ()
      | Some menu ->
          let len = Array.length menu.labels in
          if len > 0 then begin
              unselect menu;
              preselect menu.labels.(0)
            end        

let key_left menu =
  let list = find_select menu [] in
  match list with
    [] -> ()
  | (menu, i) :: _ -> unselect menu
      
let key_select menu =
  let list = find_select menu [] in
  match list with
    [] -> key_right menu
  | (menu, i) :: _ -> select_open menu.labels.(i)

      (* if true, the first item matched is selected (executed). if false,
  the item is only preselected, and enter must be hit to execute. *)
let key_direct_select = ref false
      
let rec key_direct menu keysym =
  let list = find_select menu [] in
  let (menu,i) = match list with
      [] -> menu, -1
    | s :: _ -> s in
  try
    let c = keysym - XK.xk_a in
    let min = Char.chr (Char.code 'a' + c) in
    let maj = Char.chr (Char.code 'A' + c) in
    for i = i+1 to Array.length menu.labels - 1 do
      let label = menu.labels.(i).label#string in
      if String.length label > 0 &&
        (let c = label.[0] in c = min || c = maj) then
        ((if !key_direct_select then select_open else preselect_open)
          menu.labels.(i); raise Exit)
    done;
    (* not found ... *)
  with _ -> raise Exit
      
let menutop_hook menu desc e =
  match e with
    WobButtonPress _
  | WobButtonRelease _ ->
      popdown_menu menu
  | WobMap ->
      Xlib.raiseWindow display desc#wob.w_window;
  | WobKeyPress (e,s,keysym) ->
      let modifiers = e.Xkey.state in
      if keysym = XK.xk_Escape then 
        popdown_menu menu
      else if keysym = XK.xk_Return then
        key_select menu
      else if keysym = XK.xk_Up then
        key_up menu
      else if keysym = XK.xk_Down then
        key_down menu
      else if keysym = XK.xk_Left then
        key_left menu
      else if keysym = XK.xk_Right then
        key_right menu
      else if keysym <= XK.xk_0 && keysym >= XK.xk_9 then
        let list = find_select menu [] in
        let menu = match list with
            [] -> menu
          | (menu,i) :: _ -> menu
        in select menu.labels.(keysym - XK.xk_0)
      else if keysym >= XK.xk_a && keysym <= XK.xk_z then
        key_direct menu keysym
  | WobEnter ->  ()
  | _ -> ()

let label s = 
  let l = Label.make s in
  l#set_font !!menu_font;  
  l#set_extensible_width 3;
  l
      
let make_menu target labels button =
  let menu = {
      name = (incr menus;Printf.sprintf "Menu %d" !menus);
      target = target;
      button = button;
      topmenu = None;
      top = target.w_oo; (* to initialize the field *)
      labels = [||];
    } in
  let w = target in
  let submenu = ref None in
  let labels = Array.of_list (List.map (fun (s,options,cmd) -> 
          let label = label s in
          let bar = Bar.make Horizontal [| Wob.desc label |] in
          let menu_label = {
              bar = (bar :> wob_desc);
              label = label;
              menu = menu;
              selected = false;
              submenu = None; (* for now *)
              cmd = cmd;
            } in
          bar#set_min_width !!menu_min_width;
          bar#set_min_height !!menu_min_height;
          bar#set_mask (ButtonPressMask :: ButtonReleaseMask :: label#mask);
          bar#add_hook (menu_hook menu_label);
          let pixmaps = ref [] in
          
          let fg = ref !!menu_foreground in
          let bg = ref !!menu_background in
          let image = ref !!menu_image in
          
          let hi_image = ref !!menu_hilite_image in
          let hi_fg = ref !!menu_hilite_foreground in
          let hi_bg = ref !!menu_hilite_background in
          List.iter (fun op ->
              match op with
                ItemPixmap (pix, where) ->
                  (try
                      let pixmap = Pixmap.make w pix in
                      if where then bar#insert_item 0 (pixmap :> wob_desc)
                      else bar#add_item (pixmap :> wob_desc);
                      pixmaps := pixmap :: !pixmaps
                    with _ -> ())
              | ItemFont font -> label#set_font (font ())
              | ItemForeground fgc -> 
                  fg := (fgc ());
                  hi_bg := !fg;
              | ItemBackground bgc -> 
                  image := NoImage;
                  hi_image := NoImage;
                  bg := (bgc ());
                  hi_fg := !bg
              | ItemTitle -> 
                  if !image <> NoImage then image := !!menu_title_image;
                  if !fg = !!menu_foreground then
                    fg := !!menu_title_foreground;
                  if !bg = !!menu_background then
                    bg := !!menu_title_background;
              | _ -> ()
          ) options;
          label#set_foreground !fg; 
          label#set_hilite_foreground !hi_fg;
          if (not !!use_imlib) || !image = NoImage then label#set_background !bg 
          else label#set_backimage !image;
          if (not !!use_imlib) || !hi_image = NoImage then 
            label#set_hilite_background !hi_bg
          else label#set_hilite_backimage !hi_image;
          if s = "" then label#hilite;
          List.iter (fun p -> 
              p#set_foreground !fg;
              p#set_background !bg;
          ) !pixmaps;
          menu_label
    ) labels) in
  let bar = Bar.make Vertical (Array.map (fun label -> label.bar) labels) in
  bar#set_background !!menu_background;
  let top = Top.make target "" [menutop_hook menu]
      (bar :> wob_desc) None None None None in
  menu.top <- (top :> wob_desc);
  menu.labels <- labels;
  menu

let _ = forward_make_menu := make_menu

let menu_list = Wobenv.new_var ()
let noOwnerEvents = false
let ownerEvents = true
  
let std_popup_menu target root_x root_y button menu =
  let w = target in
  let tw = w.w_top in
  let sw = tw.w_parent in
  let screen = sw.w_screen in
  let scr = screen.s_scr in
  let top = 
    let list = try Wob.getenv sw menu_list with Not_found -> [] in
    try 
      let stdmenu = List.assq menu list in
      if stdmenu.target == target && stdmenu.button = button then stdmenu.top
      else 
      let list = Utils.removeq menu list in
      Wob.send stdmenu.top#wob WobDestroy;
      let stdmenu = make_menu target menu button in
      Wob.setenv sw menu_list ((menu, stdmenu) :: list);
      stdmenu.top
    with Not_found ->
        let stdmenu = make_menu target menu button in
        Wob.setenv sw menu_list ((menu, stdmenu) :: list);
        stdmenu.top
  in
  let tw = top#wob in
  let g = tw.w_geometry in
  let sg = sw.w_geometry in
  g.x <- min root_x (sg.width - g.width);
  g.y <- min root_y (sg.height - g.height);
  prevent_autoraise := true;
  Wob.send tw WobMap;
  let scr = sw.w_screen.s_scr in
  X.grabKeyboard display tw.w_window noOwnerEvents
    GrabModeAsync GrabModeAsync currentTime;
    (* Is-this really useful ?? *)
  X.grabPointer display tw.w_window ownerEvents [
    ButtonReleaseMask] 
    GrabModeAsync GrabModeAsync noConfineTo noCursor currentTime;
  top#wob
  
  
class stdmenu_manager = object
  method menu = std_popup_menu
  method reset sw = Wob.setenv sw menu_list [];
end
  
let init () =
  Stdconfig.menu_manager := new stdmenu_manager
  