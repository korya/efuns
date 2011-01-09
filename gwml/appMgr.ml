(***********************************************************************)
(*                                                                     *)
(*                             Gwml                                    *)
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
  
(* Application manager *)

  (* This is a small hack to display a bar with all windows satisfying
  a given predicate. 
  Button 1 -> Deiconify + raise_window
  Button 2 -> Iconify/Deiconify
  Button 3 -> Deiconify + Virtual goto 
*)

type app_manager = {
    pred : client_desc -> bool;
    bar : Bar.bar;
    force : int;
    mutable labels: (wob_desc * Label.label) list;
  }
  
let managers = Wobenv.new_var ()
let manager_label = Wobenv.new_var ()
let min_width = define_option ["iconMgr_width"] "" int_option 150
let min_height = define_option ["iconMgr_label_height"] "" int_option 17
(* [create wob x y pred]  *)

let create w x y name force pred =
  let bar = 
    let label = new Label.label name in
    label#set_min_width !!min_width;
    label#set_max_width !!min_width;
    label#set_font !!iconMgr_title_font;
    label#set_foreground !!iconMgr_title_foreground;
    label#set_background !!iconMgr_title_background;
    label#set_backimage !!iconMgr_title_image;    
    new Bar.bar Vertical [| Wob.desc label |]
  in
  let top_hook top e =
    match e with
    | WobEnter ->
        let tw = top#wob in
        raiseWindow display tw.w_window;
        ontop_update tw
    | WobInit ->
        let w = top#wob in
        let sw = w.w_parent in
        let sg = sw.w_geometry in
        let g = w.w_geometry in
        g.x <- min (sg.width - !!min_width) x;
        g.y <- y;
    
    | _ -> ()
  in
  let sw = w.w_top.w_parent in
  let sg = sw.w_geometry in

  let top = Top.make sw ("appMgr_" ^ name) 
    [top_hook] (bar :> wob_desc) None None None None in
  let g = top#wob.w_geometry in
  g.x <- min (sg.width - !!min_width) x;
  g.y <- y;
  
  let old = try Wob.getenv sw managers with _ -> [] in
  let m = { pred = pred; bar = bar; force = force; labels = []; } in
  Wob.setenv sw managers (
    Sort.list (fun m1 m2 -> m1.force <= m2.force)
    (m :: old));
  Wob.send top#wob WobMap
  
let goto w () =
  let w = w.w_top in
  let s = w.w_screen.s_scr in
  (!desk_manager)#goto w;
  (!virtual_manager)#goto w;
  Wob.send w.w_top WobRaiseWindow;
  X.warpPointer display s.scr_root 0 0 s.scr_width s.scr_height 
    w.w_window 20 20

let iconMgr_actions = define_option ["iconMgr_actions"] 
  "<iconMgr_actions> is the bindings list for each label in the icon manager
  (the application manager). See <screen_actions> for the format of a list
  of bindings."
    (list_option binding_option) []

let toggle_iconify w =
  let tw = w.w_top in
  let c = tw.w_oo#client in
  if c.c_wm_state = IconicState || c.c_wm_state = WithdrawnState then
    Wob.send tw (WobDeiconifyRequest true)
  else
    Wob.send tw (WobIconifyRequest true)
  
let _ =
  define_action "toggle_iconify" (Function toggle_iconify);
  define_action "goto_window" (Function goto_window);
  if !!iconMgr_actions = [] then begin
      iconMgr_actions =:= [
        Button (1, 0), NamedAction "raise_window", false;
        Button (2, 0), NamedAction "toggle_iconify", false;
        Button (3, 0), NamedAction "goto_window", false;        
      ];
    end

let client_var = Wobenv.new_var ()
    
let execute_action action w =
  let w = Wob.getenv w client_var in
  match get_action action with
    NoAction -> ()
  | Function f -> f w
  | Menu m -> ignore (popup_menu w true (m ()))
  | ActiveMenu m -> ignore (popup_menu w true (m w))
  | NamedAction _ -> assert false
  | NamedActionX _ -> assert false
      
let convert_bindings l =
  List.map (fun (key, action, grabbed) ->
      (key, execute_action action, grabbed)) l
  
let label_hook c tw desc e =
  match e with
    WobInit ->
      desc#set_actions (convert_bindings !!iconMgr_actions);
      desc#set_mask (ButtonPressMask :: desc#mask);
      Log.catch "AppMgr.label_hook: WobInit -> %s" (fun _ -> 
          Wob.setenv desc#wob client_var tw)
  | _ -> () 
      
let update c tw =
  let sw = tw.w_parent in
  let managers = Wob.getenv sw managers in
  try
    List.iter (fun m ->
        if m.pred c then
          let c_label = Wob.getenv tw manager_label in
          c_label#set_string (Printf.sprintf "%s%s" 
              (if Wob.sgetenv tw is_iconified_var false then "(ICON) " else
              if not (Wob.sgetenv tw is_in_dvroom_var true) then
                "(DESK) " else "")
            (!comp_name c.c_name));
          raise Exit
    ) managers
  with _ -> ()
      
let hook c desc e =
  match e with
    WobCreate ->
      let tw = desc#wob in
      let sw = tw.w_parent in
      let managers = Wob.getenv sw managers in
      List.iter (fun m ->
          if m.pred c then
            let c_label = new Label.label (!comp_name c.c_name) in
            c_label#set_min_height !!min_height;
            c_label#set_max_width !!min_width;
            c_label#set_mask (ButtonPressMask :: c_label#mask);
            c_label#set_borderwidth 1;
            c_label#add_hook (label_hook c tw c_label);
            
            c_label#set_font !!iconMgr_font;            
            c_label#set_background !!iconMgr_background;
            c_label#set_foreground !!iconMgr_foreground;
            c_label#set_backimage !!iconMgr_image;
            
            c_label#set_hilite_foreground !!iconMgr_active_foreground;
            c_label#set_hilite_background !!iconMgr_active_background;
            c_label#set_hilite_backimage !!iconMgr_active_image;
            
            m.bar#add_item (Wob.desc c_label);
            m.labels <- (desc, c_label) :: m.labels;
            Wob.setenv tw manager_label c_label;
            
            let tw = m.bar#wob.w_top in             
            let s = sw.w_screen in
            let (c,w) = Wintbl.find s.s_clients tw.w_window in 
            let tg = tw.w_geometry in 
            let g = w.w_geometry in
            g.width <- tg.width;
            g.height <- tg.height;
            Wob.send w.w_top WobGetSize;
            
            Wob.send c_label#wob WobMap;
            
            raise Exit) managers
  | WobPropertyChange p when p = XA.xa_wm_name -> 
      let tw = desc#wob in update c tw
  | WobUnmap _
  | WobMap ->
      let tw = desc#wob in update c tw
  | WobDestroy ->
      let tw = desc#wob in
      let sw = tw.w_parent in
      let managers = Wob.getenv sw managers in
      List.iter (fun m ->
          if m.pred c then
            let c_label = Wob.getenv tw manager_label in
            let items = m.bar#items in
            for i = 0 to Array.length items - 1 do
              if (Wob.desc c_label) == items.(i) then
                begin
                  m.bar#remove_item i;
                  m.labels <- List.remove_assq desc m.labels;
                  raise Exit
                end
            done;
      ) managers
  | _ -> ()

let noOwnerEvents = false
  
let rec first_client managers =
  match managers with
    [] -> raise Exit
  | { labels = [] } :: managers -> first_client managers
  | { labels = client :: clients } :: managers -> client, clients, managers

let rec next_client clients managers all_managers =
  match clients with
    client :: clients -> client, clients, managers
  | [] ->
      match managers with
        [] -> first_client all_managers
      | { labels = [] } :: managers -> next_client [] managers all_managers
      | { labels = client :: clients } :: managers ->
          client, clients, managers

let prev_client client managers =
  let first_client = first_client managers in
  let rec iter ((_, clients, ms) as prev_client) =
    let (next_client, _, _) as next = next_client clients ms managers in
    if next_client == client then prev_client
    else iter next
  in
  iter first_client
          
let highlight ((_,label), _,_) = label#hilite
let unhighlight ((_,label), _, _) = label#unhilite
  
let key_control sw managers =
  let s = sw.w_screen in
  let scr = s.s_scr in
  let client = ref (first_client managers) in
  highlight !client;
  try
    X.grabKeyboard display scr.scr_root noOwnerEvents 
      GrabModeAsync GrabModeAsync currentTime;
      (* First of all, raise all the manager windows *)
    List.iter (fun m ->
        Xlib.raiseWindow display m.bar#wob.w_top.w_window) managers;
    while true do
      let ev = Xlib.peekEvent display in
      match ev.ev_event with
      | KeyReleaseEvent _ -> ()
      | KeyPressEvent e -> 
          let modifiers = e.Xkey.state in
          let (s,keysym,m) = KeyBind.lookupString display ev.ev_event in
          if keysym = XK.xk_Escape then raise Exit;
          if keysym = XK.xk_Tab || keysym = XK.xk_Down
            || keysym = XK.xk_Up then 
            begin
              unhighlight !client;
              let (c, clients, ms) = !client in
              if (modifiers land controlMask <> 0) || keysym = XK.xk_Up then
                client := next_client clients ms managers
              else
                client := prev_client c managers;
              highlight !client;
            end else 
          let ((top, _), _, _) = !client in
          let c = top#client in
          if keysym = XK.xk_i then begin
              if c.c_wm_state = IconicState || c.c_wm_state = WithdrawnState
              then Wob.send top#wob (WobDeiconifyRequest true) else
                Wob.send top#wob (WobIconifyRequest true)
            end
          else if keysym = XK.xk_l then
            Wob.send top#wob WobLowerWindow
          else if keysym = XK.xk_r then
            Wob.send top#wob WobRaiseWindow
          else if keysym = XK.xk_k then
            Wob.send top#wob WobDestroy
          else if keysym = XK.xk_d then
            Wob.send top#wob WobDeleteWindow
          else if keysym = XK.xk_w || keysym = XK.xk_Return then begin
              if c.c_wm_state = IconicState || c.c_wm_state = WithdrawnState
              then Wob.send top#wob (WobDeiconifyRequest true);
              goto top#wob ();
              raise Exit
            end
      | ExposeEvent e -> 
          Eloop.handle_event s.s_scheduler ev
      | _ -> Xlib.putBackEvent display ev
    done;
  with
  | GrabError _ -> unhighlight !client
  | _ ->
      unhighlight !client;
      List.iter (fun m ->
          Xlib.lowerWindow display m.bar#wob.w_top.w_window) managers;
      X.ungrabKeyboard display currentTime
      
let message w msg =
  let sw = w.w_top.w_parent in
  let managers = Wob.getenv sw managers in
  if msg = "key-control" then key_control sw managers else
  ()
      
class manager = (object
  method hook = hook
  method create = create
  method message = message
end : Stdconfig.icon_manager)

let manager = new manager 
  