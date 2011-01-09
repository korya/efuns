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
open Stdplacement
open Stdicon

(* We must check that the mouse is not only passing in the window
to another toplevel window *)
let check_final_dest tw =
(* We are looking for a "EnterNotify" event, occuring inside another 
toplevel window ... *)
  let tw = tw.w_top in
  let pred = ref false in
  try
  match checkPredEvent display (fun e ->
        match e.ev_event with
          EnterNotifyEvent e ->
            begin
              try
                let window = e.Xcrossing.event in
                let (c,cw)= Wintbl.find tw.w_screen.s_clients window in
                not (cw.w_top == tw)
              with Not_found -> false
            end
        | _ -> false
            
    ) true with
      Some e -> Xlib.putBackEvent display e; false
    | _ -> true
  with _ -> true
  
let ignore_focus_click = define_option ["ignore_focus_click"] 
  "<ignore_focus_click> is true if the click used to set the focus to a
    click_to_focus client must not be received by the client." 
    bool_option false 
  
let auto_raise_var, auto_raise_p, set_auto_raise,
  auto_raise_list = define_bool_option  "auto_raise_list" []
  
let click_to_focus_var, click_to_focus_p, set_click_to_focus,
  click_to_focus_list = define_bool_option  "click_to_focus_list" []

let click_to_focus = define_option ["click_to_focus"] 
  "<click_to_focus> is true if you must click on a window to give the focus.
    If <auto_raise> will not work if click_to_focus is true."
    bool_option false 
  
let last_focused = ref 0

let update_focus (desc : wob_desc) focus =  
  if focus then desc#iter (fun s -> 
        s#set_font !!active_font;
        s#hilite;
    ) else desc#iter (fun s -> 
        s#set_font !!title_font;
        s#unhilite
    ) 
    
let name_update c label e =
  match e with
    WobPropertyChange p when p = XA.xa_wm_name -> 
      label#set_string (!comp_name c.c_name); 
  | WobPropertyChange p when p = XA.xa_wm_hints ->
      if c.c_wm_hints.urgency_hint then
        let w = label#wob in
        let (c,cw)= Wintbl.find w.w_screen.s_clients c.c_window in
        (!virtual_manager)#place_on_screen w.w_top;
        Wob.send_one w.w_top  
          (if c.c_wm_state = NormalState then WobMove
          else (WobDeiconifyRequest false))
  | _ -> ()

let is_iconified w =
  Wob.sgetenv w.w_top is_iconified_var false

let focus_new_window = define_option ["focus_new_window"]
  "<focus_new_window> is true if the focus to automatically go
    to new mapped windows." bool_option true

let transient_p c = not (c.c_transient_for == noWindow)

type direction = North | East | West | South
  
let direction_option = sum_option [
    "north", North; "east", East; "west", West; "south", South]
  
let reduced_window_var, get_reduced_window, set_reduced_window,
  reduced_window_list = define_class_option "reduced_window_list"
  "<reduced_window_list> is a list of associations between a tuple 
  (direction (east,west,north,south) * size) and a list of client
  descriptions. The direction indicates the sens of the reduction and
  the size specifies the final size of the window reduced."
    (tuple2_option (direction_option, int_option))
  [ (East, 30), [ "GwML", "gwml_pager"; "GwML", "gwml_appMgr_*"];
  
    (North, 0), ["", ""];
    (North, 10), ["XConsole",""]
  ]

let pointer_inp_var = Wobenv.new_var ()
let reduced_windowp_var = Wobenv.new_var ()
let unreduce_window_size_var = Wobenv.new_var ()

  (* reduce_window can only be applied to fixed size windows, since it 
  remembers the previous size of the window and doesn't take into account
  changes *)

let reduced_window_temp_var = Wobenv.new_var ()
  
let get_reduced_window c tw =
  try
    Wob.getenv tw reduced_window_temp_var
  with _ -> get_reduced_window c
      
let unreduce_window c tw force =
  try
    if Wob.getenv tw reduced_windowp_var then
      let (dir, size) = get_reduced_window c tw in
      if size <> 0 || force then
        let g = tw.w_geometry in
        let (x,y) = Wob.getenv tw unreduce_window_size_var in
        Wob.setenv tw reduced_windowp_var false;
        g.x <- x; g.y <- y; 
        moveWindow display tw.w_window g.x g.y;
        Wob.send_one tw (WobGetSize)      
  with _ -> ()
      
let reduce_window c tw = 
  if not (Wob.sgetenv tw reduced_windowp_var false) then
    let (dir, size) = get_reduced_window c tw in
    if size <> 0 then
      let g = tw.w_geometry in
      Wob.setenv tw unreduce_window_size_var (g.x,g.y);
      Wob.setenv tw reduced_windowp_var true;
      (match dir with
          North -> g.height <- size
        | South -> g.y <- g.y + g.height - size; g.height <- size
        | West -> g.width <- size
        | East -> g.x <- g.x + g.width - size; g.width <- size);
      moveResizeWindow display tw.w_window g.x g.y g.width g.height
  
let set_temp_reduced_window c tw v = 
  unreduce_window c tw true;
  Wob.setenv tw reduced_window_temp_var v

let timer = ref 0
  
let c_hook c (desc : Top.top) e = 
  let tw = desc#wob in
  match e with
  | WobInit -> 
      desc#set_actions (convert_bindings !!window_actions);
      desc#set_borderwidth !!frame_borderwidth;
      desc#set_mask (ButtonPressMask :: desc#mask);
      Wob.setenv tw.w_top ontop_var (is_on_top c);
      Wob.setenv tw.w_top onbottom_var (is_on_bottom c);
  
  | WobCreate ->
      let s = tw.w_screen in
(* set_grabs display tw.w_window !simple_window_grabs; *)
      let g = tw.w_geometry in
      g.x <- c.c_geometry.x;
      g.y <- c.c_geometry.y;
      Wob.setenv tw focus_var false;
      update_focus (desc :> wob_desc) false;
      stack_raise tw;
  
  | WobMapRequest ->  (* placement *)
      let placement_policy = find_placement c in
      placement_policy c tw;
      if c.c_wm_state = IconicState then
        Wob.send tw (WobIconifyRequest true)
      else
      if c.c_wm_state = WithdrawnState then
        Wob.send tw (WobUnmap false)
      else
(* if we want to iconify windows during placement .. *)
      if c.c_wm_state <> IconicState &&
        (Wob.sgetenv tw is_in_dvroom_var true ||
          Wob.sgetenv tw sticky_var false) then begin
          Wob.send_one tw WobMap;
          if !!focus_new_window && not (!!click_to_focus || click_to_focus_p c)
          then
            Wob.send_one tw (WobSetInputFocus true)
        end
  
  | WobButtonPress _ -> (* click always sets the focus *)
      unreduce_window c tw true;
      Wob.send tw (WobSetInputFocus true);
      Wob.send tw (WobInstallColormap false)
  
  | WobMove ->
(* If the client is reduced, we must of the new position *)
      let g = tw.w_geometry in
      Wob.setenv tw unreduce_window_size_var (g.x,g.y)
  
  | WobClickInClient -> 
      let focus = Wob.getenv tw focus_var in
      if not focus && (!!click_to_focus || click_to_focus_p c) then begin
          Wob.send tw WobRaiseWindow;
          Wob.send tw (WobSetInputFocus true);
          Wob.send tw (WobInstallColormap false)
        end;
      let s = tw.w_screen in
      let (c,w) = Wintbl.find s.s_clients desc#client.c_window in      
      X.allowEvents display (
        if !!ignore_focus_click && not focus then begin
            Log.catch "ungrab: %s\n" 
              (fun _ -> X.ungrabButton display w.w_window anyButton anyModifier);
            AsyncPointer 
          end
        else ReplayPointer) currentTime;
  
  | WobRaiseWindow ->
      if not (c.c_transient_for == noWindow) then begin
          try
            let s = tw.w_screen in
            let (_,ww) = Wintbl.find s.s_clients c.c_transient_for  in
            stack_raise ww;
            X.configureWindow display tw.w_top.w_window [
              CWSibling ww.w_top.w_window; CWStackMode Above]
          with _ -> stack_raise tw
        end
      else stack_raise tw
  
  | WobLowerWindow ->
      stack_lower tw
  
  | WobEnter ->
      if check_final_dest tw then begin
          Wob.setenv tw pointer_inp_var true;
          unreduce_window c tw false;
          incr last_focused;
          let last_focused_old = !last_focused in
          if not (!!click_to_focus || click_to_focus_p c) || auto_raise_p c 
          then begin
              Wob.send tw (WobSetInputFocus false);

(* click_to_focus is disabled if auto_raise is true *)
              if !!auto_raise || auto_raise_p c then begin
                  incr timer;
                  Concur.Thread.add_timer !!auto_raise_delay (fun _ ->
                      if (* !last_focused = last_focused_old && *)
                        not !prevent_autoraise &&
                        Wob.sgetenv tw pointer_inp_var false
                      then begin
                          Wob.send tw WobRaiseWindow;
                          Wob.send tw (WobSetInputFocus true);
                        end
                  )      
                end
            end;
          if !!auto_colormap then
            Concur.Thread.add_timer 0.05 (fun _ ->
                let gif = X.getInputFocus display in
                if gif.gif_win = c.c_window then
                  Wob.send tw (WobInstallColormap false);
            );
        end
        
  | WobLeave true ->
      Wob.setenv tw pointer_inp_var false;
      Concur.Thread.add_timer 1.0 (fun _ ->
          if not (Wob.getenv tw pointer_inp_var) then reduce_window c tw)
      
  | WobIconifyRequest _ ->
      Sound.file_play !!deiconify_sound;
      let (icon, group) = try Wob.getenv tw icon_var with Not_found ->
            let i = icon_make c tw in
            let group = ref [] in
            Wob.setenv tw icon_var (i, group); i,group in
            (* Group iconification *)
      (* Normal iconification *)
      let i = icon#wob in
      Wob.send i WobMapRequest;
      last_iconify_windows := tw :: !last_iconify_windows;
      Wob.setenv tw is_iconified_var true;
      Wob.setenv tw is_group_iconified_var false;                    
      Wob.send tw (WobUnmap true);
      !!Animate.resize_anim tw tw.w_geometry i.w_geometry;
      if !!group_iconification then
        begin
          group := [];
          Wintbl.iter (fun win (cc,cw) ->
              if win = cc.c_window then
                if cc.c_wm_hints.window_group = c.c_window &&
                  cc.c_wm_state = NormalState then
                  ( let tw = cw.w_top in
                    Wob.send tw (WobUnmap true);
                    Wob.setenv tw is_iconified_var true;
                    Wob.setenv tw is_group_iconified_var true;                    
                    !!Animate.resize_anim tw tw.w_geometry i.w_geometry; 
                    group := win :: !group)
          ) tw.w_screen.s_clients;
        end;
      (!undo_manager)#add_undo (fun _ ->
          Wob.send tw (WobDeiconifyRequest true)
      )
  
  | WobDeiconifyRequest force ->
      if force || not (stay_iconified tw) then begin
          Sound.file_play !!iconify_sound;
          (try
              let (icon, group) = Wob.getenv tw icon_var in
              let i = icon#wob in      
              Wob.send i (WobUnmap true);
              let ig = i.w_geometry in
              !!Animate.resize_anim tw ig tw.w_geometry;
              Wob.setenv tw is_iconified_var false;
              Wob.setenv tw is_group_iconified_var false;                    
              Wob.send_one tw WobMap;
          (* Group iconification *)
              Wintbl.iter (fun win (cc,cw) ->
                  if win = cc.c_window then
                    if cc.c_wm_hints.window_group = c.c_window &&
                      cc.c_wm_state = IconicState &&
                      List.memq win !group
                    then begin
                        let tw = cw.w_top in
                        !!Animate.resize_anim tw ig tw.w_geometry;
                        Wob.setenv tw is_iconified_var false;
                        Wob.setenv tw is_group_iconified_var false; 
                        Wob.send tw WobMap;
                      end
              ) tw.w_screen.s_clients;
              group := []
            
            with Not_found -> ());
          Wob.setenv tw is_iconified_var false;
          Wob.setenv tw is_group_iconified_var false;                    
          Wob.send_one tw WobMap; (* to be sure *)
          Wob.send tw WobRaiseWindow;
          (!undo_manager)#add_undo (fun _ ->
              Wob.send tw (WobIconifyRequest true)
          )
        end
        
  | WobDestroy ->
      stack_remove tw;
      Sound.file_play !!exit_sound;
      (try
          let icon,group = Wob.getenv tw icon_var in
          Wob.send icon#wob WobDestroy;        
          (* Group iconification *)
          Wintbl.iter (fun win (cc,cw) ->
              if win = cc.c_window then
                if cc.c_wm_hints.window_group = c.c_window &&
                  cc.c_wm_state = IconicState &&
                  List.memq win !group
                then
                  Wob.send cw.w_top WobMap;
          ) tw.w_screen.s_clients;
        
        with Not_found -> ());
        (* ensure window is not mapped *)
      Animate.tv_close tw

  | WobMessage "minimize" ->
      Sound.file_play !!unmaximize_sound;
      let deco = desc#deco in 
      let mini = try Wob.getenv tw is_minimized_var with _ -> 0 in
      let w =
        match deco.Top.center with
          None -> raise Not_found
        | Some w -> w
      in 
      if mini = 0 then
        let bw =
          match deco.Top.title with
            None -> raise Not_found
          | Some w -> w
        in 
        Wob.send w (WobUnmap true);
        let g = bw.w_geometry in
        let tg = tw.w_geometry in
        Wob.setenv tw is_minimized_var tg.height;
        tg.height <- g.height;
        Wob.send tw (WobResize true)
      else
        begin
          Wob.send w WobMap;
          let tg = tw.w_geometry in
          Wob.setenv tw is_minimized_var 0;
          tg.height <- mini;
          Wob.send tw (WobResize false)
        end 
        
  | WobExitGwml ->
      let mini = Wob.sgetenv tw is_minimized_var 0 in
      let mini = Wob.sgetenv tw is_minimized_var 0 in
      let deco = desc#deco in
      let w =
        match deco.Top.center with
          None -> raise Not_found
        | Some w -> w
      in 
      if mini <>0 then
        begin
          X.mapWindow display w.w_window;
          Wob.send w WobMap;
          let tg = tw.w_geometry in
          tg.height <- mini;
          Wob.send tw (WobResize true)
        end
  
  | WobClientFocus focus ->
      Wob.setenv tw focus_var focus;
      update_focus (desc :> wob_desc) focus;
      if not focus && !!ignore_focus_click then
      (* client is loosing focus: reset the click grab *)
        let s = tw.w_screen in
        let (c,w) = Wintbl.find s.s_clients desc#client.c_window in
        X.grabButton display w.w_window true [ButtonPressMask] 
        GrabModeSync GrabModeAsync noWindow noCursor
          anyButton anyModifier
        
  | _ -> ()
      
class c_ledit c =
  object (self)
  
  inherit Ledit.ledit (!comp_name c.c_name)
  
  method edit_set_string = 
    Icccm.setWM_NAME display c.c_window self#string;
    Icccm.setWM_ICON_NAME display c.c_window self#string
end

let bottom_bar = define_option ["bottom_bar"] "" bool_option false
let pixmap_corners = define_option ["pixmap_corners"] "" bool_option false

let window_popup = define_option ["window_popup"] "" menu_option []

let remove_null = define_option ["remove_null"] 
  "Should null borders be transparent (Stddeco)" bool_option true

let add_borders = define_option ["add_borders"] "" bool_option false

let mini_hook sw o e = 
    match e with
      WobButtonPress e ->
        if Wob.click sw.w_screen = Double then
          let w = o#wob in
          let tw = w.w_top in
          Wob.send tw (WobMessage "minimize")
        else 
        let tw = o#wob.w_top in
        User.move tw true;
        Wob.send tw WobRaiseWindow
    | _ -> ()

let make_null sw = 
  let pixmap = Null.make () in
  pixmap#set_mask (ButtonPressMask :: pixmap#mask);
  pixmap#add_hook (mini_hook sw pixmap);
(*  pixmap#set_min_width !!window_borderwidth;
  pixmap#set_min_height !!window_borderwidth; *)
  Wob.desc pixmap
  
  
let simple_window sw c =
  let label = 
    let label = if !!editable_title then 
        (new c_ledit c :> Label.label) else
      let label = Label.make (!comp_name c.c_name) in
      label#set_mask (ButtonPressMask :: label#mask);
      label#add_hook (mini_hook sw label);
      label
    in
    label#add_hook (name_update c label);
    label#set_min_height 17;
    label#set_min_width 200;
    label#set_font !!window_font;
    label#set_background !!title_background;
    label#set_foreground !!title_foreground;
    label#set_backimage !!title_image;
    label#set_hilite_backimage !!active_image;
    label#set_hilite_background !!active_background;
    label#set_hilite_foreground !!active_foreground;
    label#set_extensible_width 2;
    label
  in
  let pixmap = 
    try
      let filename = get_title_icon c in
      let pixmap = Pixmap.make sw (FromFile filename) in
      Wob.desc pixmap
    with _ -> Wob.desc (Null.make ())
  in
  pixmap#set_mask (ButtonPressMask :: pixmap#mask);
  pixmap#add_hook (fun e ->
      match e with
        WobButtonPress e ->
          let _ = popup_menu sw true !!window_popup  in ()
      | _ -> ()      
  );
  pixmap#set_shaped false;
  
  let top_bar = match !!title_justification with
      Center ->
        let left = Null.make () in
        if !!remove_null then left#no_shape;
        left#set_background !!title_background;
        left#set_backimage !!title_image;
        left#set_hilite_backimage !!active_image;
        left#set_hilite_background !!active_background;
        left#set_hilite_foreground !!active_foreground;
        left#set_mask (ButtonPressMask :: left#mask);
        left#add_hook (mini_hook sw left);
        let right = Null.make () in
        if !!remove_null then right#no_shape;
        right#set_background !!title_background;
        right#set_backimage !!title_image;
        right#set_hilite_backimage !!active_image;
        right#set_hilite_background !!active_background;
        right#set_mask (ButtonPressMask :: right#mask);
        right#add_hook (mini_hook sw right);
        [| Wob.desc pixmap; Wob.desc left; Wob.desc label; Wob.desc right |]
    | Left -> 
        let right = Null.make () in
        if !!remove_null then right#no_shape;
        right#set_background !!title_background;
        right#set_backimage !!title_image;
        right#set_hilite_backimage !!active_image;
        right#set_hilite_background !!active_background;
        right#set_mask (ButtonPressMask :: right#mask);
        right#add_hook (mini_hook sw right);
        [| Wob.desc pixmap; Wob.desc label; Wob.desc right |]
    | Right ->
        let left = Null.make () in
        if !!remove_null then left#no_shape;
        left#set_background !!title_background;
        left#set_backimage !!title_image;
        left#set_hilite_backimage !!active_image;
        left#set_hilite_background !!active_background;
        left#set_mask (ButtonPressMask :: left#mask);
        left#add_hook (mini_hook sw left);
        [| Wob.desc pixmap; Wob.desc left; Wob.desc label |]
  in
  
  let make_pix () = 
    let pixmap = 
      try
        let filename = get_title_icon c in
        let pixmap = Pixmap.make sw (FromFile filename) in
        Wob.desc pixmap
      with _ -> Wob.desc (Null.make ())
    in
    pixmap#set_mask (ButtonPressMask :: pixmap#mask);
    pixmap#add_hook (mini_hook sw pixmap);
    pixmap#set_shaped false;
    pixmap
  in  
  
  let bottom_bar = 
    if !!bottom_bar then 
      let center = Null.make () in
      center#set_background !!title_background;
      center#set_mask (ButtonPressMask :: center#mask);
      center#add_hook (mini_hook sw center);
      center#set_min_height 10;
      if !!remove_null then center#no_shape;      
      if !!pixmap_corners then
        Some (Wob.desc (Bar.make Horizontal
              [| make_pix (); Wob.desc center; make_pix () |]))
      else Some (Wob.desc center)
    else None 
  in
  let top_bar = if !!pixmap_corners then
      Array.concat [top_bar; [|make_pix ()|]] else top_bar in
  
  let top_bar = Bar.make Horizontal top_bar in
    
  let top_bar = if !!add_borders then
      let top_bar = Bar.make Vertical [| Wob.desc top_bar; make_null sw |] in
      top_bar#set_extensible_width 1;
      Bar.make Horizontal [| Wob.desc top_bar |]
    else top_bar
  in
  let bottom_bar = if !!add_borders then
      match bottom_bar with
        None -> Some (make_null sw)
      | Some bar -> 
          Some (
            let bottom_bar = Bar.make Vertical [| make_null sw; bar |] in
            bottom_bar#set_extensible_width 1;
            let bottom_bar = Bar.make Horizontal [| Wob.desc bottom_bar |] in
            Wob.desc bottom_bar
          )
          
    else bottom_bar
  in

  let left_bar = if !!add_borders then Some (make_null sw) else None in
  let right_bar = if !!add_borders then Some (make_null sw) else None in
  
  ([c_hook c; icon_manager_hook c; 
      desk_manager_hook c], left_bar, right_bar, 
    Some (Wob.desc top_bar) , bottom_bar)
    
let sticky_hook desc e =
  match e with
  | WobCreate -> 
      let w = desc#wob in
      Wob.setenv w sticky_var true;
      Wob.setenv w is_in_dvroom_var true;
      (!virtual_manager)#omit_move w true;
  | _ -> ()
  
      (* no_deco: no decoration, stay on screen *)
let no_deco sw c = 
  ([c_hook c; sticky_hook; desk_manager_hook c], None, None, None, None)
  
let actions = define_option ["actions"] ""
    (list_option (tuple2_option (string_option, list_option binding_option)))
  []
  
    
let assoc_to_value list (name, v) =
  SmallList [Value name; to_value (List.assoc name list) v]

let value_to_assoc list v =
    match v with
    List [Value name; v] -> name, from_value (List.assoc name list) v
  | _ -> raise Not_found
        
let assoc_option list = define_option_class "Assoc" 
  (value_to_assoc list) (assoc_to_value list)

let object_option = list_option string2_option
let bar_options = [
    "button", object_option;
    "pixmap", object_option;
    "label", object_option;
    "null", object_option;
  ]

let bar_option = list_option (assoc_option bar_options)

let deco_options = [
    "top_bar", bar_option;
    "bottom_bar", bar_option;
    "left_bar", bar_option;
    "right_bar", bar_option
  ]

let deco_option = list_option (assoc_option deco_options)

  
let decorations = define_option ["decorations"] ""
    (list_option (tuple2_option (string_option, deco_option)))
  []
  
let removeif name list = try List.remove_assoc name list with _ -> list

let adder option =
  let o = define_option ["add_" ^ shortname option] ""
      (get_class option) [] in
  option_hook o (fun _ ->
      match !!o with
        [] -> ()
      | list -> 
          o =:= [];
          List.iter (fun (name, value) ->
              let list = removeif name !!option in
              option =:= (name, value) :: list
          ) list);
  o
  
let adder_decorations = adder decorations
let adder_actions = adder actions 

    
let make_text s = 
  let len = String.length s in
  let rec iter start t =
    try
      let fin = String.index_from s start '\n' in
      iter (fin+1) ((String.sub s start (fin - start)) :: t)
    with
      _ ->
        (String.sub s start (len - start)) :: t
  in
  Array.of_list (List.rev (iter 0 []))
 
let tip_win = (Wobenv.new_var () : (Text.text * int ref) Wobenv.var)

let tip_viewable_delay = define_option ["tip_viewable_delay"]
  "<tip_viewable_delay> is the delay in seconds during which a tip
    is viewable" float_option 3.0
  
let standard_tip2 text wob inside =
  let w = wob#wob in
  let sw = w.w_top.w_parent in
  let tip_win, counter = try
      let (label, counter) = Wob.getenv sw tip_win in
      label#set_text text;
      label, counter
    with Not_found ->
        let counter = ref 0 in
        let label = new Text.text text in
        let tip_hook top e =
          match e with
            WobKeyPress _ -> 
              Wob.send label#wob.w_top (WobUnmap false)
          | _ -> ()
        in
        label#set_min_width 100;
        label#set_min_height 50;
        let top = Top.make sw "" [tip_hook] (label :> wob_desc) 
          None None None None in
        Wob.setenv sw tip_win (label, counter);
        label, counter
  in
  if inside then
    let w = tip_win#wob.w_top in
    let g = w.w_geometry in
    let qp = X.queryPointer display sw.w_window in
    g.x <- qp.qp_root_x + 10;
    g.y <- qp.qp_root_y - 3; 
    Wob.send w WobMap;
    Wob.send w WobRaiseWindow;
    incr counter;
    let c = !counter in
    Concur.Thread.add_timer !!tip_viewable_delay (fun _ ->
        if c = !counter then
          Wob.send w (WobUnmap false)
  )
  else
    Wob.send tip_win#wob.w_top (WobUnmap false)

let no_action2 x y = ()
  
let standard_tip s =
 if s <> "" then
    let text = make_text s in
    standard_tip2 text
  else no_action2

let actions_var = Wobenv.new_var ()
let set_actions actions wob = Wob.setenv wob#wob actions_var actions
let get_actions wob = Wob.sgetenv wob#wob actions_var []
  
let list_to_string sep path =
  let s =   List.fold_left (fun str dir -> 
        Printf.sprintf "%s%c%s" str sep dir) "" path in
  if String.length s > 0 then 
    let len = String.length s in
    String.sub s 1 (len-1)
  else ""
  
let standard_tip3 prev_actions text wob _ inside =
  let new_actions = get_actions wob in
  if new_actions <> [] then
  if new_actions == !prev_actions then
    standard_tip !text wob inside
  else
  let t = List.map (fun (key, action, grab) ->
        Printf.sprintf "%15s %30s %s"
          (key_to_string key)
        (match action with
            NamedAction s -> s
          | NamedActionX (s,args) ->
              list_to_string ' ' (s::args)
          | _ -> "(internal)")
        (if grab then "(grabbed)" else "")
    ) new_actions in
  text := "Bindings:\n" ^ (list_to_string '\n' t);
  prev_actions := new_actions;
  standard_tip !text wob inside  
  
let binding_tip wob = 
  let prev_actions = ref [] in
  let text = ref "" in
  standard_tip3 prev_actions text wob  
  
let add_create_hook (desc : wob_desc) f =
  desc#add_hook (fun e -> match e with WobCreate -> f desc | _ -> ())
  
let eval_arg sw c desc (arg, value) =
  try
    match arg with
      "actions" -> 
        begin try
            let actions = List.assoc value !!actions in
            desc#set_actions (convert_bindings actions);
            add_create_hook desc (set_actions actions);
            Log.printf "Action arg %s\n" "done";
          with Not_found -> 
              Log.printf "Stddeco.eval_arg: actions %s not found\n" value
        end
    | "min_width" -> desc#set_min_width (int_of_string value)
    | "min_height" -> desc#set_min_height (int_of_string value)
    | "extensible_width" -> desc#set_extensible_width (int_of_string value)
    | "extensible_height" -> desc#set_extensible_height (int_of_string value)
    | "bg" -> desc#set_background value
    | "fg" -> desc#set_foreground value
    | "image" -> desc#set_backimage (TileImage (
            Utils.string_to_filename value)) 
    | "timage" -> desc#set_backimage (TileImage (
            Utils.string_to_filename value)) 
    | "simage" -> desc#set_backimage (ScaleImage (
            Utils.string_to_filename value)) 
    | "active_fg" -> 
        desc#set_hilite_foreground value
    | "active_bg" ->
        desc#set_hilite_background value
    | "active_image" ->
        desc#set_hilite_backimage (TileImage (
            Utils.string_to_filename value));
    | "active_timage" ->
        desc#set_hilite_backimage (TileImage (
            Utils.string_to_filename value));
    | "active_simage" ->
        desc#set_hilite_backimage (ScaleImage (
            Utils.string_to_filename value));
    | "font" -> desc#set_font value
    | "shaped" -> desc#set_shaped (bool_of_string value)
    | "tip" -> desc#set_tip_display (standard_tip value)
    | _ -> Log.printf "Stddeco.eval_arg: unknown option %s\n" arg
  with e -> Log.exn "Stddeco.eval_arg: %s\n" e
      
let object_make sw c (o_name, o_args) = 
  let desc = 
    match o_name with
    | "button"
    | "pixmap" -> 
        let pixmap =
          try
            let filename = try
                let file = List.assoc "name" o_args in
                Log.printf "PIXMAP: %s\n" file;
                let file = Utils.string_to_filename file in
                Log.printf "PIXMAP: %s\n" file;
                find_pixmap file
              with e ->
                  Log.exn "find_pixmap: %s\n" e;
                  get_title_icon c in
            let pixmap = Pixmap.make sw (FromFile filename) in
            Wob.desc pixmap
          with _ -> Wob.desc (Null.make ())
        in 
        pixmap#set_shaped false;
        pixmap
    | "label" ->
        begin
          let label = 
            try
              let kind = List.assoc "class" o_args in
              let label = match kind with
                  "name" -> raise Not_found (* default *)
                | "class" -> (fst c.c_class) ^ "." ^ (snd c.c_class)
                | _ -> List.assoc "name" o_args
              in
              let label = Label.make label in
              label#set_extensible_width 2;
              label
            with _ ->
                let label = Label.make (!comp_name c.c_name) in
                label#add_hook (name_update c label);
                label#set_extensible_width 2;
                label
          in
          List.iter (fun (name, value) -> try
                match name with
                | "justify" -> label#set_justified (
                      List.assoc value
                        ["left", Left; "center", Center; "right", Right])
                | _ -> ()
              with _ -> ()
          ) o_args;
          Wob.desc label
        end
    | "null" ->
        begin
          let null = Null.make () in
          List.iter (fun (name, value) -> try
                match name with
                  "removed" -> if bool_of_string value then null#no_shape
                | _ -> ()
              with _ -> ()
          ) o_args;
          Wob.desc null
        end
    | _ -> 
        Log.printf "Stddeco.object_make: unknown object %s\n" o_name;
        Wob.desc (Null.make ())
  in
  desc#set_font !!window_font;
  desc#set_background !!title_background;    
  desc#set_foreground !!title_foreground; 
  desc#set_backimage !!title_image;
  desc#set_hilite_backimage !!active_image;
  desc#set_hilite_background !!active_background;
  desc#set_hilite_foreground !!active_foreground;
  desc#set_tip_display (binding_tip desc);
  Log.catch "Stddeco.object_make Eval args: %s\n" (fun _ -> List.iter (
        eval_arg sw c desc) o_args);
  desc
  
let make_bar kind sw c bar_name deco =
  try
    let objects = List.assoc bar_name deco in
    match objects with
      [] -> None
    | _ -> Some (
          Log.printf "Stddeco.make_bar: %d objects\n" (List.length objects);
          let bar = Bar.make kind (Array.map (
                object_make sw c 
              ) (Array.of_list objects)) in
          Wob.desc bar)
  with e -> 
      Log.exn "Stddeco.make_bar: %s\n" e;
      None
  
let generic_window deco_name sw c =
  try
    Log.printf "Stddeco.generic_window: NEW %s\n" deco_name;
    let deco = List.assoc deco_name !!decorations in
    let top_bar = make_bar Horizontal sw c "top_bar" deco in
    let bottom_bar = make_bar Horizontal sw c "bottom_bar" deco in
    let left_bar = make_bar Vertical sw c "left_bar" deco in
    let right_bar = make_bar Vertical sw c "right_bar" deco in    
    let top_bar = if !!add_borders then
        match top_bar with
          None -> Some (make_null sw)
        | Some top_bar -> 
            let top_bar = 
              Bar.make Vertical [| Wob.desc top_bar; make_null sw |] in
            top_bar#set_extensible_width 1;
            let top_bar = Bar.make Horizontal [| Wob.desc top_bar |] in
            Some (Wob.desc top_bar)
      else top_bar
    in
    let bottom_bar = if !!add_borders then
        match bottom_bar with
          None -> Some (make_null sw)
        | Some bar -> 
            Some (
              let bottom_bar = Bar.make Vertical [| make_null sw; bar |] in
              bottom_bar#set_extensible_width 1;
              let bottom_bar = Bar.make Horizontal [| Wob.desc bottom_bar |] in
              Wob.desc bottom_bar
            )
      
      else bottom_bar
    in
    
    let left_bar = if !!add_borders then
        match left_bar with
          None -> Some (make_null sw)
        | Some left_bar -> 
            let left_bar = 
              Bar.make Horizontal [| Wob.desc left_bar; make_null sw |] in
            let left_bar = Bar.make Vertical [| Wob.desc left_bar |] in
            Some (Wob.desc left_bar)
      else left_bar
    in
    
    let right_bar = if !!add_borders then
        match right_bar with
          None -> Some (make_null sw)
        | Some right_bar -> 
            let right_bar = 
              Bar.make Horizontal [| Wob.desc right_bar; make_null sw |] in
            let right_bar = Bar.make Vertical [| Wob.desc right_bar |] in
            Some (Wob.desc right_bar)
      else right_bar
    in    
    
    let hooks = [c_hook c; icon_manager_hook c; desk_manager_hook c] in
    (hooks, left_bar, right_bar, top_bar, bottom_bar)
  with 
    e -> 
      Log.exn "Stddeco.generic_window: %s\n" e;
      match deco_name with
        "standard_deco" -> simple_window sw c
      | "no_deco" -> no_deco sw c
      | _ -> simple_window sw c
          
            
let decorate = define_option ["decorate"] ""
    (list_option (tuple2_option (string_option, list_option string2_option)))
  []

let gwml_themes = define_option ["gwml_themes"] "" path_option
    [ "/usr/local/share/GwML" ]

let load_theme name =
  let dirname = Utils.find_in_path !!gwml_themes name in
  let filename = Filename.concat dirname ((Filename.basename name) ^ ".gwmlrc") in
  append filename;
  Array.iter (fun s ->
      let sw = s#wob in
      (* reset menus *)
      (!menu_manager)#reset sw;      
      (* reset the clients *)
      List.iter (fun (c,w) ->
          let tw = w.w_top in
          let focus = Wob.getenv tw focus_var in
          update_focus tw.w_oo focus
      ) (list_clients sw)) !screens
  

let theme_menu w =
  let files = 
    List.flatten (List.map (fun dir -> 
          List.map (fun file ->
              Filename.concat dir file
          ) (Utils.list_dir_normal dir)) !!gwml_themes) in
  let files = List.flatten (List.map (fun file ->
          if Sys.file_exists (Filename.concat file 
              ((Filename.basename file) ^ ".gwmlrc")) then
            [Filename.basename file, file]
          else []
      ) files) in
  List.map (fun (name, file) ->
      name, [] , Function (fun w -> load_theme file) 
  ) files
  
let _ =
  define_action "gwml_themes" (ActiveMenu theme_menu)
  
let set_class_deco name w =
  let c = w.w_oo#client in
  add_decoration (generic_window name) c.c_class;
  decorate =:= List.map (fun (name, list) ->
      name, Utils.list_remove list c.c_class
  ) !!decorate;
  decorate =:= (
    if List.mem_assoc name !!decorate then
      List.map (fun (deco_name, deco_list) ->
          if name = deco_name then 
            deco_name, c.c_class :: deco_list
          else 
            deco_name, deco_list
      ) !!decorate
    else
      (name , [c.c_class]) :: !!decorate)

let deco_menu f w =
  let s1 = "standard_deco" in
  let s2 = "no_deco" in
  (s1, [], Function (f s1)) ::
  (s2, [], Function (f s2)) ::
  (List.map (fun (name, deco) ->
      name, [], Function (f name)
  ) !!decorations)
  
let _ =
  define_action "win_deco_menu" (ActiveMenu (deco_menu set_class_deco))