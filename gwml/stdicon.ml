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

(* Simple icons for the standard configuration *)

open Options
open Xtypes
open Xlib
open Gwml
open Stdconfig
    
let x_pos = define_option ["icon_box_x"] "" int_option 410
let y_pos = define_option ["icon_box_y"] "" int_option 0
let max_width = define_option ["icon_max_width"] "" int_option 65
let max_height = define_option ["icon_max_height"] "" int_option 65
let row_size = define_option ["icon_box_cols"] "" int_option 7
let col_size = define_option ["icon_box_lines"] "" int_option 2
let min_icon_size = 50
let deiconify_in_vscreen = 
  define_option ["deiconify_in_vscreen"] "" bool_option true

let icon_pixmap_var, get_icon_pixmap, set_icon_pixmap,
  icon_pixmap_list = define_class_option  "icon_pixmap_list" 
  "<icon_pixmap_list> is a list of (pixmap filename) * (client pair list)
  specifying the pixmap associated with each client."
  filename_option 
    ["xterm.xpm", ["",""] ] 
  
let icon_pos = Wobenv.new_var ()

let set_icon_pos (w : Gwml.wob) x y = 
  Wob.setenv w icon_pos (x,y)
  
let get_icon_pos w = 
  Wob.getenv w icon_pos
let remove_icon_pos w = Wob.remenv w icon_pos
  
let min_size w e =
  match e with
    WobGetSize -> 
      let w = w#wob in
      w.w_geometry.width <- min_icon_size;
      w.w_geometry.height <- min_icon_size;
  | _ -> ()

let icons = Array.init 3 (fun _ -> Array.create 10 None)

let find_free_slot () =
  let xx = ref 0 in
  let yy = ref 0 in
  try
    for y = 0 to !!col_size do
      for x = 0 to !!row_size - 1 do
        match icons.(y).(x) with
          None -> xx := x; yy := y; raise Exit
        | Some _ -> ()
      done
    done;
    raise Not_found
  with
    Exit -> !xx,!yy

let default_place_icon tw w =
  try
    get_icon_pos tw
  with _ ->
      try
        let (x,y) = find_free_slot () in
        icons.(y).(x) <- Some tw;
        let x = !!x_pos + x* !!max_width in
        let y = !!y_pos + y* !!max_height in
        set_icon_pos tw x y;
        x,y  
      with _ -> 
          (* instead of this, we could simply ask the user to place the icon *)
          0,0
          
let default_remove_icon tw w =
  for y = 0 to !!col_size do
    for x = 0 to !!row_size do
      match icons.(y).(x) with
      | Some ww when tw == ww -> 
          icons.(y).(x) <- None;
          remove_icon_pos tw;
      | _ -> ()
    done
  done
  
let place_icon = ref default_place_icon
let remove_icon = ref default_remove_icon

let icon_actions = define_option ["icon_actions"] "" 
    (list_option binding_option) [
    DblClick(1,0), NamedAction "icon_deiconify", true;
    Button(2,0), NamedAction "icon_deiconify", true;
    Button(1,0), NamedAction "icon_move", true;
    Button(3,0), NamedAction "icon_goto", true;
  ]
  
let icon_client = Wobenv.new_var ()

let auto_raise_icons = define_option ["auto_raise_icons"] "" bool_option true
  
let _ =
  define_action "icon_deiconify" (Function (fun w ->
      let tw = Wob.getenv w.w_top icon_client in
      if !!deiconify_in_vscreen then
        (!virtual_manager)#place_on_screen tw;
      Wob.send tw (WobDeiconifyRequest true)
  ));
  define_action "icon_move" (Function (fun w ->
        let tw = Wob.getenv w.w_top icon_client in
        if not !!auto_raise_icons then Wob.send tw WobRaiseWindow;
        User.move w true;
        !remove_icon tw w;
        set_icon_pos tw w.w_top.w_geometry.x w.w_top.w_geometry.y
    ));
  define_action "icon_goto" (Function (fun w ->
      let tw = Wob.getenv w.w_top icon_client in
      Wob.send tw (WobDeiconifyRequest true);
      (!virtual_manager)#goto tw  
    ))

let apply_icon args =
  let name = match args with name :: _ -> name | _ -> raise Not_found in
  Function (fun w ->
      let tw = Wob.getenv w.w_top icon_client in
      execute_action (NamedAction name) tw
  )
  
let icon_hook tw c w e = 
  let w = w#wob in
  match e with
    WobMapRequest ->
      let (xx,yy) = !place_icon tw w in
      let g = w.w_geometry in
      g.x <- xx;
      g.y <- yy;
      Wob.send w WobMap
  | WobUnmap true -> !remove_icon tw w
  | WobUnmap false -> ()
  | WobDestroy -> !remove_icon tw w
  | WobEnter -> 
      if !!auto_raise_icons then
        Wob.send w.w_top WobRaiseWindow
  | WobCreate -> 
      c.c_wm_icon <- w.w_window;
      Wob.setenv w.w_top icon_client tw
  | _ -> ()
      
let create_icon_pixmap c tw =
  let sw = tw.w_top.w_parent in
  let descr =
    try 
      FromFile (get_icon_pixmap c)
    with _ -> try
          FromFile (find_pixmap(find_icon c))
    with _ -> try
          (FromFile (find_pixmap
                (Xrm.get x_res
                  ((fst c.c_class):: (snd c.c_class) :: ["icon_pixmap"]))))
        with _ -> try
              (FromFile 
                  (find_pixmap
                    (Printf.sprintf "%s.xpm"
                      (String.lowercase (fst c.c_class)))))
            with _ ->
                let pix =  c.c_wm_hints.icon_pixmap in
                if pix <> noWindow then
                  let mask = c.c_wm_hints.icon_mask in
                  (FromFunction ((fst c.c_class)^(snd c.c_class),
                      fun _ -> (pix,mask)))
                else
                  raise Not_found
  in
  Pixmap.make sw descr
  
let icon_make c tw =
  let sw = tw.w_top.w_parent in
  let pixmap = 
    try
      let pixmap = try
          try create_icon_pixmap c tw with e ->
          (* If we are in a group, use the same icon kind as the master *)
              let group = c.c_wm_hints.window_group in
              if group <> noWindow then 
                let s = sw.w_screen in
                let (c,w) = Wintbl.find s.s_clients group in
                create_icon_pixmap c w
              else raise e
        with e -> 
            let pixmap = (FromFile (find_pixmap "xterm.xpm")) in
            Pixmap.make sw pixmap
      in
      pixmap#set_shaped true;
      Wob.desc pixmap
    with
      _ -> 
        let null = Null.make_wob () in
        null#add_hook (min_size null);
        null
  in
  let label = 
    let string = c.c_icon_name in
    Label.make string in
  tw.w_oo#add_hook (fun e ->
      match e with
        WobPropertyChange p when p = XA.xa_wm_icon_name -> 
          label#set_string c.c_icon_name
      | _ -> ());
  let top = Top.make sw "" [icon_hook tw c] 
      (Wob.desc pixmap) None None None (Some (label :> wob_desc)) in 
  top#set_borderwidth !!icon_borderwidth;
  pixmap#set_background !!icon_background;
  label#set_background !!icon_background;
  label#set_font !!icon_font;
  label#set_foreground !!icon_foreground;
  top#set_background !!icon_background;
  top#set_actions (convert_bindings !!icon_actions);
  top
  