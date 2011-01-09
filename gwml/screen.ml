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
open Gwml_args
open Xtypes
open Gwml
open Wob


let appendsome f some list =
  match some with
    None -> list
  | Some x -> (f x) :: list
      
let cwx x = CWX x
let cwy x = CWY x
let cwwidth x = CWWidth x
let cwheight x = CWHeight x
let cwborderwidth x = CWBorderWidth x
let cwsibling x = CWSibling x
let cwstackmode x = CWStackMode x

module E = Xconfigurerequest 
  
let handle_ConfigureRequest sw e =
  let s = sw.w_screen in
  try
    let (c,w) = Wintbl.find s.s_clients e.E.window in
    let g = w.w_geometry in
    (match e.E.width with Some w -> g.width <- w | _ -> ());
    (match e.E.height with Some w -> g.height <- w | _ -> ());
    Wob.send w.w_top WobGetSize;
    Wob.send w.w_top (WobResize false)
  with
    Not_found ->
      X.configureWindow display e.E.window 
        (appendsome cwx e.E.x
          (appendsome cwy e.E.y
            (appendsome cwwidth e.E.width
              (appendsome cwheight e.E.height
                (appendsome cwborderwidth e.E.border_width 
                  (appendsome cwsibling e.E.sibling
                    (appendsome cwstackmode e.E.stack_mode
                      [])))))))    

let handle_ColormapNotify sw e =
  let s = sw.w_screen in
  match e.Xcolormap.newp, e.Xcolormap.state with
    true, _ -> (* this window changed its colormap *) ()
  | false, ColormapInstalled -> (* a new colormap was installed *) ()
  | false, ColormapUninstalled -> (* a colormap was uninstalled *)
      if s.s_last_cmap = e.Xcolormap.colormap then 
        Wob.send s.s_cmap_wob (WobInstallColormap true)
      
let xevents sw ev = 
  let s = sw.w_screen in
  if !!debug_events then (Log.printf "Root: %s\n" 
        (event_to_string ev));
  match ev.ev_event with
  | DestroyNotifyEvent e ->
      begin
        try
          let (c,w) = Wintbl.find s.s_clients e.Xdestroywindow.window in
          Wob.send w.w_top WobDestroy;
        with
          Not_found -> ()
      end
  | MapRequestEvent e ->
      begin
        if !!debug then   
          ( Log.printf "Map request for %d\n"
            (window_to_id e.Xmaprequest.window));
        try
          let (c,w) = Wintbl.find s.s_clients e.Xmaprequest.window in
          Wob.send w.w_top (WobDeiconifyRequest false);
          X.mapWindow display e.Xmaprequest.window      
        with
          _ ->
            if !!debug then
              Log.printf "%s: Decorate new client\n" "Screen.xevents";
            let c = Log.watch "Screen.xevents: %s in Client.create" 
                (fun _ -> Client.create sw e.Xmaprequest.window true) in
            let (c,w) = Wintbl.find s.s_clients c.c_window in              
            wm_broadcast (c,w,AddClient);
            if c.c_wm_state <> IconicState then
              X.mapWindow display e.Xmaprequest.window      
      end;

  | ConfigureRequestEvent e ->
      handle_ConfigureRequest sw e
  | ButtonPressEvent e -> 
      Wob.send sw (WobButtonPress e)
  | KeyPressEvent e -> 
      let modifiers = e.Xkey.state in
      let (str,keysym,_) = KeyBind.lookupString display ev.ev_event in
      Wob.send sw (WobKeyPress (e,str,keysym))
      
  | UnmapNotifyEvent e when ev.ev_sent  ->
      Log.printf "UNMAP%s\n" "";
      let win = e.Xunmap.window in
      let (c,w) = Wintbl.find s.s_clients win in
      if win = c.c_window then
        let tw = w.w_top in
        Wob.send tw WobWithdrawn
  | ColormapNotifyEvent e ->
      handle_ColormapNotify sw e
  | _ -> ()

class screen =
  object (self)
  inherit wob_base
  method first_hook e = 
    let w = self#wob in
    match e with
      WobInstallColormap force  ->
        let s = w.w_screen in      
        if force || not (s.s_cmap_wob == w) then
          let scr = s.s_scr in
          s.s_cmap_wob <- w;
          s.s_last_cmap <- scr.scr_default_colormap;
          X.installColormap display scr.scr_default_colormap
    | WobKeyPress (e,s,k) -> self#handle_key (e,s,k)
    | WobButtonPress e -> self#handle_button e
    | _ -> ()

  method client = raise Not_found
  method iter f = failwith "screen#iter not implemented"
end

let last_hook w e = ()

let make scr ddisp i =
  let desc = new screen in
  let rec s = {
      s_clients = Wintbl.create 37;
      s_scr = scr;
      s_colors = Hashtbl.create 37;
      s_fonts = Hashtbl.create 37;
      s_pixmaps = Hashtbl.create 37;
      s_cursors = Hashtbl.create 37;
      s_scheduler = ddisp;                            
      s_last_cmap = scr.scr_default_colormap;
      s_cmap_wob = sw;
      s_focus_wob = sw;
      s_top_opening_hooks = [];
    } 
  and sw = {
      w_window = scr.scr_root;
      w_parent = sw;
      w_top = sw;
      w_screen = s;
      w_geometry = { x=0; y=0; width = scr.scr_width; 
        height = scr.scr_height; border = 0; };
      w_env = Wobenv.empty ();
      w_oo = (desc :> Gwml.wob_desc);
      w_queue = [];
    }
  in
  desc#set_wob sw;
  Eloop.add_window ddisp sw.w_window (xevents sw);
  if !batch_mode then
    begin
      exec_hooks sw !screen_opening_hooks;      
      desc
    end
  else
  try
    let rec iter () =
      let jeton = Xlib.selectInput display sw.w_window
          [StructureNotifyMask;
          SubstructureNotifyMask;
          SubstructureRedirectMask;
          ButtonPressMask;
          KeyPressMask;
          ColormapChangeMask;
        ] in
      let _ = X.getInputFocus display in
      if !retry then
        try
          Jeton.wait jeton
        with
          _ -> Unix.sleep 1; iter ()
      else
        Jeton.wait jeton
    in
    iter ();
    desc#set_mask [StructureNotifyMask;
          SubstructureNotifyMask;
          SubstructureRedirectMask;
          ButtonPressMask;
          KeyPressMask;
          ColormapChangeMask];

    (* Reinstall default colormap *)
    X.installColormap display scr.scr_default_colormap;

    (* exec configuration hooks *)
    exec_hooks sw !screen_opening_hooks;
    Utils.catchexn "Decorate screen:" (fun () -> !decorate_screen sw);
    
    (* decorate all existing windows *)
    let qt = X.queryTree display sw.w_window in
    let clients = ref [] in
    List.iter (fun window ->
        try
          let c = Client.create sw window false in
          let (c,w) = Wintbl.find s.s_clients c.c_window in
          wm_broadcast (c, w,AddClient);
        with _ -> ()
    ) qt.qt_subwindows;
    desc
  with
    e -> 
      Printf.printf "GWML error: Can not get control of screen %d\n(Maybe another Window-manager is running)" i; 
      print_newline ();
      exit 2
      
      