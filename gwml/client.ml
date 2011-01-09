(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Options
open Gwml_args
open Utils
open Xtypes
open Gwml
open Wob
open XA
  
  (* This file is intented to perform ICCCM compliance (version 2.0) on
  its client window. This compliance can only be achieved if the config
  provided by the user does not break it. *)


let cwx x = CWX x
let cwy x = CWY x
let cwwidth x = CWWidth x
let cwheight x = CWHeight x
let cwborderwidth x = CWBorderWidth x
let cwsibling x = CWSibling x
let cwstackmode x = CWStackMode x

let appendsome f some list =
  match some with
    None -> list
  | Some x -> (f x) :: list

module E = Xconfigurerequest 

let handle_ConfigureRequest sw e =
  let s = sw.w_screen in
  try
    let (c,w) = Wintbl.find s.s_clients e.E.window in
    let g = w.w_geometry in
    (match e.E.width with Some w -> g.width <- w | _ -> ());
    (match e.E.height with Some w -> g.height <- w | _ -> ());
    Wob.send w.w_top WobGetSize
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

let set_shape c w = 
  let module S = Shape in
  S.X.shapeCombine display w.w_window S.Bounding 
    0 0 c.c_window S.Bounding S.Set;
  Wob.send_one w.w_top WobUpdateShape
  
let c_is_shaped c = match c.c_shaped with None -> false | _ -> true
  
let handle_shapeNotify c w ev =
  let module S = Shape in
  let module SE = S.XshapeNotify in
  if ev.SE.shape_kind = S.Bounding && ev.SE.window == c.c_window &&
    c_is_shaped c = ev.SE.shaped then begin
      set_shape c w
    end
  else
    S.X.shapeCombine display w.w_window S.Bounding 0 0 noWindow S.Bounding S.Set
    

let client_xevents c w ev =
  if !!Gwml.debug_events then Log.printf "Client: %s\n" 
      (event_to_string ev);
  match ev.ev_event with
    
    PropertyNotifyEvent e ->
      let prop = e.Xproperty.atom in
      if prop = XA.xa_wm_name then
        c.c_name <- Icccm.getWM_NAME display c.c_window else
      if prop = XA.xa_wm_icon_name then
        c.c_icon_name <- Icccm.getWM_ICON_NAME display c.c_window else
      if prop = XA.xa_wm_hints then 
        c.c_wm_hints <- Icccm.safe_getWM_HINTS display c.c_window  else
      if prop = XA.xa_wm_normal_hints then
        c.c_size_hints <- Icccm.safe_getWM_SIZE_HINTS display c.c_window else
      if prop = xa_wm_protocols then
        begin
          let protocols = Icccm.getWM_PROTOCOLS display c.c_window in
          c.c_take_focus <- List.memq xa_wm_take_focus protocols;
          c.c_delete_window <- List.memq xa_wm_delete_window protocols;
        end else
      if prop = xa_wm_colormap_windows then 
        c.c_colormap_windows <- 
          Icccm.getWM_COLORMAP_WINDOWS display c.c_window else
      if prop = xa_wm_state then ();
      Wob.send w.w_top (WobPropertyChange prop);
      wm_broadcast (c, w, ClientPropertyChange prop)
  
  | ClientMessageEvent e ->
      if e.Xclient.datatype = xa_wm_change_state && 
        (Xbuffer.getEnum8 e.Xclient.data 0) = IconicState then
        Wob.send w.w_top (WobIconifyRequest false);
  
  | ColormapNotifyEvent e ->
      let s = w.w_screen in
      (match e.Xcolormap.newp, e.Xcolormap.state with
          true, _ -> (* this window changed its colormap *)
            c.c_colormap <- e.Xcolormap.colormap;
            if w == s.s_cmap_wob then
              if e.Xcolormap.colormap = noColormap then
                Wob.send w.w_top.w_parent (WobInstallColormap false)
              else
                Wob.send w (WobInstallColormap true)
        | false, ColormapInstalled -> (* a new colormap was installed *)
            ()
        | false, ColormapUninstalled -> (* a colormap was uninstalled *)
            if s.s_last_cmap = e.Xcolormap.colormap then ()
      )     
  
  | DestroyNotifyEvent e -> Wob.send w.w_top WobDestroy;
  
  | FocusInEvent _ -> 
      let s = w.w_screen in s.s_focus_wob <- w;
      wm_broadcast (c, w,ClientFocus true);
      Wob.send w.w_top (WobClientFocus true);
      
  | FocusOutEvent _ -> 
      let s = w.w_screen in
      if s.s_focus_wob == w then s.s_focus_wob <- w.w_top.w_parent;
      wm_broadcast (c, w,ClientFocus false);
      Wob.send w.w_top (WobClientFocus false)

  | CoreEvent e -> 
      let ev = Shape.to_shapeEvent display e in
      handle_shapeNotify c w ev
      
  | _ -> ()
      
let parent_events c w ev =
  if !!debug_events then 
    Log.printf "Client parent: %s\n" (event_to_string ev);
  match ev.ev_event with
  | UnmapNotifyEvent e ->
(*
ICCCM, 4.1.4
                   Advice to Implementors

     For compatibility with obsolete clients, window
     managers should trigger the transition to the
     Withdrawn state on the real UnmapNotify rather
     than waiting for the synthetic one.  They should
     also trigger the transition if they receive a syn-
     thetic UnmapNotify on a window for which they have
     not yet received a real UnmapNotify.
      *)
      
      let win = e.Xunmap.window in
      let (c,w) = Wintbl.find w.w_screen.s_clients win in
      if win = c.c_window then
        let tw = w.w_top in
        ignore (X.getInputFocus display);
        Wob.send tw (match Xlib.checkPredEvent display (
              fun e -> match e.ev_event with
                  DestroyNotifyEvent e when e.Xdestroywindow.window = c.c_window
                  
                  -> true
                | _ -> false) true with
            None -> 
              WobWithdrawn
          | Some _ -> WobDestroy)
  
  | ConfigureRequestEvent e ->
      handle_ConfigureRequest w e
  
  | ButtonPressEvent _ ->
      let tw = w.w_top in 
      Wob.send_one tw WobClickInClient;
      
  | ButtonReleaseEvent _ ->
      X.allowEvents display ReplayPointer currentTime
  | _ -> ()

module F = Xconfigure

let sendSyntheticConf c w =
  let s = w.w_screen in
  let g = w.w_geometry in
  let xx,yy = root_position w in
  X.sendEvent display c.c_window false [StructureNotifyMask] (
    ConfigureNotifyEvent {
      F.event = c.c_window;
      F.window = c.c_window;
      F.x = xx;
      F.y = yy;
      F.width = max 1 g.width;
      F.height = max 1 g.height;
      F.border_width = 0;
      F.above_sibling = noWindow;
      F.override_redirect = false;
    })

let allow_withdrawn_state = define_option ["allow_withdrawn_state"] ""
    bool_option false

let window_borderwidth = 
  define_option ["window_borderwidth"] 
  "<window_borderwidth> is the border width of the internal frame of
  a client window (ie between the client window and the decoration)." 
  int_option 1

let window_borderpixel = 
  define_option ["window_borderpixel"] "" color_option "slategrey"

let test_grab = define_option ["test_grab"] "" bool_option false

class client c =
  object (self)
    inherit wob_base
    
    val c = c
    
    method is_shaped = c_is_shaped c
    method first_hook e =
      let w = self#wob in
      match e with
      | WobInit ->
          let g = w.w_geometry in
          g.width <- c.c_geometry.width;
          g.height <- c.c_geometry.height;
          g.border <- !!window_borderwidth
      | WobGetSize -> ()      
      | WobMove -> 
          wm_broadcast (c, w,ClientMove);
          sendSyntheticConf c w
      
      | WobResize force ->
          begin          
            let do_resize = 
              if not force then begin
      (* check that the current size corresponds to hints *)
                  let nh = c.c_size_hints in
                  let min_width, min_height = match nh.min_size with
                      None -> 0,0
                    | Some (w,h) -> w, h in
                  let inc_x,inc_y = match nh.resize_inc with
                      None -> 1,1
                    | Some (w,h) -> w,h in      
                  let r = w.w_geometry in
                  let new_w =
                    if r.width < min_width then min_width
                    else
                    if inc_x <> 0 then
                      ((r.width - min_width) / inc_x) * inc_x + min_width
                    else
                      r.width
                  in
                  let new_h =
                    if r.height < min_height then min_height else
                    if inc_y <> 0 then
                      ((r.height - min_height) / inc_y) * inc_y + min_height
                    else
                      r.height
                  in
                  if (new_w <> r.width || new_h <> r.height) && not force then
                    begin
                      r.width <- new_w;
                      r.height <- new_h;
                      Wob.send w.w_top WobGetSize;
                      Wob.send w.w_top (WobResize true);
                      false
                    end
                  else
                    true
                end
              else true
            in
            if do_resize then
              let g = w.w_geometry in
              let s = w.w_screen in
              Xlib.moveResizeWindow display c.c_window 0 0 
                (max 1 g.width) (max 1 g.height); 
              if w.w_window <> noWindow then
                Xlib.moveResizeWindow display w.w_window g.x g.y
                  (max 1 g.width) (max 1 g.height);
              wm_broadcast (c, w,ClientResize);
          end;
      
      | WobCreate ->
          begin
            Log.catch "Client: WobCreate %s " (fun _ ->
                let s = w.w_screen in
                let sw = w.w_top.w_parent in
                w.w_window <- c.c_window;
                let g = w.w_geometry in      
                let tg = w.w_top.w_geometry in
                w.w_window <- X.createWindow display w.w_parent.w_window
                  g.x g.y g.width g.height
                  copyDepthFromParent InputOutput copyVisualFromParent 
                  !!window_borderwidth
                  [CWEventMask 
                    (SubstructureNotifyMask:: 
                    SubstructureRedirectMask ::(self#mask));
                  CWOverrideRedirect true;
                  CWBorderPixel (color_make sw !!window_borderpixel);
                ];
                
                Xsync.reparentWindow display c.c_window w.w_window 0 0;
                X.configureWindow display c.c_window [CWBorderWidth 0];
                if !!test_grab then
                  X.grabButton display w.w_window true [ButtonPressMask] 
                    GrabModeSync GrabModeAsync noWindow noCursor
                    anyButton anyModifier ;
                if c_is_shaped c then set_shape c w;
                Wintbl.add s.s_clients c.c_window (c,w);
                Wintbl.add s.s_clients w.w_top.w_window (c,w);
                Wintbl.add s.s_clients w.w_window (c,w);
      (* We must get the UnmapNotifyEvent now, else we could believe
      that the client has unmap is window, thus going in WithDrawn state *)
                let _ = Xlib.checkPredEvent display (
                    fun e -> 
                      match e.ev_event with
                        UnmapNotifyEvent e when
                        e.Xunmap.window = w.w_window -> true
                      | _ -> false) true in
                let _ = Xlib.checkPredEvent display (
                    fun e -> 
                      match e.ev_event with
                        UnmapNotifyEvent e when 
                        e.Xunmap.window = w.w_window -> true
                      | _ -> false) true in
                
                Eloop.add_window s.s_scheduler w.w_window (
                  parent_events c w);
                Eloop.add_window s.s_scheduler c.c_window (
                  client_xevents c w);
        (* We must take care of our own windows !! *)
                if not c.c_own_window then begin
                    X.changeWindowAttributes display c.c_window  [
                      CWEventMask Top.client_mask];
                  end;
                X.mapWindow display c.c_window;      
                X.mapWindow display w.w_window;      
                Wob.send w.w_top (WobResize false);
                Wob.send w.w_top WobMove;
            );
            begin
              try
                ignore (X.getGeometry display c.c_window)
              with _ ->
            (* Error with simple request to client !! *)
                  Wob.send w.w_top WobDestroy
            end        
          end
      
      | WobDestroy ->
          wm_broadcast (c, w,RemoveClient);
          let s = w.w_screen in
          Wintbl.remove s.s_clients c.c_window;
          Wintbl.remove s.s_clients w.w_window;
          Wintbl.remove s.s_clients w.w_top.w_window;
          Eloop.remove_window s.s_scheduler c.c_window
      
      | WobMap ->
          X.mapWindow display w.w_window;
          if c.c_wm_state = IconicState then wm_broadcast (c, w,ClientDeiconify)
          else wm_broadcast (c, w,ClientMap);
          Icccm.setWM_STATE display c.c_window (NormalState,c.c_wm_icon);
          c.c_wm_state <- NormalState;
          sendSyntheticConf c w;
          wm_broadcast (c,w,ClientMap)
      
      | WobUnmap iconified -> 
          if iconified then wm_broadcast (c, w,ClientIconify)
          else wm_broadcast (c, w,ClientUnmap);
          X.unmapWindow display w.w_window;
          if iconified && c.c_wm_state <> IconicState then begin
              c.c_wm_state <- IconicState;
              Icccm.setWM_STATE display c.c_window (c.c_wm_state,c.c_wm_icon);
            end else begin
            (* this is particularly dangerous, since the wm may be aborted
          , leaving some clients in WithDrawn state !!! *)
              c.c_wm_state <- WithdrawnState;
              let state = 
                if !!allow_withdrawn_state then WithdrawnState else
                  IconicState in
              Icccm.setWM_STATE display c.c_window (state,c.c_wm_icon);
            end
      
      | WobExitGwml ->
          if c.c_wm_state = WithdrawnState then
            Icccm.setWM_STATE display c.c_window (NormalState,c.c_wm_icon);
      
      | WobWithdrawn ->
          let s = w.w_screen in
        (* We must check if the window has not been reparented before reparenting it to the screen *)
          
          
          let qt = X.queryTree display c.c_window in
          if qt.qt_parent == w.w_window then
            begin
              X.reparentWindow display c.c_window s.s_scr.scr_root
                w.w_top.w_geometry.x w.w_top.w_geometry.y;
              X.unmapWindow display c.c_window;
              Icccm.setWM_STATE display c.c_window (WithdrawnState,noWindow)
            end;
          Wob.send w.w_top WobDestroy
      
      | WobSetInputFocus force ->
          if Wob.getenv w.w_top is_mapped then 
            if force || (c.c_set_focus && c.c_wm_state = NormalState) then
              X.setInputFocus display c.c_window RevertToPointerRoot
                !Eloop.event_time
            else
            if c.c_take_focus then
              X.sendEvent display c.c_window false [] 
                (ClientMessageEvent {
                  Xclient.format = 4;
                  Xclient.window = c.c_window;
                  Xclient.datatype = xa_wm_protocols;
                  Xclient.data = (let s = String.create 20 in
                    Xbuffer.setEnum32 s 0 xa_wm_take_focus;
                    Xbuffer.setTime s 4 !Eloop.event_time;
                    s) 
                })
            else  () (* do not give focus to no-focus windows *)
      
      | WobDeleteWindow ->
          if c.c_delete_window then
            X.sendEvent display c.c_window false [] 
              (ClientMessageEvent {
                Xclient.format = 4;
                Xclient.window = c.c_window;
                Xclient.datatype = xa_wm_protocols;
                Xclient.data = (let s = String.create 20 in
                  Xbuffer.setEnum32 s 0 xa_wm_delete_window;
                  Xbuffer.setTime s 4 !Eloop.event_time;
                  s) 
              });
      
      | WobInstallColormap reinstall ->
          let s = w.w_screen in
          if reinstall || not (s.s_cmap_wob == w) then
            let ok = ref false in
            List.iter (fun win ->
                if win = c.c_window then ok := true;
                let gwa = X.getWindowAttributes display win in
                if s.s_last_cmap <> gwa.gwa_colormap then
                  (X.installColormap display gwa.gwa_colormap;
                    s.s_last_cmap <- gwa.gwa_colormap)
            ) (List.rev c.c_colormap_windows);
            if not !ok then
              (let cmap = if c.c_colormap = noColormap then
                    s.s_scr.scr_default_colormap
                  else c.c_colormap
                in
                if s.s_last_cmap <> cmap then
                  (X.installColormap display cmap;
                    s.s_last_cmap <- cmap));
            wm_broadcast (c,w, ClientColormap true);
            s.s_cmap_wob <- w
      
      | WobPropertyChange atom when atom == xa_wm_colormap_windows ->
      (* if WM_COLORMAP_WINDOWS changes and the current window has
  the colormap focus then re-install the colormaps *)
          let s = w.w_screen in
          if s.s_cmap_wob == w then
            Wob.send w (WobInstallColormap true)
      
      | _ -> ()
          
  method client = c
end

let last_hook w e = ()

let make c = new client c
  
let create sw window new_window =
  let gwa = X.getWindowAttributes display window in
  let wh = Icccm.safe_getWM_HINTS display window in
  let state = 
    match wh.initial_state with
      None -> NormalState | Some state -> state in
  let state =        
    try
      fst (Icccm.getWM_STATE display window)
    with
      _ -> state in
  if gwa.gwa_override_redirect then
    (if !!debug then 
        Log.printf "%s: Ignore Override-redirect window\n" "Client.create";
      raise Not_found);
  let state = if state = WithdrawnState && !mapall then NormalState else state
  in
  if (not !mapall) && (not new_window) &&  (gwa.gwa_map_state = IsUnmapped) then
    (if !!debug then Log.printf "%s: Ignore Unmapped window\n" "Client.create";
      Icccm.setWM_STATE display window (WithdrawnState,noWindow);
      raise Not_found);
  if (not !mapall) && (not new_window) &&  (state = WithdrawnState) then
    (if !!debug then Log.printf "%s: Ignore Withdrawn window\n" "Client.create";
      if gwa.gwa_map_state <> IsUnmapped then
        X.unmapWindow display window;
      raise Not_found);
  let gg = X.getGeometry display window in
  let transient = 
    (try Icccm.getWM_TRANSIENT_FOR display window 
      with _ -> noWindow) in
  let wm_name =
    try Icccm.getWM_NAME display window 
    with Not_found -> "window"
  in
  let protocols = 
    (try Icccm.getWM_PROTOCOLS display window with Not_found -> []) in
  
  let shaped = if use_shape then begin
        let module S = Shape in
        S.X.shapeSelectInput display window true;
        let sqe = S.X.shapeQueryExtents display window in
        if sqe.S.sqe_bounding || sqe.S.sqe_clip then Some sqe else None
      end
    else None in
  let s = sw.w_screen in
  let c = {
      c_window = window;
      c_screen = s;
      (* server info *)
      c_set_focus = (match wh.input with None -> true | Some i -> i);
      c_colormap = (if gwa.gwa_colormap <> s.s_scr.scr_default_colormap then
          gwa.gwa_colormap else noColormap);
      c_new_window = new_window;
      (* properties *)
      c_name = wm_name;
      c_icon_name = 
      (try Icccm.getWM_ICON_NAME display window 
        with _ -> wm_name);
      c_machine = 
      (try Icccm.getWM_CLIENT_MACHINE display window
        with _ -> "machine");
      c_class = (match 
          (try Icccm.getWM_CLASS display window
            with _ -> ["Any";""])
        with
          [c] -> (c,"")
        | c :: i :: _ -> (c,i)
        | [] -> ("Any","")
      );
      c_size_hints = Icccm.safe_getWM_SIZE_HINTS display window;
      c_wm_hints = wh;
      c_transient_for = transient;
      c_colormap_windows = (try
          Icccm.getWM_COLORMAP_WINDOWS display window
        with _ -> []);
      c_icon_size = (try Icccm.getWM_ICON_SIZE display window
        with _ -> []);
      c_wm_state = state;
      c_wm_icon = noWindow;
      c_geometry = { x = gg.gg_x; y=gg.gg_y; 
        width = gg.gg_width; height = gg.gg_height;
        border = 0;
      };
      c_take_focus = List.memq xa_wm_take_focus protocols;
      c_delete_window = List.memq xa_wm_delete_window protocols;
      c_decorated = false;
      c_protocols = protocols;
      c_own_window = Eloop.known_window s.s_scheduler window;
      c_shaped = shaped;
    } in
  if not c.c_own_window then
    X.changeSaveSet display c.c_window InsertResource;
  let c_wob = make c in
  (* There is a BUG if the default config file cannot be loaded (stddeco.cmo)
  because no decoration is available for windows *)
  if c.c_wm_state = WithdrawnState then c.c_wm_state <- NormalState;
  Utils.catchexn "Decorate client:"  (fun () -> 
      !decorate_client sw c_wob);
  if c.c_wm_state = NormalState then
    begin
      X.mapWindow display c.c_window;
    end;
  c.c_new_window <- false;
  c
  
