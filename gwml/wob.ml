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

open Xtypes
open Gwml
open Options

let backing_store = define_option ["backing_store"] "" bool_option true
  
              
let remove_expose ev =
  let win = ev.ev_window in
  Xlib.removePredEvent display (fun ev' ->
      match ev'.ev_event with
        ExposeEvent _ when ev'.ev_window = win -> true
      | _ -> false)

  (*
  KeyPressMask; KeyReleaseMask;
  ButtonPressMask; ButtonReleaseMask;
  *)
let std_mask = 
  [
    EnterWindowMask;LeaveWindowMask;
    ExposureMask; 
    FocusChangeMask;
    ]

let not_grab_related mode = 
  mode <> NotifyGrab && mode <> NotifyUngrab && mode <> NotifyWhileGrabbed

let send w e = 
  let desc = w.w_oo in
  (try desc#first_hook e with _ -> ());
  List.iter (fun hook -> try hook e with _ -> ()) desc#wob_hooks;
  (try desc#last_hook e with _ -> ())

let wob_queue = ref []
let send_one w e =
  if not (List.memq e w.w_queue) then
    (w.w_queue <- e :: w.w_queue;
      wob_queue := w :: !wob_queue)

let after_event_list = ref []
let after_event f =
  if not (List.memq f !after_event_list) then
    after_event_list := f :: !after_event_list

let after_events_list = ref []
let after_events f =
  if not (List.memq f !after_events_list) then
    after_events_list := f :: !after_events_list

    (* Process them in reverse order *)
let rec end_hook () = 
  match !wob_queue with
    [] -> iter_list ()
  | w :: tail ->
      match w.w_queue with
        [] -> 
          wob_queue := tail;
          end_hook ()
      | e :: tail ->
          w.w_queue <- tail;
          end_hook ();
          (try send w e with _ -> ())

and iter_list () =
  match !after_event_list with
    [] -> ()
  | f :: tail ->
      after_event_list := tail;
      (try f () with _ -> ());
      iter_list ()

let rec iter_list2 () =
  match !after_events_list with
    [] -> ()
  | f :: tail ->
      after_events_list := tail;
      (try f () with _ -> ());
      iter_list2 ()

let optimize_events = define_option ["optimize_events"] "" bool_option false
      
let _ = 
  (if !!optimize_events then
    Eloop.add_after_events_hook
  else 
    Eloop.add_after_event_hook) end_hook;
  Eloop.add_after_events_hook iter_list2
(*          
type wob_info = {
    mutable actions : bindings;
    mutable background : string;
    mutable min_width : int;
    mutable min_height : int;
    mutable mask : eventMask list;
    mutable wob_hooks : (wob_event -> unit) list;
    mutable borderwidth : int;
    mutable borderpixel : string;
    mutable cursor : cursor_desc option;
    }
    *)  

let default_borderpixel = define_option ["borderpixel"] "" 
  color_option "gray51"
let default_background = define_option ["default_background"] "" 
  color_option "#c0c0c0"
let default_foreground = define_option ["default_foreground"] "" 
  color_option "white"
let default_font = define_option ["default_font"] "" 
    font_option "fixed"
  
let double_click_delay = define_option ["double_click_delay"] 
  "<double_click_delay> is the minimal delay between two button clicks
  for it to be viewed as one double-click and not two single clicks
  (in seconds)." 
  float_option 0.250
let delta_move_size = define_option ["delta_move_size"] 
  "<delta_move_size> is the minimal offset the mouse must have moved
  after a click during the <delta_move_delay> time for the event to be
  viewed as a DeltaMove." 
  int_option 1
let delta_move_delay = define_option ["delta_move_delay"] 
    "<delta_move_delay> is the delay (in seconds) used for DeltaMove." 
  float_option 0.1

let last_click = ref
    {
    Xbutton.detail = 0;
    Xbutton.time = currentTime;
    Xbutton.root = noWindow;
    Xbutton.event = noWindow;
    Xbutton.child = noWindow;
    Xbutton.x_event = 0;
    Xbutton.y_event = 0;
    Xbutton.x_root = 0;
    Xbutton.y_root = 0;
    Xbutton.state = 0;
    Xbutton.same_screen = false;
  }
  
let click s =
  try
    let e1 = match Eloop.last_event s.s_scheduler with
        ButtonPressEvent e -> e
      | _ -> 
          raise Exit
    in
    let e2 = !last_click in
    if e2.Xbutton.detail = e1.Xbutton.detail &&
      e2.Xbutton.event = e1.Xbutton.event &&
      (real_time e1.Xbutton.time) -. (real_time e2.Xbutton.time)
      < 1000. *. !!double_click_delay
    then Double else begin
        ignore (Concur.ThreadUnix.select [] [] [] !!delta_move_delay);
        let qp = X.queryPointer display e1.Xbutton.root in
        let old_x = e1.Xbutton.x_root in
        let old_y = e1.Xbutton.y_root in
        if abs(qp.qp_root_x - old_x) >= !!delta_move_size ||
          abs(qp.qp_root_y - old_y) >= !!delta_move_size then 
          DeltaMove else Simple
      end
  with  _ -> Other
      
let set_grabs display win list =
  List.iter (fun (g,_,bool) ->
      if bool then
        try
          match g with
            Key (keysym,modifiers) ->
              let keycode,_ = KeyBind.keysymToKeycode display keysym in
              X.grabKey display win false
                GrabModeAsync GrabModeAsync keycode modifiers;
          | Button (button,modifiers) ->
              X.grabButton display win false []
                GrabModeAsync GrabModeAsync noConfineTo noCursor 
                button modifiers;
          | DblClick (button,modifiers) ->
              X.grabButton display win false []
                GrabModeAsync GrabModeAsync noConfineTo noCursor 
                button modifiers;
          | BMove (button,modifiers) ->
              X.grabButton display win false []
                GrabModeAsync GrabModeAsync noConfineTo noCursor 
                button modifiers;
        with _ -> ()
  ) list
  
let unset_grabs display win list =
  List.iter (fun (g,_,bool) ->
      if bool then
        try
          match g with
            Key (keysym,modifiers) ->
              let keycode,_ = KeyBind.keysymToKeycode display keysym in
              X.ungrabKey display win keycode modifiers;
          | Button (button,modifiers) ->
              X.ungrabButton display win button modifiers;
          | DblClick (button,modifiers) ->
              X.ungrabButton display win button modifiers;
          | BMove (button,modifiers) ->
              X.ungrabButton display win button modifiers;
        with _ -> ()
  ) list


let images = Hashtbl.create 31

let reset_image_cache () =
  let images_l = ref [] in
  Hashtbl.iter (fun _ (_, im) ->
      if not (List.memq im !images_l) then
        images_l := im :: !images_l) images;
  Hashtbl.clear images;
  List.iter (fun im -> Imager.image_destroy im) !images_l
  
let exit_exn = Exit
let make_backimage image w bg =
  ungrab_exec (fun _ -> 
      try
        if not !!use_imlib then raise exit_exn;
        let filename, dx, dy =
          match image with
            NoImage -> raise exit_exn
          | ScaleImage file -> 
              let g = w.w_geometry in file, g.width, g.height
          | TileImage file -> file, 0, 0
        in
        let pix =
          try
            let (pix,im) = Hashtbl.find images (filename,dx,dy) in
            pix
          with Not_found -> try
                let (_,im) = Hashtbl.find images (filename,0,0) in
                let pix = Imager.image_pixmap im dx dy in
                Hashtbl.add images (filename, dx, dy) (pix.Imager.pixmap,im);
                pix.Imager.pixmap
              with Not_found ->
                  Gradients.check_filename filename;
                  if not (Sys.file_exists filename) then raise Not_found;
                  let im = Imager.image_load filename in
                  let pix = Imager.image_pixmap im im.Imager.w im.Imager.h in
                  Hashtbl.add images (filename, 0, 0) (pix.Imager.pixmap, im);
                  let pix = if dx = 0 || dy = 0 then pix else
                      begin
                        let pix = Imager.image_pixmap im dx dy in
                        Hashtbl.add images (filename, dx, dy)
                        (pix.Imager.pixmap,im);
                        pix
                      end 
                  in
                  pix.Imager.pixmap
        in
        X.changeWindowAttributes display w.w_window [CWBackPixmap pix];
        X.clearArea display w.w_window 0 0 0 0 true
      with
        _ -> 
          X.changeWindowAttributes display w.w_window
            [CWBackPixel (color_make w bg)];
          X.clearArea display w.w_window 0 0 0 0 true 
  )

let tip_delay = define_option ["tip_delay"] "" float_option 1.

class wob_base =
  object (self)
    val mutable wob = (None: wob option)
    
    val mutable actions = [];
    val mutable min_width = 0;
    val mutable min_height = 0;
    val mutable emask = [];
    val mutable wob_hooks = [];
    val mutable borderpixel = !!default_borderpixel;
    val mutable cursor = NoCursor;
    
    val mutable background = !!default_background;
    val mutable foreground = !!default_foreground;
    val mutable backimage = NoImage
    val mutable hilite_backimage = NoImage
    val mutable hilite_background = !!default_background
    val mutable hilite_foreground = !!default_foreground;
    val mutable hilitep = false;
    val mutable font = !!default_font    
    val mutable backimage_pix = noPixmap
    val mutable render_backimage = false
    
    method iter f = f (self :> wob_desc)
    
    method wob =
      match wob with
        None -> failwith "Wob desc not initialized"
      | Some w -> w
    
    method first_hook (e : wob_event) = ()
    method last_hook (e : wob_event) = ()
    method wob_hooks = wob_hooks
    method actions = actions
    method min_width = min_width
    method min_height = min_height
    method mask = emask @ std_mask
    
    method foreground = foreground
    method background = background
    method backimage = backimage
    
    method hilite_foreground = hilite_foreground
    method hilite_background = hilite_background
    method hilite_backimage = hilite_backimage
    
    method font = font
    method set_font f = font <- f;
    
    method set_backimage filename = 
      backimage <- filename;
      self#update_bg
    method set_hilite_background bg = 
      hilite_background <- bg;
      self#update_bg
    method set_hilite_foreground fg =
      hilite_foreground <- fg
    method set_hilite_backimage image =
      hilite_backimage <- image;
      self#update_bg    
    
    method set_foreground fg = 
      if fg <> foreground then
        begin
          foreground <- fg;
          self#refresh
        end
    
    method set_background bg =
      background <- bg;
      self#set_backimage NoImage;
      self#update_bg
    
    method client = self#wob.w_top.w_oo#client
    
    val mutable tip = false
    val mutable inside = false
    val mutable display_tip = fun _ _ -> ()
    
    method add_hook hook = 
      wob_hooks <- hook :: wob_hooks
    
    method send e =
      (try self#first_hook e with _ -> ());
      (try 
          List.iter (fun hook -> hook e) self#wob_hooks;
          self#last_hook e;
        with _ -> ())
    
    method set_actions bindings =
      let old = actions in
      actions <- bindings;
      if not (List.mem KeyPressMask emask) then
        (try List.iter (fun (a,_,_) ->
                match a with Key _ -> emask <- KeyPressMask :: emask; raise Exit
                | _ -> ()) bindings with _ -> ());
      if not (List.mem ButtonPressMask emask) then
        (try List.iter (fun (a,_,_) ->
                match a with Button _ | DblClick _ -> 
                    emask <- ButtonPressMask :: emask; 
                    raise Exit
                | _ -> ()) bindings with _ -> ());
      self#set_mask emask;
      match wob with
        None -> ()
      | Some w -> 
          if w.w_window <> noWindow then
            let win = w.w_window in
            unset_grabs display win old;
            set_grabs display win bindings
    
    method refresh =
      match wob with
        None -> ()
      | Some w -> send_one w WobRefresh
    
    method set_hilite h =
      hilitep <- h; self#update_bg; self#update_fg
    method reverse =
      self#set_hilite (not hilitep); hilitep
    
    method hilite = hilitep <- true; self#update_bg; self#update_fg
    method hilitep = hilitep
    method unhilite = hilitep <- false; self#update_bg; self#update_fg
    
    method bg = if hilitep then hilite_background else background
    method fg = if hilitep then hilite_foreground else foreground
    method bgimage = if hilitep then hilite_backimage else backimage
    
    method update_bg =
      match wob with
        None -> ()
      | Some w ->
          if not (w.w_window == noWindow) then
            let backimage = self#bgimage in
            make_backimage backimage w self#bg
    
    method update_fg = ()
    
    method set_backimage filename = 
      if backimage <> filename || render_backimage then begin
          backimage <- filename;
          if backimage <> NoImage then (try self#update_bg with _ -> ())
        end
    
    method resized = 
      match backimage with
        ScaleImage _ -> self#update_bg
      | _ -> ()
    
    method set_backpixmap pix =
      match wob with
        None -> ()
      | Some w ->
          if w.w_window <> noWindow then
            begin
              X.changeWindowAttributes display w.w_window
                [CWBackPixmap pix];
              X.clearArea display w.w_window 0 0 0 0 true
            end
    
    method handle_key (e,(s : string),key) = 
      try
        let modifiers = e.Xkey.state in
        let modmask = lnot modifiers in
        List.iter (fun binding ->
            match binding with
              Key(k,m), action, _ ->
                if k = key && (m  = modifiers || m = anyModifier) then
                  (action self#wob; raise Exit)
            | _ -> ()
        ) actions;
      (* if modifiers are in excess, still use *)
        List.iter (fun binding ->
            match binding with
              Key(k,m), action, _ ->
                if k = key && m land modifiers = modifiers then
                  (action self#wob; raise Exit)
            | _ -> ()
        ) actions;
      with
        Exit -> ()
    
    method handle_button e =
      try
        let w = self#wob in
        let button = e.Xbutton.detail in
        let modifiers = e.Xbutton.state in
        let click_kind = ref None in
        let which_click () =
          match !click_kind with
            None -> 
              let kind = click w.w_screen in
              click_kind := Some kind;
              kind 
          | Some kind -> kind in
        let modmask = lnot modifiers in
        List.iter (fun binding ->
            match binding with
              DblClick(b, m), action, _ ->
                if (b = anyButton || b = button)
                  && (m = modifiers || m = anyModifier) &&
                  which_click () = Double then
                  (action w; raise Exit) 
            | BMove (b, m), action, _ ->
                if (b = anyButton || b = button)
                  && (m = modifiers || m = anyModifier) &&
                  which_click () = DeltaMove then
                  (action w; raise Exit) 
            | _ -> ()
        ) actions;
        List.iter (fun binding ->
            match binding with
              Button(b, m), action, _ ->
                if (b = anyButton || b = button)
                  && (m = modifiers || m = anyModifier) then
                  (action w; raise Exit) 
            | _ -> ()
        ) actions;
        List.iter (fun binding ->
            match binding with
              Button(b, m), action, _ ->
                if (b = anyButton || b = button)
                  && (m land modifiers = modifiers) then
                  (action w; raise Exit) 
            | DblClick(b, m), action, _ ->
                if (b = anyButton || b = button)
                  && (m land modifiers = modifiers) &&
                  which_click () = Double then
                  (action w; raise Exit) 
            | BMove (b, m), action, _ ->
                if (b = anyButton || b = button)
                  && (m land modifiers = modifiers) &&
                  which_click () = DeltaMove then
                  (action w; raise Exit) 
            | _ -> ()
        ) actions;
      with
        Exit -> ()
    
    method set_min_height height =
      let old = min_height in
      min_height <- height;
      match wob with
        None -> ()
      | Some w -> 
          if w.w_window <> noWindow && w.w_geometry.height < min_height then
            send_one w.w_top WobGetSize
    
    method borderwidth = self#wob.w_geometry.border
    method set_borderwidth border = 
      match wob with
        None -> ()
      | Some w ->
          w.w_geometry.border <- border;
          if w.w_window <> noWindow then
            begin
              X.configureWindow display w.w_window [CWBorderWidth border];
              send_one w.w_top WobGetSize
            end
    
    val mutable is_shaped = false
    method is_shaped = is_shaped
    method set_shaped b = is_shaped <- b
    
    
    val mutable extensible_width = 0
    val mutable extensible_height = 0  
    method set_extensible_width n = extensible_width <- n
    method set_extensible_height n = extensible_height <- n
    
    
    method borderpixel = borderpixel 
    method set_borderpixel color = 
      borderpixel <- color;
      match wob with
        None -> ()
      | Some w ->
          if w.w_window <> noWindow then
            X.changeWindowAttributes display w.w_window 
              [CWBorderPixel (color_make w color)]
    
    method set_cursor new_cursor =
      cursor <- new_cursor;
      match wob with
        None -> ()
      | Some w ->
          if w.w_window <> noWindow then
            X.changeWindowAttributes display w.w_window 
              [CWCursor (cursor_make w new_cursor)]
    
    method create over=
      let w = self#wob in
      let s = w.w_screen in
      let cwa = [CWEventMask self#mask;
          CWOverrideRedirect over] in
      let cwa = let b = self#background in
        if b = "" then cwa else 
          (CWBackPixel (color_make w b)) :: cwa in
      let cwa = let b = self#borderpixel in
        if b = "" then cwa else
          (CWBorderPixel (color_make w b)):: cwa in
      let cwa = 
        if cursor = NoCursor then cwa else
          (CWCursor (cursor_make w cursor)):: cwa in
      let cwa = 
        if !!backing_store then (CWBackingStore WhenMapped)::cwa else cwa in
      let g = w.w_geometry in
      w.w_window <- X.createWindow display w.w_parent.w_window 
        g.x g.y 
        (max g.width 1) (max g.height 1)
      copyDepthFromParent InputOutput copyVisualFromParent 
        g.border cwa;
      Eloop.add_window s.s_scheduler w.w_window (self#xevents);
      set_grabs display w.w_window self#actions;  
    
    method set_min_width width =
      let old = min_width in
      min_width <- width;
      match wob with
        None -> ()
      | Some w ->
          if w.w_window <> noWindow && w.w_geometry.width < min_width then
            send_one w.w_top WobGetSize
    
    method set_mask new_mask =
      emask <- new_mask;
      match wob with
        None -> ()
      | Some w ->
          if w.w_window <> noWindow then
            X.changeWindowAttributes display w.w_window [CWEventMask self#mask]
    
    method set_wob new_wob =
      match wob with
        None -> wob <- Some new_wob;
      | Some w -> failwith "Wob desc already used"
    
    method set_tip_display f = 
      tip <- true;
      display_tip <- f    
    method xevents ev =
      match ev.ev_event with
        EnterNotifyEvent e when not_grab_related e.Xcrossing.mode ->
          inside <- true;
          if tip then Concur.Thread.add_timer !!tip_delay (fun _ ->
                if inside then display_tip (self :> wob_desc) inside
            );
          send self#wob WobEnter
      | LeaveNotifyEvent e when not_grab_related e.Xcrossing.mode ->
          inside <- false;
          (* remove the tip ... *)
          display_tip (self :> wob_desc) inside;
          send self#wob (WobLeave (match e.Xcrossing.detail with
                NotifyAncestor | NotifyVirtual | NotifyNonlinearVirtual -> true
              | _ -> false))

      | ButtonPressEvent e1 -> 
          send self#wob (WobButtonPress e1);
          last_click := e1;
      | KeyPressEvent e -> 
          let modifiers = e.Xkey.state in
          let (s,keysym,_) = KeyBind.lookupString display ev.ev_event in
          send self#wob (WobKeyPress (e,s,keysym))
      | ExposeEvent e -> 
          remove_expose ev;
          send_one self#wob WobRefresh
      | ButtonReleaseEvent e -> send self#wob (WobButtonRelease e)
      | _ -> ()
          
end          

let make parent wob =
  let w =
    {
      w_window = noWindow;
      w_parent = parent;
      w_top = parent.w_top;
      w_screen = parent.w_screen;
      w_geometry = { x=0; y=0; width=0; height=0; border=0; };
      w_env = Wobenv.empty ();
      w_oo = wob;
      w_queue = [];
    } in
  wob#set_wob w;
  send w WobInit;
  w

let setenv w var value = Wobenv.set w.w_env var value
let getenv w var = Wobenv.get w.w_env var
let remenv w var = Wobenv.remove w.w_env var
let sgetenv w var value = try Wobenv.get w.w_env var with _ -> value

let desc x = (x :> wob_desc)

let restore_clients () =
  Array.iter (fun sdesc ->
      let sw = sdesc#wob in
      let root = sw.w_window in
      let s = sw.w_screen in
      Wintbl.iter (fun window (c,w) ->
          send w.w_top WobExitGwml;
          if window = c.c_window then
            let g = w.w_top.w_geometry in
            Xsync.reparentWindow display c.c_window root
              (g.x - g.border) (g.y - g.border);
            let geo = X.getGeometry display c.c_window in
            ()
            (*
            let cg = w.w_geometry in
            Printf.printf "Top: %d,%d(%d) Client: %d,%d Final: %d,%d"
              g.x g.y g.border cg.x cg.y geo.gg_x geo.gg_y;
            print_newline (); 
            *)
      ) s.s_clients
  ) !screens
  

let exit_gwml _ =
  restore_clients ();
  Xlib.closeDisplay display;
  exit 43

let restart_cmd = ref Sys.argv
  
let restart _ = 
  restore_clients ();
  Utils.set_signal Sys.sigalrm Sys.Signal_ignore;
  Utils.set_signal Sys.sigvtalrm Sys.Signal_ignore;
  Unix.execvp !restart_cmd.(0) !restart_cmd

  
