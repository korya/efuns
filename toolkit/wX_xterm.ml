(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xlib
open Xtypes
open WX_types  
  
type char_drawing = Xtypes.setGCattributes list
type xterm_gc = int

type xterm_event =
  XTKeyPress of Xtypes.modifiers * string * Xtypes.keySym
| XTResize of int * int
| XTButtonPress of Xtypes.modifiers * int * int * int
| XTMouseMotion of Xtypes.modifiers * int * int * int

type xterm_color = int
type xterm_font = int

type xterm_display = 
  {
    dpy : Xtypes.display;
    root : Xtypes.window;
    visual : Xtypes.visual;
    colormap : Xtypes.colormap;
    depth : int;
    mutable f_width : int;
    mutable f_ascent : int;
    mutable f_height : int;
    windows : (Xtypes.window, unit -> unit) Hashtbl.t;
    dpy_pixels_names : string array;
    dpy_pixels : Xtypes.pixel array;
    mutable dpy_pixels_n : int;
    dpy_fonts_names : string array;
    dpy_fonts : Xtypes.font array;
    mutable dpy_fonts_n : int;
    mutable dpy_highlighted : int;
    root_oo : WX_root.t;
  } 

and xterm_window =
  {
    display : xterm_display;
    win : Xtypes.window;
    gc : Xtypes.gc;
    mutable gc_state : int;
    mutable ncols : int;
    mutable nlines : int;
    mutable table : string array;
    mutable gc_table : int array array;
    mutable modified : bool array;
    mutable region : string option;
    mutable handler : (Xtypes.xevent -> unit);
  }

let displays = ref []
  
let create_display (root_oo : WX_root.t) color_names font_names =
  let dpy = root_oo#display in
  let screen = root_oo#screen.s_screen in
  let root = screen.scr_root in
  let visual = screen.scr_root_visual_id in
  let depth = screen.scr_root_depth in
  let colormap = screen.scr_default_colormap in
  let font_name = font_names.(0) in  
  let font = root_oo#font_make font_name true in
  let display =
    { 
      dpy = dpy;
      root = root;
      colormap = colormap;
      visual = visual;
      depth = depth;
      windows = Hashtbl.create 13;
      f_width = font.font_width;
      f_ascent = font.font_ascent;
      f_height = font.font_height;
      dpy_pixels_names = color_names;
      dpy_pixels = Array.create 256 (id_to_pixel (-1));
      dpy_pixels_n = 2;    
      dpy_fonts_names = font_names;
      dpy_fonts = Array.create 256 (id_to_font (-1));
      dpy_fonts_n = 1;
      dpy_highlighted = 0;
      root_oo = root_oo;
    } 
  in
  let _ = 
    displays := display :: !displays;
    let resetProperty atom =
      X.changeProperty dpy root PropModeAppend atom XA.xa_string 1 "";
    in
    resetProperty XA.xa_cut_buffer0;
    resetProperty XA.xa_cut_buffer1;
    resetProperty XA.xa_cut_buffer2;
    resetProperty XA.xa_cut_buffer3;
    resetProperty XA.xa_cut_buffer4;
    resetProperty XA.xa_cut_buffer5;
    resetProperty XA.xa_cut_buffer6;
    resetProperty XA.xa_cut_buffer7;
  in
  display

type event_handler = xterm_window -> xterm_event -> unit

let get_color display n =
  let pixel = display.dpy_pixels.(n) in
  if pixel_to_id pixel = -1 then
    let pixel = 
      try (X.allocNamedColor display.dpy display.colormap 
            display.dpy_pixels_names.(n)).anc_pixel
      with
        XError e -> 
          Printf.printf "No color %s (replaced by white)" 
            display.dpy_pixels_names.(n);
          print_newline (); 
          let screen = display.root_oo#screen.s_screen in
          screen.scr_white_pixel
    in
    display.dpy_pixels.(n) <- pixel; pixel
  else pixel

let get_font display n =
  let font = display.dpy_fonts.(n) in
  if font_to_id font = -1 then
    let font =
      try
        Xsync.openFont display.dpy display.dpy_fonts_names.(n) 
      with
        _ -> X.openFont display.dpy "fixed"
    in
    display.dpy_fonts.(n) <- font; font
  else font
  
let gcattrs_from_attr display attr =
  let fg = attr land 0xff in
  let bg = (attr lsr 8) land 0xff in
  let font = (attr lsr 16) land 0xff in
  let specials = attr lsr 24 in
  if specials = 0 then 
    [ GCforeground (get_color display fg);
      GCbackground (get_color display bg);
      GCfont (get_font display font)]
  else
    if specials land 1 = 1 then 
      (* highlighted *)
      [ GCforeground (get_color display fg);
        GCbackground (get_color display display.dpy_highlighted);
        GCfont (get_font display font)]
    else
      [ GCforeground (get_color display fg);
        GCbackground (get_color display bg);
        GCfont (get_font display font)]
      
let make_attr fg bg font highlighted =
  fg + (bg lsl 8) + (font lsl 16) + (if highlighted then 1 lsl 24 else 0)

let direct_attr =  make_attr 0 1 0 false
let inverse_attr =  make_attr 0 1 0 false

let default_handler display ev = 
  match ev.ev_event with
  | SelectionRequestEvent e -> 
      Selection.handleSelectionRequest display e
  | SelectionClearEvent e ->
      Selection.handleSelectionClear display e
  | _ -> ()      
  

let setSelection window string =
  Selection.setSelection window.display.dpy window.win XA.xa_primary
    (fun target ->
      if target = XA.xa_string then 1, string else raise Not_found)
  !Eloop.event_time
  
let getSelection window =
  Selection.getSelection window.display.dpy window.win
    XA.xa_primary XA.xa_string !Eloop.event_time 

let removeSelection window =
  Selection.removeSelection window.display.dpy XA.xa_primary 
    !Eloop.event_time
  
let draw_string window x y str pos len attr =
  let strlen = String.length str in
  let len = if strlen < pos + len then strlen - pos else len in
  begin
    if x < window.ncols && y < window.nlines && y > (-1) &&
      pos < strlen then
      let (x,len,pos) = 
        if x < 0 then (0,len + x, pos - x)
        else (x,len,pos)
      in
      let len = 
        if x+len < window.ncols then len
        else window.ncols - x
      in
      if len > 0 then
        begin
          String.blit str pos window.table.(y) x len;
          Array.fill window.gc_table.(y) x len attr;
          window.modified.(y) <- true;
        end;
  end

let clear_eol window x y len =
  if x < window.ncols && y < window.nlines && y > (-1) && 0 < len then
    let (x,len) =
      if x < 0 then (0,len + x)
      else (x,len)
    in
    let len = 
      if x+len < window.ncols then len
      else window.ncols - x
    in
    if len > 0 then
      begin
        String.fill window.table.(y) x len ' ';
        Array.fill window.gc_table.(y) x len 0;
        window.modified.(y) <- true;
      end

let clear window =
  for y = 0 to window.nlines - 1 do
    String.fill window.table.(y) 0 window.ncols ' ';
    Array.fill window.gc_table.(y) 0 window.ncols 0;
    window.modified.(y) <- true;
  done

let changeGC display gc gc_state xterm_gc =
  if xterm_gc <> !gc_state then
    begin
      gc_state := xterm_gc;
      X.changeGC display.dpy gc (gcattrs_from_attr display xterm_gc)
    end

let expose_window window () =
  let display = window.display in
  let baseline = 2 + display.f_ascent in
  let gc_state = ref window.gc_state in
  let gc = window.gc in
  for i = 0 to window.nlines - 1 do
    if window.modified.(i) then
      let start = ref 0 in
      let curs = ref 0 in
      while !curs < window.ncols do
        if window.gc_table.(i).(!curs) == window.gc_table.(i).(!start) then
          curs := !curs + 1
        else
          let xterm_gc = window.gc_table.(i).(!start) in
          if xterm_gc = 0 then
            X.clearArea display.dpy window.win 
              (2 + !start * display.f_width) 
            (2 + i * display.f_height)
            ((!curs - !start) * display.f_width)
            display.f_height false
          else
            begin
              changeGC display gc gc_state xterm_gc;
              Xlib.imageSubString display.dpy window.win 
                gc
                (2 + !start * display.f_width) 
              (baseline + i * display.f_height)
              window.table.(i) !start (!curs - !start);
            end;
          start := !curs;
          curs := !curs + 1;
      done;
      let xterm_gc = window.gc_table.(i).(!start) in
      if xterm_gc = 0 then
        X.clearArea display.dpy window.win 
          (2 + !start * display.f_width) 
        (2 + i * display.f_height)
        ((!curs - !start) * display.f_width)
        display.f_height false
      else
        begin
          changeGC display gc gc_state xterm_gc;
          Xlib.imageSubString display.dpy window.win gc
            (2 + !start * display.f_width)
          (baseline + i * display.f_height)
          window.table.(i) !start (!curs - !start);
        end;
      window.modified.(i) <- false;
  done;
  window.gc_state <- !gc_state


let create_window display parent ncols nlines =
  let dpy = display.dpy in
  let mask = 
    [CWBackPixel (get_color display 1);
      CWEventMask [ExposureMask; KeyPressMask; 
        ButtonPressMask; ButtonReleaseMask;
        StructureNotifyMask; 
        ButtonMotionMask;
      ]
    ]
  in
  let mask =
    try
      (CWCursor (createFontCursor dpy XC.xc_xterm)) :: mask
    with
      _ -> mask
  in
  let win = 
    Xlib.createSimpleWindow dpy parent 
      0 0 
      (5 + ncols * display.f_width)
    (5 + nlines * display.f_height)
    1 mask
  in
  let gc = X.createGC dpy parent
    (gcattrs_from_attr display direct_attr) in
  let window =
    {
      display = display;
      win = win;
      ncols = ncols;
      nlines = nlines;
      gc = gc;
      gc_state = direct_attr;
      table = Array.init nlines (fun i -> String.make ncols ' ');
      gc_table = Array.init nlines (fun i -> Array.create ncols 0);
      modified = Array.create nlines false;
      region = None;
      handler = (fun _ -> ());
    }
  in
  X.mapWindow display.dpy win;
  window
    ;;

let remove_expose dpy win =
  try
    Xlib.cleanEvents dpy;
    while true do
      match Xlib.checkPredEvent dpy (fun ev' ->
            match ev'.ev_event with
              ExposeEvent _ when ev'.ev_window = win ->  true
            | _ -> false) false with
        None -> raise Not_found
      | _ -> ()
    done
  with
    Not_found -> ()

let remove_motions dpy win k =
  let e = ref k in
  try
    while true do
      let ev' = Xlib.checkEvent dpy in
      match ev'.ev_event with
        MotionNotifyEvent k when ev'.ev_window = win -> e := k
      | _ -> 
          Xlib.putBackEvent dpy ev';
          raise Not_found
    done;
    !e
  with
    Not_found -> !e

let dirty_window window =
  for i = 0 to window.nlines - 1 do
    window.modified.(i) <- true;
  done

let update_displays () =
  List.iter (fun display ->
      Hashtbl.iter (fun win expose -> expose ())
      display.windows
  ) !displays

let button = ref 0
let xterm_handler window handler ev =
  let display = window.display in
  match ev.ev_event with
    KeyPressEvent k ->
      let modifiers = k.Xkey.state in
      let (s,keysym,_) = KeyBind.lookupString display.dpy ev.ev_event in
      handler (XTKeyPress (modifiers, s, keysym));
      update_displays ()
  | ButtonPressEvent k ->
      button := k.Xbutton.detail;
      handler (XTButtonPress (k.Xbutton.state,k.Xbutton.detail,
          (k.Xbutton.x_event - 2) / display.f_width,
          (k.Xbutton.y_event - 2) / display.f_height
        ));
      update_displays ();
  | ButtonReleaseEvent k ->
      button := 0
  | MotionNotifyEvent k -> 
      let k = remove_motions display.dpy window.win k in
      if !button <> 0 then
        handler (XTMouseMotion (k.Xmotion.state, !button,
            (k.Xmotion.x_event - 2) / display.f_width,
            (k.Xmotion.y_event - 2) / display.f_height
          ));
      update_displays ()
  | ExposeEvent _ ->
      remove_expose display.dpy window.win;
      dirty_window window;
      expose_window window ()
  | ConfigureNotifyEvent e ->
      let ncols = e.Xconfigure.width / display.f_width in
      let nlines = e.Xconfigure.height / display.f_height in
      if ncols <> window.ncols || nlines <> window.nlines then
        begin
          window.nlines <- nlines;
          window.ncols <- ncols;
          window.table <- Array.init nlines
            (fun i -> String.make ncols ' ');
          window.gc_table <- Array.init nlines
            (fun i -> Array.create ncols 0);
          window.modified <- Array.create nlines true;
          handler (XTResize (ncols,nlines));
          expose_window window ()
        end
  | SelectionRequestEvent e -> 
      Selection.handleSelectionRequest display.dpy e
  | SelectionClearEvent e ->
      Selection.handleSelectionClear display.dpy e
  | _ -> ()

let install_handler display window handler =
  window.handler <- xterm_handler window handler;
  Hashtbl.add display.windows window.win (expose_window window)

  
let list_removeq list ele =
  List.fold_left (fun list e ->
                    if e == ele then list
                    else e ::  list) [] list
  
  
let close_display display =
  displays := list_removeq !displays display

let lst_it = ref 0

let destroy_window window =
  let display = window.display in
  X.destroyWindow display.dpy window.win;
  Hashtbl.remove display.windows window.win

let change_font window fontname =
  let display = window.display in
  let font = Xsync.openFont display.dpy fontname
  in
  let qf = X.queryFont display.dpy font in
  let font_info = qf.qf_info in
  let font_width = font_info.font_max_bounds.char_width in
  let font_ascent = font_info.Xtypes.font_ascent in
  let font_descent = font_info.Xtypes.font_descent in
  let font_height = font_ascent + font_descent in
  display.f_width <- font_width;
  display.f_height <- font_height;
  display.f_ascent <- font_ascent;
  X.configureWindow display.dpy window.win
    [CWWidth (5 + window.ncols * display.f_width);
    CWHeight (5 + window.nlines * display.f_height)];
  X.changeGC display.dpy window.gc [GCfont font]

let get_cutbuffer window =
  try
    getSelection window
  with
    _ ->
  let display = window.display in
  let prop = getWholeProperty display.dpy display.root 
      XA.xa_cut_buffer0 in
  if prop.gp_type = XA.xa_string then
    prop.gp_value
  else
    raise Not_found

let set_cutbuffer window str =
  let display = window.display in
  X.rotateProperties display.dpy display.root 1 [
    XA.xa_cut_buffer0;
    XA.xa_cut_buffer1;
    XA.xa_cut_buffer2;
    XA.xa_cut_buffer3;
    XA.xa_cut_buffer4;
    XA.xa_cut_buffer5;
    XA.xa_cut_buffer6;
    XA.xa_cut_buffer7];
  X.changeProperty display.dpy display.root PropModeReplace
    XA.xa_cut_buffer0  XA.xa_string
    1 str;
  setSelection window str
  
let setHighlight xt n = xt.dpy_highlighted <- n

class t (parent : container) display ncols nlines =
  object (self)
    val s = parent#screen
    val mutable parent = parent
    val nlines = nlines
    val ncols = ncols
    val mutable xterm = None
    val mutable szhints = {
        border_width = 0;
        requested_width = 1;
        requested_height = 1;
        min_width = display.f_width;
        min_height = display.f_height+5;
        max_width = max_int;
        max_height = max_int;
        expand_x = true;
        expand_y = true;
        retract_x = false;
        retract_y = false;
        inc_x = 0;
        inc_y = 0;
        comp_timestamp = -1; (* Never computed *)
      } 
    val display = display
    
    method set_parent p = 
      assert ((match xterm with None -> true | Some xterm ->
              xterm.win = noWindow) || parent == p);
      parent <- p
    
    method show = match xterm with None -> () | Some xterm ->
          X.mapWindow display.dpy xterm.win
    
    method hide = match xterm with None -> () | Some xterm ->
          X.unmapWindow display.dpy xterm.win
    
    method realize =   
      let xt = create_window display parent#window ncols nlines
      in 
      Eloop.add_window s.s_eloop xt.win (fun e -> xt.handler e);    
      xterm <- Some xt
    
    method destroy = match xterm with None -> () | Some xterm ->
          X.destroyWindow display.dpy xterm.win
    
    method  size_request = 
      let sz = szhints in
      let sz = 
        match xterm with None -> 
            sz.requested_width <- ncols * display.f_width + 4;
            sz.requested_height <- nlines * display.f_height + 4;
            sz
        | Some xterm ->
            sz.requested_width <- xterm.ncols * display.f_width + 4;
            sz.requested_height <- xterm.nlines * display.f_height + 4;
            sz
      in
      (*
      Printf.printf "Xterm request: %d x %d" 
        sz.requested_width sz.requested_height;
      print_newline ();
      *)
      sz
      
  method size_allocate x y dx dy =
    match xterm with None -> () | Some xterm ->
        Xlib.moveResizeWindow display.dpy xterm.win x y dx dy
  
  method width =
    match xterm with None -> 0 | Some xterm -> xterm.ncols * display.f_width
  
  method height =
    match xterm with None -> 0 | Some xterm -> xterm.nlines * display.f_height
  
  method configure (list : base_attributes list) = ()
  method xterm = 
    match xterm with None -> raise Not_found | 
      Some xterm -> xterm
  method contained = (self :> contained)
  method name = "xterm"
  
  method check_abort =
      let xterm = self#xterm.win in
    match Xlib.checkPredEvent display.dpy (fun ev ->
          match ev.ev_event with
            KeyPressEvent e when e.Xkey.event = xterm ->
              let mods = e.Xkey.state in
              let (s, k, m) = KeyBind.lookupString display.dpy ev.ev_event in
              (k = XK.xk_g && mods land controlMask <> 0)
          | _ -> 
              false
      ) false with
      None -> false
      | Some _ -> true
          
    method display = display.dpy
    method window = self#xterm.win
    method set_cutbuffer = set_cutbuffer self#xterm
end