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

open Xtypes
  
let max (x : int) (y : int) = if x>y then x else y
let min (x : int) (y : int) = if x<y then x else y
type handler = unit -> unit

let drawLine = Xlib.drawSegment
  
let relieveRectangle dpy win gc x y w h hilite shadow =
  X.changeGC dpy gc [GCforeground hilite];
  drawLine dpy win gc x y (w+x-1) y;
  drawLine dpy win gc x y x (h+y-1);
  X.changeGC dpy gc [GCforeground shadow];
  drawLine dpy win gc x (h+y-1) (w+x-1) (h+y-1);
  drawLine dpy win gc (w+x-1) y (w+x-1) (h+y-1)

let relieveHalfRectangle dpy win gc x y w h hilite shadow =
  X.changeGC dpy gc [GCforeground hilite];
  drawLine dpy win gc x (y-1) x (h+y);
  drawLine dpy win gc (x+1) y (x+1) (h+y-1);
  X.changeGC dpy gc [GCforeground shadow];
  drawLine dpy win gc (w+x-1) (y-1) (w+x-1) (h+y);
  drawLine dpy win gc (w+x-2) y (w+x-2) (h+y-1)

  
let pixmap_path = ref ([] : string list)
let double_click_delay = ref 250.
let delta_move_size = ref 1
  
let default_borderpixel = ref ""
let default_background = ref "gray51"
let default_foreground = ref "white"
let default_font = ref "fixed"

module GCCache = struct
    
    let size = 16
    type t = {
        gcs : gc array;
        ids : int array;
        mutable next : int;
        dpy : display;
        root : window;
      }
      
    let create dpy scr = 
      let fg = scr.scr_black_pixel in
      let bg = scr.scr_white_pixel in
      let font = X.openFont dpy "fixed" in
      let t = { 
          gcs = Array.init size (fun _ -> X.createGC dpy scr.scr_root
                [GCforeground fg; GCbackground bg; GCfont font]);
          ids = Array.create (size * 3) 0;
          next = 0;
          dpy = dpy;
          root = scr.scr_root;
        } in
      for i = 0 to size - 1 do t.ids.(i) <- pixel_to_id fg done;
      for i = size to 2*size - 1 do t.ids.(i) <- pixel_to_id bg done;
      for i = 2*size to 3*size - 1 do t.ids.(i) <- font_to_id font done;
      t
    
    let get3 t fg bg font =
      let ids = t.ids in
      let rec iter i =
        if pixel_to_id fg = ids.(i) &&
          pixel_to_id bg = ids.(i+size) &&
          font_to_id font = ids.(i+size+size) then
          t.gcs.(i)
        else
        if i>0 then
          iter (i-1)
        else
        let next = t.next in
        let gc = t.gcs.(next) in
        t.next <- (next+1) mod size;
        ids.(next) <- pixel_to_id fg;
        ids.(next+size) <- pixel_to_id bg;
        ids.(next+size+size) <- font_to_id font;
        X.changeGC t.dpy gc [GCforeground fg; GCbackground bg; GCfont font]; 
        gc
      in
      iter (size-1)
    
    let get2 t fg bg =
      let ids = t.ids in
      let rec iter i =
        if pixel_to_id fg = ids.(i) &&
          pixel_to_id bg = ids.(i+size) then
          t.gcs.(i)
        else
        if i>0 then
          iter (i-1)
        else
        let next = t.next in
        let gc = t.gcs.(next) in
        t.next <- (next+1) mod size;
        ids.(next) <- pixel_to_id fg;
        ids.(next+size) <- pixel_to_id bg;
        X.changeGC t.dpy gc [GCforeground fg; GCbackground bg];
        gc
      in
      iter (size-1)
        
    let get1 t fg =
      let ids = t.ids in
      let rec iter i =
        if pixel_to_id fg = ids.(i) then
          t.gcs.(i)
        else
        if i>0 then
          iter (i-1)
        else
        let next = t.next in
        let gc = t.gcs.(next) in
        t.next <- (next+1) mod size;
        ids.(next) <- pixel_to_id fg;
        X.changeGC t.dpy gc [GCforeground fg];
        gc
      in
      iter (size-1)
  
  end
  
type event_desc =
| EnterWindow 
| LeaveWindow  
| ButtonPress
| KeyPress
| ButtonReleased
| ButtonMotion
| Key of keySym * modifiers
| Button of button * modifiers
| GrabbedKey of keySym * modifiers
| GrabbedButton of button * modifiers
| FocusIn
| FocusOut
  
  
let autorepeat_delay = ref 0.06
let autorepeat_rate = ref 0.05

type click = Simple | Double | DeltaMove | Other
type justified = Left | Center | Right
  
type bar_desc = Horizontal | Vertical  

type color = {
    c_pixel : Xtypes.pixel;
    c_red : int;
    c_green : int;
    c_blue : int;
  }

type relief = ReliefFlat | 
  ReliefSunken
| ReliefRaised
| ReliefRidge
| ReliefGroove
| ReliefInSunken
| ReliefInRaised
| ReliefInRidge
| ReliefInGroove
| ReliefSunkenN of int
| ReliefRaisedN of int
| ReliefInSunkenN of int
| ReliefInRaisedN of int
  
type color_desc =
  Namedcolor of string
| RgbColor of int * int * int
  
type pixmap_desc = 
  FromFile of string
| FromFunction of Xtypes.pixmap * Xtypes.pixmap
| FromData of Xpm.pixmap_data
  
type cursor_desc =
  FontCursor of int
| BitmapCursor of string * string
| NoCursor

  
type font = {
    font_name : string;
    font_id : Xtypes.font;
    font_info : Xtypes.queryFontRep;
    font_ascent : int;
    font_width : int;
    font_height : int;
  }

type cursor = {
    curs_desc : cursor_desc;
    curs_id : Xtypes.cursor;
  }

let noCursor = {
    curs_desc = NoCursor;
    curs_id = noCursor;
  }

let noColor = {
    c_pixel = Xtypes.id_to_pixel 0;
    c_red = 0;
    c_green = 0;
    c_blue = 0;
  }
  
type szhints = {
    mutable border_width : int;
    mutable requested_width : int;
    mutable requested_height : int;
    mutable min_width : int;
    mutable min_height : int;
    mutable max_width : int;
    mutable max_height : int;
    mutable expand_x : bool;
    mutable expand_y : bool;
    mutable retract_x : bool;
    mutable retract_y : bool;
    mutable inc_x : int;
    mutable inc_y : int;
    mutable comp_timestamp : int;
  }  

  
type op = Append | Replace | Remove
  
type base_attributes =
  
(* SIZE HINTS *)
| MinWidth of int
| MinHeight of int
| MaxWidth of int
| MaxHeight of int
| ExpandX of bool
| ExpandY of bool
| FillX of bool
| FillY of bool
| IncX of int
| IncY of int
| PadX of int
| PadY of int
| IpadX of int
| IpadY of int
| RetractX of bool
| RetractY of bool
| Position of int * int
  
(* WIN ATTRIBUTES *)
| Background of string
| Foreground of string
| BorderColor of string
| BorderWidth of int
| Cursor of cursor_desc
| EventMask of op * (eventMask list)
| Relief of relief
  

(* BINDINGS *)
| Bindings of (event_desc * handler) list

type contained = <
  show : unit;
  hide : unit;
  realize : unit;
  destroy : unit; 
  size_request : szhints; (* These ones should be computed *)
  size_allocate : int -> int -> int -> int -> unit;
  width : int; height : int;
  configure : base_attributes list -> unit;
  set_parent : container -> unit;
  name : string;
  >

and resize_widget = < update_size : unit >
and refresh_widget = < refresh : unit >
  
and screen_struct = {
    s_display : Xtypes.display;
    s_screen : Xtypes.screen;
    
    s_colors : (color_desc, color) Hashtbl.t;
    s_default_color : color;
    
    s_pixmaps : (string, Xpm.pixmap) Hashtbl.t;
    
    s_fonts : (string, font) Hashtbl.t;
    s_default_font : font;
    
    s_cursors : (cursor_desc, cursor) Hashtbl.t;
    s_gcs : GCCache.t;
    
    mutable s_timestamp : int;
    mutable s_wait_resize : resize_widget list;
    mutable s_wait_refresh : refresh_widget list;
    
    mutable s_last_click : Xbutton.t;
    s_eloop : Eloop.display;
  }

and  container = <
  screen : screen_struct;
  color_make : string -> bool -> color;
  cursor_make : cursor_desc -> bool -> cursor;
  display : Xtypes.display; 
  font_make : string -> bool -> font;
  pixmap_make : string * pixmap_desc -> Xpm.pixmap;
  update_top_size : unit;
  window : Xtypes.window; 
  geometry : geometry;
  getShadow : color -> color;
  getHilite : color -> color;
  default_font : font;
  wait_resize : unit;
  name : string;
  >

and  container_add = <
  screen : screen_struct;
  color_make : string -> bool -> color;
  cursor_make : cursor_desc -> bool -> cursor;
  display : Xtypes.display; 
  font_make : string -> bool -> font;
  pixmap_make : string * pixmap_desc -> Xpm.pixmap;
  update_top_size : unit;
  window : Xtypes.window; 
  geometry : geometry;
  getShadow : color -> color;
  getHilite : color -> color;
  default_font : font;
  wait_resize : unit;
  name : string;
  container_add : contained -> unit;
  >
  
and window = {
    mutable w_window : Xtypes.window;
    mutable w_shown : bool;
    mutable w_clear : bool;
    w_geometry : geometry;  
    mutable w_override : bool;
    mutable w_background : color;
    mutable w_relief_colors : (color * color) option;
    mutable w_relief : relief;
    mutable w_borderpixel : color;
    mutable w_cursor : cursor;
    mutable w_mask : eventMask list;
    mutable w_size_timestamp : int;
    mutable w_refresh_timestamp : int;
    mutable w_foreground : color;
    mutable w_inverse : bool;
    
    mutable w_enter_window : handler;
    mutable w_leave_window : handler;
    mutable w_button_press : handler;
    mutable w_key_press : handler;
    mutable w_button_released : handler;
    mutable w_button_motion : handler;
    mutable w_focus_in : handler;
    mutable w_focus_out : handler;
    mutable w_actions : (event_desc * handler) list;
    mutable w_ipad_x : int;
    mutable w_ipad_y : int; 
    mutable w_fill_x : bool;
    mutable w_fill_y : bool;
    mutable w_size_modified : bool;
  }

let hex c =
  let c = Char.lowercase c in
  if c >='0' && c<='9' then Char.code c - Char.code '0' else
  if c >= 'a' && c<='f' then Char.code c - Char.code 'a' else
    raise Not_found

let loop = Eloop.event_loop 



  (*
  KeyPressMask; KeyReleaseMask;
  ButtonPressMask; ButtonReleaseMask;
  *)
  

let not_grab_related mode = mode <> NotifyGrab && mode <> NotifyUngrab
  
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
  
let delta_move_size = ref 1

      
let set_grabs display win list =
  List.iter (fun (g,_) ->
      try
        match g with
          GrabbedKey (keysym,modifiers) ->
            let keycode,_ = KeyBind.keysymToKeycode display keysym in
            X.grabKey display win false
              GrabModeAsync GrabModeAsync keycode modifiers;
        | GrabbedButton (button,modifiers) ->
            X.grabButton display win false []
              GrabModeAsync GrabModeAsync noConfineTo Xtypes.noCursor 
              button modifiers;
        | _ -> ()
      with _ -> ()
  ) list
  
let unset_grabs display win list =
  List.iter (fun (g,_) ->
      try
        match g with
          GrabbedKey (keysym,modifiers) ->
            let keycode,_ = KeyBind.keysymToKeycode display keysym in
            X.ungrabKey display win keycode modifiers;
        | GrabbedButton (button,modifiers) ->
            X.ungrabButton display win button modifiers;
        |  _ -> ()
      with _ -> ()
  ) list

let mouse_x_event = ref 0
let mouse_y_event = ref 0
let button_event = ref 0
let key_string = ref ""
let key_sym = ref 0
let modifiers_event = ref 0
      
type null = < >
let null_handler () = ()

let default_click =     {
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

(* OTHER REQUIRED WIDGETS:
- Scrollbar : horizontal and vertical
- ViewPort : limited size but inside greater.
- Menu : popup
*)

  
  (* This is the list of attributes which can be used to configure a widget.
  Most of them are useful for all windows, but some may be only useful for
  particular ones. 
  
  These attributes are given either at the creation of the widget,
  or after the creation with the "configure" method.
  
  NOTE: This is a development version. Some of these attributes are not
  implemented.
  *)
  
    
let drawRelief2 dpy win cache x y w h xin yin hilite shadow relief =
  match relief with
    ReliefFlat -> ()
  | ReliefRaised ->
      let gc = GCCache.get1 cache hilite in
      X.polyLine dpy win gc Origin [(w+x-1),y; x,y; x, (h+y-1)];
      let gc = GCCache.get1 cache shadow in
      X.polyLine dpy win gc Origin [(w+x-1),y; (w+x-1),(h+y-1); x,(h+y-1)]
  | ReliefSunken ->
      let gc = GCCache.get1 cache shadow in
      X.polyLine dpy win gc Origin [(w+x-1), y;x,y; x, (h+y-1)];
      let gc = GCCache.get1 cache hilite in
      X.polyLine dpy win gc Origin [x, (h+y-1); (w+x-1), (h+y-1);(w+x-1), y]
  | ReliefRidge ->
      let gc = GCCache.get1 cache hilite in
      X.polyLine dpy win gc Origin [(w+x-2), (y+1); (x+1),(y+1);(x+1),(h+y-2);
        (w+x-2),(h+y-2); (w+x-2),(y+1)];
      let gc = GCCache.get1 cache shadow in
      X.polyLine dpy win gc Origin [(w+x-1),y;x,y;x,(h+y-1);
        (w+x-1),(h+y-1); (w+x-1),y]
  | ReliefGroove ->
      let gc = GCCache.get1 cache shadow in
      X.polyLine dpy win gc Origin [(w+x-2), (y+1); (x+1),(y+1); (x+1), (h+y-2);
        (w+x-2), (h+y-2); (w+x-2), (y+1)];
      let gc = GCCache.get1 cache hilite in
      X.polyLine dpy win gc Origin [x, y; (w+x-1), y; x, (h+y-1);
        (w+x-1), (h+y-1); (w+x-1), y]
  | ReliefInRaised ->
      let x = x + xin - 1 in
      let y = y + yin - 1 in
      let w = w - xin - xin + 1 in
      let h = h - xin - xin + 1 in
      let gc = GCCache.get1 cache hilite in
      X.polyLine dpy win gc Origin [(w+x-1),y; x,y; x, (h+y-1)];
      let gc = GCCache.get1 cache shadow in
      X.polyLine dpy win gc Origin [(w+x-1),y; (w+x-1),(h+y-1); x,(h+y-1)]
  | ReliefInSunken ->
      let x = x + xin - 1 in
      let y = y + yin - 1 in
      let w = w - xin - xin + 1 in
      let h = h - xin - xin + 1 in
      let gc = GCCache.get1 cache shadow in
      X.polyLine dpy win gc Origin [(w+x-1),y;x,y; x, (h+y-1)];
      let gc = GCCache.get1 cache hilite in
      X.polyLine dpy win gc Origin [x, (h+y-1); (w+x-1), (h+y-1);(w+x-1), y]
  | ReliefInRidge ->
      let x = x + xin - 2 in
      let y = y + yin - 2 in
      let w = w - xin - xin + 2 in
      let h = h - xin - xin + 2 in
      let gc = GCCache.get1 cache hilite in
      X.polyLine dpy win gc Origin [(w+x-2),(y+1); (x+1),(y+1);(x+1),(h+y-2);
        (w+x-2),(h+y-2); (w+x-2),(y+1)];
      let gc = GCCache.get1 cache shadow in
      X.polyLine dpy win gc Origin [(w+x-1),y;x,y;x,(h+y-1);
        (w+x-1),(h+y-1); (w+x-1),y]
  | ReliefInGroove ->
      let x = x + xin - 2 in
      let y = y + yin - 2 in
      let w = w -xin-xin + 2 in
      let h = h -xin-xin + 2 in
      let gc = GCCache.get1 cache shadow in
      X.polyLine dpy win gc Origin [(w+x-2), (y+1); (x+1),(y+1); (x+1), (h+y-2);
        (w+x-2), (h+y-2); (w+x-2), (y+1)];
      let gc = GCCache.get1 cache hilite in
      X.polyLine dpy win gc Origin [x, y; (w+x-1), y; x, (h+y-1);
        (w+x-1), (h+y-1); (w+x-1), y]
  
  | ReliefRaisedN n ->
      let gc = GCCache.get1 cache hilite in
      for i = 0 to n-1 do
        let x = x + i in
        let y = y + i in
        let w = w -i-i in
        let h = h -i-i in
        X.polyLine dpy win gc Origin [(w+x-1),y; x,y; x, (h+y-1)];
      done;
      let gc = GCCache.get1 cache shadow in
      for i = 0 to n-1 do
        let x = x + i in
        let y = y + i in
        let w = w -i-i in
        let h = h -i-i in
        X.polyLine dpy win gc Origin [(w+x-1),y; (w+x-1),(h+y-1); x,(h+y-1)]
      done
  | ReliefSunkenN n ->
      let gc = GCCache.get1 cache shadow in
      for i = 0 to n-1 do
        let x = x + i in
        let y = y + i in
        let w = w -i-i in
        let h = h -i-i in
        X.polyLine dpy win gc Origin [(w+x-1),y;x,y; x, (h+y-1)];
      done;
      let gc = GCCache.get1 cache hilite in
      for i = 0 to n-1 do
        let x = x + i in
        let y = y + i in
        let w = w -i-i in
        let h = h -i-i in
        X.polyLine dpy win gc Origin [x, (h+y-1); (w+x-1), (h+y-1);(w+x-1), y]
      done;
  | ReliefInRaisedN n ->
      let n = min xin n in
      let xin = xin - n in
      let x = x + xin in
      let y = y + yin in
      let w = w - xin - xin in
      let h = h - xin - xin in
      let gc = GCCache.get1 cache hilite in
      for i = 0 to n-1 do
        let x = x + i in
        let y = y + i in
        let w = w -i-i in
        let h = h -i-i in
        X.polyLine dpy win gc Origin [(w+x-1),y; x,y; x, (h+y-1)];
      done;
      let gc = GCCache.get1 cache shadow in
      for i = 0 to n-1 do
        let x = x + i in
        let y = y + i in
        let w = w -i-i in
        let h = h -i-i in
        X.polyLine dpy win gc Origin [(w+x-1),y; (w+x-1),(h+y-1); x,(h+y-1)]
      done;
  | ReliefInSunkenN n ->
      let n = min xin n in
      let xin = xin - n in
      let x = x + xin in
      let y = y + yin in
      let w = w - xin - xin in
      let h = h - xin - xin in
      let gc = GCCache.get1 cache shadow in
      for i = 0 to n-1 do
        let x = x + i in
        let y = y + i in
        let w = w -i -i in
        let h = h -i -i in
        X.polyLine dpy win gc Origin [(w+x-1),y;x,y; x, (h+y-1)];
      done;
      let gc = GCCache.get1 cache hilite in
      for i = 0 to n-1 do
        let x = x + i in
        let y = y + i in
        let w = w -i -i in
        let h = h -i -i in
        X.polyLine dpy win gc Origin [x, (h+y-1); (w+x-1), (h+y-1);(w+x-1), y]
      done
     
let drawRelief dpy w cache hilite shadow relief =
  let g = w.w_geometry in
  drawRelief2 dpy w.w_window cache 0 0 g.width g.height
  w.w_ipad_x w.w_ipad_y hilite shadow relief

let relief_size relief pad =
  max 0 (
    (match relief with
        ReliefFlat -> 0
      | ReliefRaised -> 1
      | ReliefSunken -> 1
      | ReliefRaisedN n -> n
      | ReliefSunkenN n -> n
      | ReliefInRaised -> 1
      | ReliefInSunken -> 1
      | ReliefInRaisedN n -> n
      | ReliefInSunkenN n -> n
      | _ -> 2)
    - pad)