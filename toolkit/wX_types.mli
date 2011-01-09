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
val max : int -> int -> int
val min : int -> int -> int
type handler = unit -> unit
val drawLine :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> int -> int -> unit
val relieveRectangle :
  Xtypes.display ->
  Xtypes.window ->
  Xtypes.gc ->
  int -> int -> int -> int -> Xtypes.pixel -> Xtypes.pixel -> unit
val relieveHalfRectangle :
  Xtypes.display ->
  Xtypes.window ->
  Xtypes.gc ->
  int -> int -> int -> int -> Xtypes.pixel -> Xtypes.pixel -> unit
val pixmap_path : string list ref
val double_click_delay : float ref
val delta_move_size : int ref
val default_borderpixel : string ref
val default_background : string ref
val default_foreground : string ref
val default_font : string ref
module GCCache :
  sig
    val size : int
    type t =
      { gcs: Xtypes.gc array;
        ids: int array;
        mutable next: int;
        dpy: Xtypes.display;
        root: Xtypes.window }
    val create : Xtypes.display -> Xtypes.screen -> t
    val get3 : t -> Xtypes.pixel -> Xtypes.pixel -> Xtypes.font -> Xtypes.gc
    val get2 : t -> Xtypes.pixel -> Xtypes.pixel -> Xtypes.gc
    val get1 : t -> Xtypes.pixel -> Xtypes.gc
  end
type event_desc =
  | EnterWindow
  | LeaveWindow
  | ButtonPress
  | KeyPress
  | ButtonReleased
  | ButtonMotion
  | Key of Xtypes.keySym * Xtypes.modifiers
  | Button of Xtypes.button * Xtypes.modifiers
  | GrabbedKey of Xtypes.keySym * Xtypes.modifiers
  | GrabbedButton of Xtypes.button * Xtypes.modifiers
  | FocusIn
  | FocusOut
val autorepeat_delay : float ref
val autorepeat_rate : float ref
type click = | Simple | Double | DeltaMove | Other
and justified = | Left | Center | Right
and bar_desc = | Horizontal | Vertical
and color = { c_pixel: Xtypes.pixel; c_red: int; c_green: int; c_blue: int }
and relief =
  | ReliefFlat
  | ReliefSunken
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
and color_desc = | Namedcolor of string | RgbColor of int * int * int
and pixmap_desc =
  | FromFile of string
  | FromFunction of Xtypes.pixmap * Xtypes.pixmap
  | FromData of Xpm.pixmap_data
and cursor_desc =
  | FontCursor of int
  | BitmapCursor of string * string
  | NoCursor
and font =
  { font_name: string;
    font_id: Xtypes.font;
    font_info: Xtypes.queryFontRep;
    font_ascent: int;
    font_width: int;
    font_height: int }
and cursor = { curs_desc: cursor_desc; curs_id: Xtypes.cursor }
val noCursor : cursor
val noColor : color
type szhints =
  { mutable border_width: int;
    mutable requested_width: int;
    mutable requested_height: int;
    mutable min_width: int;
    mutable min_height: int;
    mutable max_width: int;
    mutable max_height: int;
    mutable expand_x: bool;
    mutable expand_y: bool;
    mutable retract_x: bool;
    mutable retract_y: bool;
    mutable inc_x: int;
    mutable inc_y: int;
    mutable comp_timestamp: int }
and op = | Append | Replace | Remove
and base_attributes =
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
  | Background of string
  | Foreground of string
  | BorderColor of string
  | BorderWidth of int
  | Cursor of cursor_desc
  | EventMask of op * Xtypes.eventMask list
  | Relief of relief
  | Bindings of (event_desc * handler) list
and contained =
  < configure : base_attributes list -> unit; destroy : unit; height : 
    int; hide : unit; name : string; realize : unit;
    set_parent : container -> unit; show : unit;
    size_allocate : int -> int -> int -> int -> unit; size_request : 
    szhints; width : int >
and resize_widget = < update_size : unit >
and refresh_widget = < refresh : unit >
and screen_struct =
  { s_display: Xtypes.display;
    s_screen: Xtypes.screen;
    s_colors: (color_desc, color) Hashtbl.t;
    s_default_color: color;
    s_pixmaps: (string, Xpm.pixmap) Hashtbl.t;
    s_fonts: (string, font) Hashtbl.t;
    s_default_font: font;
    s_cursors: (cursor_desc, cursor) Hashtbl.t;
    s_gcs: GCCache.t;
    mutable s_timestamp: int;
    mutable s_wait_resize: resize_widget list;
    mutable s_wait_refresh: refresh_widget list;
    mutable s_last_click: Xtypes.Xbutton.t;
    s_eloop: Eloop.display }
  
and container =
  < color_make : string -> bool -> color;
    cursor_make : cursor_desc -> bool -> cursor; default_font : font;
    display : Xtypes.display; font_make : string -> bool -> font;
    geometry : Xtypes.geometry; getHilite : color -> color;
    getShadow : color -> color; name : string;
    pixmap_make : string * pixmap_desc -> Xpm.pixmap; screen : screen_struct;
    update_top_size : unit; wait_resize : unit; window : Xtypes.window; >
and container_add =
  < color_make : string -> bool -> color;
    cursor_make : cursor_desc -> bool -> cursor; default_font : font;
    display : Xtypes.display; font_make : string -> bool -> font;
    geometry : Xtypes.geometry; getHilite : color -> color;
    getShadow : color -> color; name : string;
    pixmap_make : string * pixmap_desc -> Xpm.pixmap; screen : screen_struct;
    update_top_size : unit; wait_resize : unit; window : Xtypes.window;
    container_add : contained -> unit;
  >
  
and window =
  { mutable w_window: Xtypes.window;
    mutable w_shown: bool;
    mutable w_clear: bool;
    w_geometry: Xtypes.geometry;
    mutable w_override: bool;
    mutable w_background: color;
    mutable w_relief_colors: (color * color) option;
    mutable w_relief: relief;
    mutable w_borderpixel: color;
    mutable w_cursor: cursor;
    mutable w_mask: Xtypes.eventMask list;
    mutable w_size_timestamp: int;
    mutable w_refresh_timestamp: int;
    mutable w_foreground: color;
    mutable w_inverse: bool;
    mutable w_enter_window: handler;
    mutable w_leave_window: handler;
    mutable w_button_press: handler;
    mutable w_key_press: handler;
    mutable w_button_released: handler;
    mutable w_button_motion: handler;
    mutable w_focus_in: handler;
    mutable w_focus_out: handler;
    mutable w_actions: (event_desc * handler) list;
    mutable w_ipad_x: int;
    mutable w_ipad_y: int;
    mutable w_fill_x: bool;
    mutable w_fill_y: bool;
    mutable w_size_modified: bool }
val hex : char -> int
val loop : unit -> unit
val not_grab_related : Xtypes.notifyMode -> bool
val last_click : Xtypes.Xbutton.t ref
val delta_move_size : int ref
val set_grabs :
  Xtypes.display -> Xtypes.window -> (event_desc * 'a) list -> unit
val unset_grabs :
  Xtypes.display -> Xtypes.window -> (event_desc * 'a) list -> unit
val mouse_x_event : int ref
val mouse_y_event : int ref
val button_event : int ref
val key_string : string ref
val key_sym : int ref
val modifiers_event : int ref
type null = <  >
val null_handler : unit -> unit
val default_click : Xtypes.Xbutton.t
val drawRelief2 :
  Xtypes.display ->
  Xtypes.window ->
  GCCache.t ->
  int ->
  int ->
  int -> int -> int -> int -> Xtypes.pixel -> Xtypes.pixel -> relief -> unit
val drawRelief :
  Xtypes.display ->
  window -> GCCache.t -> Xtypes.pixel -> Xtypes.pixel -> relief -> unit
val relief_size : relief -> int -> int
