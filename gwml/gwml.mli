(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

type image = ScaleImage of string | TileImage of string | NoImage
and justified = Left | Center | Right
and binding =
  Key of Xtypes.keySym * Xtypes.modifiers
| Button of Xtypes.button * Xtypes.modifiers
| DblClick of Xtypes.button * Xtypes.modifiers
| BMove of Xtypes.button * Xtypes.modifiers
and click = Simple | Double | DeltaMove | Other
and bindings = (binding * (wob -> unit) * bool) list
and wob_event =
    WobInit
  | WobGetSize
  | WobCreate
  | WobResize of bool
  | WobMove
  | WobKeyPress of Xtypes.Xkey.t * string * Xtypes.keycode
  | WobButtonPress of Xtypes.Xbutton.t
  | WobUnmap of bool
  | WobMap
  | WobEnter
  | WobLeave of bool
  | WobDestroy
  | WobMapRequest
  | WobRefresh
  | WobIconifyRequest of bool
  | WobDeiconifyRequest of bool
  | WobWithdrawn
  | WobButtonRelease of Xtypes.Xbutton.t
  | WobPropertyChange of Xtypes.atom
  | WobSetInputFocus of bool
  | WobDeleteWindow
  | WobInstallColormap of bool
  | WobMessage of string
  | WobExitGwml
  | WobRaiseWindow
  | WobLowerWindow
  | WobClientFocus of bool
  | WobUpdateShape
  | WobClickInClient
and wob =
  { mutable w_window: Xtypes.window;
    w_screen: screen_desc;
    w_geometry: Xtypes.geometry;
    mutable w_env: Wobenv.t;
    w_oo: wob_desc;
    mutable w_queue: wob_event list;
    w_parent: wob;
    w_top: wob }
and cursor_desc =
    FontCursor of int
  | BitmapCursor of string * string
  | NoCursor
and bg_desc = BgColor of string | BgPixmap of Xtypes.pixmap
and screen_desc =
  { s_clients: (client_desc * wob) Wintbl.t;
    s_scr: Xtypes.screen;
    s_colors: (string, Xtypes.pixel) Hashtbl.t;
    s_pixmaps: (string, Xpm.pixmap) Hashtbl.t;
    s_fonts: (string, Xtypes.font * Xtypes.queryFontRep) Hashtbl.t;
    s_cursors: (cursor_desc, Xtypes.cursor) Hashtbl.t;
    s_scheduler: Eloop.display;
    mutable s_last_cmap: Xtypes.colormap;
    mutable s_cmap_wob: wob;
    mutable s_focus_wob: wob;
    mutable s_top_opening_hooks: (wob -> unit) list }
and client_desc =
  { c_window: Xtypes.window;
    c_screen: screen_desc;
    c_set_focus: bool;
    mutable c_colormap: Xtypes.colormap;
    mutable c_new_window: bool;
    c_geometry: Xtypes.geometry;
    mutable c_name: string;
    mutable c_icon_name: string;
    c_machine: string;
    c_class: string * string;
    mutable c_size_hints: Xtypes.wm_size_hints;
    mutable c_wm_hints: Xtypes.wm_hints;
    c_transient_for: Xtypes.window;
    mutable c_colormap_windows: Xtypes.window list;
    c_icon_size: Xtypes.wm_icon_size list;
    mutable c_wm_state: Xtypes.wmState;
    mutable c_wm_icon: Xtypes.window;
    mutable c_delete_window: bool;
    mutable c_take_focus: bool;
    mutable c_decorated: bool;
    mutable c_protocols: Xtypes.atom list;
    c_own_window: bool;
    mutable c_shaped: Shape.shapeQueryExtentsRep option }
and wob_desc =
  < actions : bindings; add_hook : hook -> unit; background : string;
    backimage : image; bg : string; bgimage : image; borderpixel : string;
    borderwidth : int; client : client_desc; create : bool -> unit;
    fg : string; first_hook : wob_event -> unit; font : string;
    foreground : string; handle_button : Xtypes.Xbutton.t -> unit;
    handle_key : Xtypes.Xkey.t * string * Xtypes.keySym -> unit;
    hilite : unit; hilite_background : string; hilite_backimage : image;
    hilite_foreground : string; hilitep : bool; is_shaped : bool;
    iter : (wob_desc -> unit) -> unit; last_hook : wob_event -> unit;
    mask : Xtypes.eventMask list; min_height : int; min_width : int;
    refresh : unit; resized : unit; reverse : bool; send : wob_event -> unit;
    set_actions : bindings -> unit; set_background : string -> unit;
    set_backimage : image -> unit; set_backpixmap : Xtypes.pixmap -> unit;
    set_borderpixel : string -> unit; set_borderwidth : int -> unit;
    set_cursor : cursor_desc -> unit; set_extensible_height : int -> unit;
    set_extensible_width : int -> unit; set_font : string -> unit;
    set_foreground : string -> unit; set_hilite : bool -> unit;
    set_hilite_background : string -> unit;
    set_hilite_backimage : image -> unit;
    set_hilite_foreground : string -> unit;
    set_mask : Xtypes.eventMask list -> unit; set_min_height : int -> unit;
    set_min_width : int -> unit; set_shaped : bool -> unit;
    set_wob : wob -> unit; unhilite : unit; update_bg : unit;
  update_fg : unit; wob : wob; wob_hooks : hook list;
  set_tip_display : (wob_desc -> bool -> unit) -> unit;
  xevents : Xtypes.xevent -> unit >
and hook = wob_event -> unit
and pixmap_desc =
    FromFile of string
  | FromFunction of string * (wob -> Xtypes.pixmap * Xtypes.pixmap)
  | FromData of string * Xpm.pixmap_data
and bar_desc = Horizontal | Vertical
val graphics_path : string list Options.option_record
val add_to_path : 'a list Options.option_record -> 'a -> unit
val screens : wob_desc array ref
val logp : bool Options.option_record
val display : Xtypes.display
val use_shape : bool
val use_imlib : bool Options.option_record
val screen_opening_hooks : (wob -> unit) list ref
val decorate_screen : (wob -> unit) ref
val decorate_client : (wob -> wob_desc -> unit) ref
val debug : bool Options.option_record
val debug_events : bool Options.option_record
val xdefaults : string
val x_res : string Xrm.t
val fixed_font : Xtypes.font * Xtypes.queryFontRep
val font_make : wob -> string -> Xtypes.font * Xtypes.queryFontRep
val hex : char -> int
type color_spec = NamedColor of string | RgbColor of int * int * int
val parse_color : string -> color_spec
val color_make : wob -> string -> Xtypes.pixel
val exec_hooks : 'a -> ('a -> unit) list -> unit
val ungrab_hooks : (unit -> unit) list ref
val grabServer : unit -> unit
val ungrabServer : unit -> unit
val ungrab_exec : (unit -> unit) -> unit
val pixmap_make : wob -> pixmap_desc -> Xpm.pixmap
val cursor_make : wob -> cursor_desc -> Xtypes.cursor
val xa_wm_protocols : Xtypes.atom
val xa_wm_colormap_windows : Xtypes.atom
val xa_wm_state : Xtypes.atom
val xa_wm_take_focus : Xtypes.atom
val xa_wm_delete_window : Xtypes.atom
val xa_wm_change_state : Xtypes.atom
val print_event : wob_event -> string
val top_cursor : Xtypes.cursor
val bottom_cursor : Xtypes.cursor
val left_cursor : Xtypes.cursor
val right_cursor : Xtypes.cursor
val root_position : wob -> int * int
type wm_event = client_desc * wob * client_event
and client_event =
    AddClient
  | RemoveClient
  | ClientResize
  | ClientMove
  | ClientUnmap
  | ClientMap
  | ClientIconify
  | ClientDeiconify
  | ClientPropertyChange of Xtypes.atom
  | ClientColormap of bool
  | ClientFocus of bool
val broadcast_targets : (wm_event -> unit) list ref
val wm_broadcast : wm_event -> unit
val prevent_animation : bool ref
val final_actions : (unit -> unit) list ref
val config_loaded : bool ref
val is_mapped: bool Wobenv.var
val delay: float -> unit