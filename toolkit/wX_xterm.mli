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
type char_drawing = Xtypes.setGCattributes list
and xterm_gc = int
and xterm_event =
  | XTKeyPress of Xtypes.modifiers * string * Xtypes.keySym
  | XTResize of int * int
  | XTButtonPress of Xtypes.modifiers * int * int * int
  | XTMouseMotion of Xtypes.modifiers * int * int * int
and xterm_color = int
and xterm_font = int
and xterm_display =
  { dpy: Xtypes.display;
    root: Xtypes.window;
    visual: Xtypes.visual;
    colormap: Xtypes.colormap;
    depth: int;
    mutable f_width: int;
    mutable f_ascent: int;
    mutable f_height: int;
    windows: (Xtypes.window, unit -> unit) Hashtbl.t;
    dpy_pixels_names: string array;
    dpy_pixels: Xtypes.pixel array;
    mutable dpy_pixels_n: int;
    dpy_fonts_names: string array;
    dpy_fonts: Xtypes.font array;
    mutable dpy_fonts_n: int;
    mutable dpy_highlighted: int;
    root_oo: WX_root.t }
and xterm_window =
  { display: xterm_display;
    win: Xtypes.window;
    gc: Xtypes.gc;
    mutable gc_state: int;
    mutable ncols: int;
    mutable nlines: int;
    mutable table: string array;
    mutable gc_table: int array array;
    mutable modified: bool array;
    mutable region: string option;
    mutable handler: Xtypes.xevent -> unit }
val displays : xterm_display list ref
val create_display :
  WX_root.t -> string array -> string array -> xterm_display
type event_handler = xterm_window -> xterm_event -> unit
val get_color : xterm_display -> int -> Xtypes.pixel
val get_font : xterm_display -> int -> Xtypes.font
val gcattrs_from_attr : xterm_display -> int -> Xtypes.setGCattributes list
val make_attr : int -> int -> int -> bool -> int
val direct_attr : int
val inverse_attr : int
val default_handler : Xtypes.display -> Xtypes.xevent -> unit
val setSelection : xterm_window -> string -> unit
val getSelection : xterm_window -> string
val removeSelection : xterm_window -> unit
val draw_string :
  xterm_window -> int -> int -> string -> int -> int -> int -> unit
val clear_eol : xterm_window -> int -> int -> int -> unit
val clear : xterm_window -> unit
val changeGC : xterm_display -> Xtypes.gc -> int ref -> int -> unit
val expose_window : xterm_window -> unit -> unit
val create_window :
  xterm_display -> Xtypes.window -> int -> int -> xterm_window
val remove_expose : Xtypes.display -> Xtypes.window -> unit
val remove_motions :
  Xtypes.display -> Xtypes.window -> Xtypes.Xmotion.t -> Xtypes.Xmotion.t
val dirty_window : xterm_window -> unit
val update_displays : unit -> unit
val button : Xtypes.button ref
val xterm_handler :
  xterm_window -> (xterm_event -> unit) -> Xtypes.xevent -> unit
val install_handler :
  xterm_display -> xterm_window -> (xterm_event -> unit) -> unit
val list_removeq : 'a list -> 'a -> 'a list
val close_display : xterm_display -> unit
val lst_it : int ref
val destroy_window : xterm_window -> unit
val change_font : xterm_window -> string -> unit
val get_cutbuffer : xterm_window -> string
val set_cutbuffer : xterm_window -> string -> unit
val setHighlight : xterm_display -> int -> unit

class t :
  WX_types.container ->
  xterm_display ->
  int ->
  int ->
  object
    val display : xterm_display
    val ncols : int
    val nlines : int
    val mutable parent : WX_types.container
    val s : WX_types.screen_struct
    val mutable szhints : WX_types.szhints
    val mutable xterm : xterm_window option
    method check_abort : bool
    method configure : WX_types.base_attributes list -> unit
    method contained : WX_types.contained
    method destroy : unit
    method display : Xtypes.display
    method height : int
    method hide : unit
    method name : string
    method realize : unit
    method set_cutbuffer : string -> unit
    method set_parent : WX_types.container -> unit
    method show : unit
    method size_allocate : int -> int -> int -> int -> unit
    method size_request : WX_types.szhints
    method width : int
    method window : Xtypes.window
    method xterm : xterm_window
  end
