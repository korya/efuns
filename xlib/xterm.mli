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

type xterm_gc = int
and xterm_event =
| XTKeyPress of Xtypes.modifiers * string * Xtypes.keySym
| XTResize of int * int
| XTButtonPress of Xtypes.modifiers * int * int * int
| XTMouseMotion of Xtypes.modifiers * int * int * int

type xterm_color = int
and xterm_font = int
and xterm_display
and xterm_window
and event_handler = xterm_window -> xterm_event -> unit

val get_color : xterm_display -> int -> Xtypes.pixel
val get_font : xterm_display -> int -> Xtypes.font
val gcattrs_from_attr : xterm_display -> int -> Xtypes.setGCattributes list
val make_attr : int -> int -> int -> bool -> int
val direct_attr : int
val inverse_attr : int
val create_display :
  string -> string array -> string array -> string -> string -> xterm_display
val draw_string :
  xterm_window -> int -> int -> string -> int -> int -> int -> unit
val clear_eol : xterm_window -> int -> int -> int -> unit
val clear : xterm_window -> unit
val changeGC : xterm_display -> Xtypes.gc -> int ref -> int -> unit
val expose_window : xterm_window -> unit -> unit
val create_window :
  xterm_display -> string -> 'a -> int -> int -> xterm_window
val remove_expose : Xtypes.display -> Xtypes.window -> unit
val dirty_window : xterm_window -> unit
val update_displays : unit -> unit
val button : Xtypes.button ref
val xterm_handler :
  xterm_window -> (xterm_event -> unit) -> Xtypes.xevent -> unit
val install_handler :
  xterm_display -> xterm_window -> (xterm_event -> unit) -> unit
val close_display : xterm_display -> unit
val lst_it : int ref
val event_loop : unit -> unit
val destroy_window : xterm_window -> unit
val change_font : xterm_window -> string -> unit
val create_scrollbar : xterm_window -> int -> int -> int -> int -> unit
val set_name : xterm_window -> string -> unit
val get_cutbuffer : xterm_window -> string
val set_cutbuffer : xterm_window -> string -> unit
val setSelection : xterm_window -> string -> unit
val getSelection : xterm_window -> string
val setHighlight : xterm_display -> int -> unit
