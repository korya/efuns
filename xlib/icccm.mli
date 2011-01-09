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


                    open Xtypes
open Display

val getStringProperty : display -> window -> atom -> string
val getStringListProperty : display -> window -> atom -> string list
val getEnum32Property : display -> window -> atom -> atom -> 'a list

val getWM_NAME : display -> window -> string
val getWM_ICON_NAME : display -> window -> string
val getWM_CLIENT_MACHINE : display -> window -> string
val getWM_CLASS : display -> window -> string list
val getWM_COMMAND : display -> window -> string list
val getWM_SIZE_HINTS : display -> window -> atom -> wm_size_hints
val getWM_HINTS : display -> window -> atom -> wm_hints
val getWM_TRANSIENT_FOR : display -> window -> window
val getWM_PROTOCOLS : display -> window -> atom list
val getWM_COLORMAP_WINDOWS : display -> window -> window list
val getWM_ICON_SIZE : display -> window -> wm_icon_size list
val getWM_STATE : display -> window -> wmState * window

val safe_getWM_SIZE_HINTS : display -> window -> wm_size_hints
val safe_getWM_HINTS : display -> window -> wm_hints


val setStringProperty : display -> window -> atom -> string -> unit
val setStringListProperty : display -> window -> atom -> string list -> bool -> unit
val setEnum32Property : display -> window -> atom -> atom -> 'a list -> unit

val newWM_SIZE_HINTS : unit -> wm_size_hints
val newWM_HINTS : unit -> wm_hints

val setWM_NAME : display -> window -> string -> unit
val setWM_ICON_NAME : display -> window -> string -> unit
val setWM_CLIENT_MACHINE : display -> window -> string -> unit
val setWM_CLASS : display -> window -> string list -> unit
val setWM_COMMAND : display -> window -> string list -> unit
val setWM_SIZE_HINTS : display -> window -> atom -> wm_size_hints -> unit
val setWM_NORMAL_HINTS : display -> window -> wm_size_hints -> unit
val setWM_HINTS : display -> window -> wm_hints -> unit

val setWM_TRANSIENT_FOR : display -> window -> window -> unit
val setWM_PROTOCOLS : display -> window -> atom list -> unit
val setWM_COLORMAP_WINDOWS : display -> window -> window list -> unit
val setWM_STATE : display -> window -> wmState * window -> unit
val setWM_ICON_SIZE : display -> window -> wm_icon_size list -> unit

val withdrawWindow : display -> window -> window -> unit
val iconifyWindow : display -> window -> window -> unit
