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
val openDisplay : string -> Xtypes.display
val closeDisplay : Xtypes.display -> unit
val defaultRoot : Xtypes.display -> Xtypes.window
val defaultScreen : Xtypes.display -> Xtypes.screen
val defaultWhite : Xtypes.display -> Xtypes.pixel
val defaultBlack : Xtypes.display -> Xtypes.pixel
val defaultColormap : Xtypes.display -> Xtypes.colormap
val defaultDepth : Xtypes.display -> int
val nextEvent : Xtypes.display -> Xtypes.xevent
val nextEventWait : Xtypes.display -> bool
val checkEvent : Xtypes.display -> Xtypes.xevent
val peekEventWait : Xtypes.display -> bool
val readEvent : Xtypes.display -> Xtypes.xevent
val peekEvent : Xtypes.display -> Xtypes.xevent
val putBackEvent : Xtypes.display -> Xtypes.xevent -> unit
val cleanEvents : Xtypes.display -> unit
val actions : int ref
val checkPredEvent :
  Xtypes.display -> (Xtypes.xevent -> bool) -> bool -> Xtypes.xevent option
val removePredEvent :
  Xtypes.display -> (Xtypes.xevent -> bool) -> unit
val waitPredEvent :
  Xtypes.display -> (Xtypes.xevent -> bool) -> bool -> Xtypes.xevent
val checkTypedEvent :
  Xtypes.display -> Xtypes.eventType list -> bool -> Xtypes.xevent option
val checkTypedWindowEvent :
  Xtypes.display ->
  Xtypes.window -> Xtypes.eventType list -> bool -> Xtypes.xevent option
val createSimpleWindow :
  Xtypes.display ->
  Xtypes.window ->
  int ->
  int ->
  int -> int -> int -> Xtypes.setWindowAttributes list -> Xtypes.window
val moveResizeWindow :
  Xtypes.display -> Xtypes.window -> int -> int -> int -> int -> unit
val raiseWindow : Xtypes.display -> Xtypes.window -> unit
val lowerWindow : Xtypes.display -> Xtypes.window -> unit
val selectInput :
  Xtypes.display -> Xtypes.window -> Xtypes.eventMask list -> unit Jeton.t
val moveWindow : Xtypes.display -> Xtypes.window -> int -> int -> unit
val resizeWindow : Xtypes.display -> Xtypes.window -> int -> int -> unit
val clearWindow : Xtypes.display -> Xtypes.window -> unit
val drawSubString :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> string -> int -> int -> unit
val drawString :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> string -> unit
val imageSubString :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> string -> int -> int -> unit
val imageString :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> string -> unit
val drawRectangle :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> int -> int -> unit
val drawSegment :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> int -> int -> unit
val drawLine :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> int -> int -> unit
val drawArc :
  Xtypes.display ->
  Xtypes.window ->
  Xtypes.gc -> int -> int -> int -> int -> int -> int -> unit
val drawPoint :
  Xtypes.display -> Xtypes.window -> Xtypes.gc -> int -> int -> unit
val fillRectangle :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> int -> int -> unit
val xerror_to_string : Xtypes.errorEvent -> string
val printXError : Xtypes.errorEvent -> unit
val createFontCursor : Xtypes.display -> int -> Xtypes.cursor
val getWholeProperty :
  Xtypes.display -> Xtypes.window -> Xtypes.atom -> Xtypes.getPropertyRep
val listProperties :
  Xtypes.display ->
  Xtypes.window -> (string * string * int * int * string) list
val setForeground : Xtypes.display -> Xtypes.gc -> Xtypes.pixel -> unit
val set_line_style :
  Xtypes.display ->
  Xtypes.gc ->
  int -> Xtypes.lineStyle -> Xtypes.capStyle -> Xtypes.joinStyle -> unit
