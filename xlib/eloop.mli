(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xtypes

type display

val debug : bool ref
val mutex : Concur.Mutex.t  
val event_loop : unit -> unit

val add_display : Xtypes.display -> (xevent -> unit) -> display
val remove_display : display -> unit
val display : display -> Xtypes.display

val add_window : display -> window -> (xevent -> unit) -> unit
val remove_window : display -> window -> unit
val known_window : display -> window -> bool

val add_timer : display -> float -> (unit -> unit) -> unit

val last_event : display -> event
val last_time : display -> time
val event_time: time ref
val add_after_event_hook: (unit -> unit) -> unit
val add_after_events_hook: (unit -> unit) -> unit
val handle_event:  display -> Xtypes.xevent -> unit
val update_event_time: Xtypes.xevent -> unit
val update_time: display -> Xtypes.xevent -> unit
val handle_events: bool -> bool