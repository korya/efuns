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

type top_internals =
  { mutable center: Gwml.wob option;
    mutable left: Gwml.wob option;
    mutable right: Gwml.wob option;
    mutable title: Gwml.wob option;
    mutable bottom: Gwml.wob option;
    borders: Xtypes.geometry }
val client_mask : Xtypes.eventMask list
val top_mask : Xtypes.eventMask list
val notify_mode : Xtypes.notifyMode -> string
val notify_detail : Xtypes.notifyDetail -> string
val ssend : Gwml.wob option -> Gwml.wob_event -> unit
val broadcast : top_internals -> Gwml.wob_event -> unit
val no_borders_with_shape : bool Options.option_record
val set_shape : Gwml.wob -> top_internals -> unit
class top :
  top_internals ->
  string ->
  object
    inherit Wob.wob_base
    val deco : top_internals
    method set_client : Gwml.client_desc -> unit
    method deco : top_internals
  end
val last_hook : 'a -> 'b -> unit
val make :
  Gwml.wob ->
  string ->
  (top -> Gwml.hook) list ->
  Gwml.wob_desc ->
  Gwml.wob_desc option ->
  Gwml.wob_desc option -> Gwml.wob_desc option -> Gwml.wob_desc option -> top
val resize : Gwml.wob -> int -> int -> unit
val resize_top : Gwml.wob -> int -> int -> unit
type window_desc =
  (top -> Gwml.wob_event -> unit) list * Gwml.wob_desc option *
  Gwml.wob_desc option * Gwml.wob_desc option * Gwml.wob_desc option
