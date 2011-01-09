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
type menu = item array
and item = label * action
and label = string
and action = WX_types.handler
and submenu =
  < configure : WX_types.base_attributes list -> unit; hide : unit;
    popup : int -> int -> unit; show : unit >
class t :
  WX_root.t ->
  (string * action) array ->
  object
    val mutable button : int option
    val mutable desc : (string * action) array
    val mutable once : bool
    val mutable realization : WX_top.t option
    val root : WX_root.t
    method change : (string * action) array -> unit
    method popup : int -> int -> int option -> unit
    method popup_once : int -> int -> int option -> unit
  end
