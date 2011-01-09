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

val linear : Gwml.wob -> Xtypes.geometry -> Xtypes.geometry -> unit
val linear_move : Gwml.wob -> Xtypes.geometry -> Xtypes.geometry -> unit
val linear_transform : Gwml.wob -> Xtypes.geometry -> Xtypes.geometry -> unit
val tv_close : Gwml.wob -> unit
val twist_resize : Gwml.wob -> Xtypes.geometry -> Xtypes.geometry -> unit
val animation_list : (
    string * (Gwml.wob -> Xtypes.geometry -> Xtypes.geometry -> unit)) list
val resize_anim : 
  (Gwml.wob -> Xtypes.geometry -> Xtypes.geometry -> unit) 
  Options.option_record
val animation_menu : unit -> (string * 'a list * Stdconfig.action) list