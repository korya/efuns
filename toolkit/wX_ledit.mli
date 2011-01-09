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

class orig :
  WX_types.container ->
  string ->
  WX_types.base_attributes list ->
  object
    inherit WX_label.orig
    val mutable subjects : (unit -> unit) list
    method add_subject : (unit -> unit) -> unit
  end

class t :
  WX_types.container ->
  string ->
  WX_types.base_attributes list ->
  object
    inherit WX_label.t
    method add_subject : (unit -> unit) -> unit
  end
