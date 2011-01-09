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
type options =
  { mutable labels: string array;
    mutable valides: bool array;
    mutable current: int;
    mutable change_hook: (unit -> unit) list }

class orig :
  WX_types.container ->
  WX_root.t ->
  options ->
  WX_types.base_attributes list ->
  object
    inherit WX_button.orig

    val options : options
  end

class t :
  WX_types.container ->
  WX_root.t ->
  options ->
  WX_types.base_attributes list ->
  object
    inherit WX_button.t
  end
