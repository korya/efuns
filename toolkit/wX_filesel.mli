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
type info =
  { filter: string;
    current_selection: string;
    predicat: info -> bool;
    mutable action: string -> unit;
    mutable cancel: unit -> unit }
class t :
  WX_root.t ->
  info ->
  WX_types.base_attributes list ->
  object
    inherit WX_deleg.wx_object
    inherit WX_deleg.wmtop
  end
