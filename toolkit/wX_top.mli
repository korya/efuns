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
  WX_root.t ->
  (int * int) option ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    
    val mutable widget : WX_types.contained option
    method change_position : int -> int -> unit
    method container_add : WX_types.contained -> unit
    method grab_pointer : unit
    method top : WX_types.container
  end

class t :
  WX_root.t ->
  (int * int) option ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    
    method change_position : int -> int -> unit
    method container_add : WX_types.contained -> unit
    method grab_pointer : unit
    method top : WX_types.container
  end
