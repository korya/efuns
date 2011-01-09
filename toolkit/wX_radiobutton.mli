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
  WX_adjust.t ->
  int ->
  WX_types.base_attributes list ->
  object
    inherit WX_button.orig
    
    val adj : WX_adjust.t
    val number : int
    method update_me : unit -> unit
    method select : unit
end

class t :
  WX_types.container ->
  WX_adjust.t ->
  int ->
  WX_types.base_attributes list ->
  object
    inherit WX_button.orig
    
    method update_me : unit -> unit
    method select : unit
end
