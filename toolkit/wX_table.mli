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

class t :
  WX_types.container ->
  WX_types.base_attributes list ->
  int ->
  int ->
  bool ->
  object
    
    inherit WX_object.t
    
    val hints : WX_types.szhints array
    val wobs : (WX_types.contained * int * int * int * int) array array
    method container_add :
      WX_types.contained -> int -> int -> int -> int -> unit
  end
