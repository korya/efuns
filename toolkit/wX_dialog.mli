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
  WX_root.t ->
  WX_text.SimpleText.text ->
  WX_types.base_attributes list ->
  object

    inherit WX_deleg.wx_object
      inherit WX_deleg.top
      inherit WX_deleg.container_add

    val hbar : WX_bar.h
    val text : WX_text.of_string

    method add_button : string -> (unit -> unit) -> unit
    method set_text :
      WX_text.SimpleTree.tree WX_text.SimpleTree.tree_desc -> unit

 end
