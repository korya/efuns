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

open WX_types

class t (root : WX_root.t) lines attributes =
  let top = new WX_top.t root None attributes in
  let vbar = new WX_bar.v top#container [] in
  let text = new WX_text.of_string vbar#container lines [] in
  let hbar = new WX_bar.h vbar#container [IpadX 5; IpadY 10] in
  let _ =
    top#container_add vbar#contained;
    vbar#container_add_s [text#contained; hbar#contained];
  in
  
  object (self)

    inherit WX_deleg.wx_object (top :> WX_object.t)    

  inherit WX_deleg.top top
  inherit WX_deleg.container_add (hbar :> container_add)

  val text = text
  val hbar = hbar
  
  method set_text = text#set_text
  method add_button name action =
    let button = new WX_button.t hbar#container [] in
    let label = new WX_label.t button#container name [MinWidth 70] in
    button#set_action action;
    button#container_add label#contained;
    hbar#container_add button#contained

end