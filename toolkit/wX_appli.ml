(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open WX_types

class t (root : WX_root.t) attributes =
  let top = new WX_wmtop.t root attributes in
  let vbar = new WX_bar.v top#container [] in
  let hbar = new WX_bar.h vbar#container [Relief ReliefRaised] in
  let port = new WX_bar.v vbar#container [] in
  let _ = 
    top#container_add vbar#contained;
    vbar#container_add_s [hbar#contained; port#contained];
  in
  object (self)
    
    (*
    inherit WX_deleg.base (top :> WX_base.t)
    inherit WX_deleg.wx_object (top :> WX_object.t)
  *)
    inherit WX_deleg.wmtop top
    inherit WX_deleg.container_add (port :> container_add)
    
  val root = root
  val top = top
  val port = port
  val hbar = hbar
    
  method add_menu name menu_desc =
    let menu = new WX_popup.t root menu_desc in
    let button = new WX_button.t hbar#container [] in
    let label = new WX_label.t button#container name [MinWidth 50] in
    label#set_justification Center;
    button#container_add label#contained;  
    button#set_wait_release false;
    button#set_action (fun _ -> 
        let (x,y) = button#root_coordinates in
        menu#popup x (y + button#height) (Some !button_event)
        );
    hbar#container_add button#contained

  method add_button name action =
    let button = new WX_button.t hbar#container [] in
    let label = new WX_label.t button#container name [MinWidth 50] in
    label#set_justification Center;    
    button#container_add label#contained;  
    button#set_wait_release false;
    button#set_action (action button);
    hbar#container_add button#contained

  method add_separator =
    let port = new WX_port.t hbar#container [ExpandX true] in
    hbar#container_add port#contained
  
  method configure = top#configure    
end
