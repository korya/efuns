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

open Xtypes
open WX_types
  
type options = {
    mutable labels : string array;  
    mutable valides : bool array;
    mutable current : int;
    mutable change_hook : (unit -> unit) list;
  }  
(* Comment faire pour se passer de root ? *)
  
  
class orig parent root options attributes =
  object (self)
  
  inherit WX_button.orig parent attributes

  val options = options
  
  initializer
  let labels = options.labels in
  let label = new WX_label.t self#container labels.(options.current) [] in
  self#container_add label#contained;
  let menu_desc = Array.mapi (fun i s -> (s, fun _ ->
            label#set_options [WX_label.String s];
            options.current <- i;
            List.iter (fun f -> f ()) options.change_hook)
    ) labels in
  let menu = new WX_popup.t root menu_desc in

  self#set_action (fun _ -> 
      let (x,y) = self#root_coordinates in
      menu#popup x y (Some !button_event)
      );
  self#set_wait_release false;
  
  method name = "selector"  
end

class t = orig