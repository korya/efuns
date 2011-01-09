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

(* The simplest calculator you have ever seen ... *)

open Xtypes
open WX_types
  
let display = new WX_display.t ""
let root = new WX_root.t display 0
let top = new WX_wmtop.t root []
let vbar = new WX_bar.v top#container []
let ledit = new WX_label.t vbar#container "0" [ExpandX true]
  
let keys = [
    [ "M+"; "M-"; "CM"; "RM"];
    [ "7";"8";"9";"*"];
    [ "4";"5";"6";"/"];
    [ "1";"2";"3";"-"];
    [ "0";".";"+/-";"+"];
    [ "ON"; "OFF";"INV";"="];
    ]

let op = ref "+"
let arg = ref 0.0
let value = ref 0.0
let dot = ref 0
let mem = ref 0.0

let set v = value := v; dot := 0; ledit#set_string (string_of_float !value)
  
let do_op () =
  match !op with
    "+" -> set (!arg +. !value)
  | "-" -> set (!arg -. !value)
  | "*" -> set (!arg *. !value)
  | "/" -> 
      set (if !value = 0. then 0. else !arg /. !value)
  | _ -> assert false
      
let rec decimal n =
  if n > 0 then 0.1 *. (decimal (n-1)) else 1.
  
let action name () =
  match name with
    "OFF" -> exit 0
  | "ON" -> set 0.; arg := 0.; op := "+"
  | "M+" -> mem := !mem +. !value
  | "M-" -> mem := !mem -. !value
  | "CM" -> mem := 0.
  | "RM" -> set !mem
  | "*" | "+" | "-" | "/" -> do_op (); arg := !value; op := name; set 0.
  | "=" -> do_op ()
  | "+/-" -> set (-. !value)
  | "INV" -> if !value = 0.0 then set 0. else set (1. /. !value)
  | "." -> dot := 0
  | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ->
      let x = int_of_string name in
      let x = float_of_int x in
      if !dot > 0 then (set (!value +. x *. (decimal !dot)); incr dot)
      else set (!value *. 10. +. x)
  | _ -> ()
        
let _ =
  top#container_add vbar#contained;
  top#configure [Bindings [KeyPress, (fun _ ->
          action (if !key_sym = XK.xk_Return then "=" else
              !key_string) ()
          )]];
  vbar#container_add ledit#contained;
  List.iter (fun list ->
      let hbar = new WX_bar.h vbar#container [] in
      List.iter (fun name ->
          let button = new WX_button.t hbar#container
            [MinWidth 30; MinHeight 30] in
          button#set_action (action name);
          let label = new WX_label.t button#container name [] in
          label#set_justification Center;
          button#container_add label#contained;
          hbar#container_add button#contained;
      ) list;
      vbar#container_add hbar#contained;
  ) keys;
  ledit#set_justification Right;  
  top#show;
  loop ()