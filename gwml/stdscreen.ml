(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


open Xtypes
open Xlib
open Gwml
open Stdconfig
open Stddeco
  
let client = Wobenv.new_var ()

let screen_keys = ref (fun sw k -> ())
let screen_buttons = ref (fun sw e -> ())
let s_hook desc e =
  let sw = desc#wob in
  match e with
    
    WobNewClient c ->
      let deco = find_decoration c in
      let (hooks, left, right, top, bottom) = deco sw c in
      let tw = Client.decorate sw hooks c left right top bottom in
      Wob.setenv tw#wob client c;
      Utils.catchexn "WobMapRequest" (fun _ -> Wob.send tw#wob WobMapRequest);
  
  | _ -> ()
