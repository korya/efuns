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

(*
  This is the style widget. It is normally responsible for most drawing
functions. Drawing widgets will use a WX_style.t widget to draw when
needed. By adding a new WX_style widget, drawing widgets may appear  
different.
*)

(* This code is inspired from Gtk *)

open Xtypes
open WX_types

type shadow_type =
  ShadowNone
| ShadowIn
| ShadowOut
| ShadowEtchedIn
| ShadowEtchedOut

type state_type = int
let (stateNormal : state_type) = 0
let (stateactive : state_type) = 1
let (statePrelight : state_type) = 2
let (stateSelected : state_type) = 3
let (stateInsensitive : state_type) = 4

  (* We should use a particular table (in root) for background colors, such
  that GCs for background colors would be pre-computed. 
  
  *)

type style = {
    fg : color;
    bg : color;
    light : color;
    dark : color;
    mid : color;
  }
  
  
type gc_values = {
    light_gc : gc;
    bg_gc : gc;
    dark_gc : gc;
    black_gc : gc; 
  }
  
  
class t (root : WX_root.t) =
  object
  val s = root#screen
  val r = root#window
  val d = root#display
  
  val mutable gcs = [||]
    
  method create_gcs =
    gcs <- Array.init 5 (fun i ->
        {
          bg_pixel = -1;
          light_gc = X.createGC d r [];
          bg_gc = X.createGC d r [];
          dark_gc = X.createGC d r [];
          black_gc = X.createGC d r [];
        })
    
  method update_gc v color = 
    if v.bg_pixel <> color.c_pixel then
      begin
        v.bg_pixel <- color.c_pixel;
      end
    
  method draw_diamond window bg_color state shadow x y width height =
    
    
    let half_width = width / 2 in
    let half_height = height / 2 in
    ()

  
end