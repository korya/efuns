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

let path = ref [
    "/usr/share/icons/";
    "/usr/share/wmakerconf/";
    "/usr/share/WindowMaker/Icons";
    "/usr/share/toolbar";
    "/usr/share/pixmaps/";
    "/usr/share/afterstep/desktop/buttons/";
    "/usr/share/afterstep/desktop/icons/16bpp";
    "/usr/share/afterstep/desktop/icons/8bpp";
    "/usr/share/afterstep/desktop/icons/common";
  ]
  
let pixmap = ref "xterm.xpm"
let command = ref "xterm"
let x=  ref 0
let y = ref 0
let dx = ref 50
let dy = ref 50

let window = ref noWindow
  
let pixmap_from_command = ref false
  
let _ =
  Arg.parse [
    "-pixmap",Arg.String (fun s -> pixmap := s), "<pixmap>: the pixmap to use";
    "-command", Arg.String (fun s -> command := s), "<command>: the command to launch";
    "-x", Arg.Int (fun i -> x:= i), "<x>: x coordinate";
    "-y", Arg.Int (fun i -> y:= i), "<y>: y coordinate";
    "-I", Arg.String (fun s -> path := s :: !path), "<dir>: add dir to pixmap path";
    "-C", Arg.Set pixmap_from_command, ": use command name for icon";
    "-dx", Arg.Int (fun i -> dx := i), "<dx> : width";
    "-dy", Arg.Int (fun i -> dy := i), "<dy> : height";
    "-window", Arg.Int (fun w -> window := w), "<w> : window to reparent";    
  ] (fun _ -> failwith "No anonymous argument") 
  "<gwml_button>: a simple utility to display an icon, and launch a command
 when clicked."

let _ =
  if !pixmap_from_command then pixmap := !command ^ ".xpm"
    
let g = root#geometry
let x = if !x < 0 then g.width - !x  else !x
let y = if !y < 0 then g.height - !y  else !y
    
let dx = if !dx > g.width then g.width else !dx
let dy = if !dy > g.height then g.height else !dy
    
let x = if x + dx > g.width then g.width - dx else x
let y = if y + dy > g.height then g.height - dy else y
    
let top = new WX_wmtop.t root [Position (x,y)] 
let button = new WX_button.t top#container [
    MinWidth 50; MinHeight 50;
    MaxWidth dx; MaxHeight dy;
    ]
let pixmap =
  if !window == noWindow then
    let pixmap = Utils.find_in_path !path !pixmap in
    let pixmap = new WX_pixmap.t button#container
        (pixmap, FromFile pixmap) [] in
      pixmap#contained
  else
  let parent = new WX_parent.t button#container [] in
  parent#container_add !window;
  parent#contained
  
let _ =
  button#set_action (fun _ ->
      match Unix.fork () with
        0 -> 
          ignore (Unix.setsid ());
          ignore (Unix.setgid (Unix.getpid ()));
          let args = [| "/bin/sh"; "-c"; !command |] in
          Unix.execvp args.(0) args
      | _ -> ()
  );
  button#container_add pixmap;
  top#container_add button#contained;
  top#setWM_SIZE_HINTS (let sh = Icccm.newWM_SIZE_HINTS () in
    sh.user_position <- true; sh);
  top#setWM_CLASS "GwML" "GwML";
  top#show;
  loop ()