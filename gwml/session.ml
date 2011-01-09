open Gwml;;
  
let s = !screens.(0);;
let sw = s#wob.w_screen;;

let clients = ref [];;
  
let _ = Hashtbl.iter (fun w1 (c,_) ->
      if c.c_window = w1 then clients := c :: !clients) sw.s_clients;;

List.length !clients;;


List.iter (fun c -> Printf.printf "CLient : %s" (fst c.c_class);
    print_newline () ) !clients;;


(*============================================================= *)

open Modules;;
open Fvwm;;
open Gwml;;

let file = "/etc/X11/fvwm2/system.fvwm2rc";;
let rc = load Fvwm2 file;;
let _ = Fvwm.use Fvwm2 rc;;
let w = !screens.(0)#wob;;

execute_fvwm_module Afterstep "/usr/local/bin/Wharf" w C_NO_CONTEXT [];;

(* With Afterstep, we need to create a AF_UNIX server ! 
NAME in _AS_MODULE_SOCKET
  *)



kill_module (List.hd !fvwm_modules);;







execute_fvwm_module Fvwm2 "FvwmTaskBar" w C_NO_CONTEXT [];;


execute_fvwm_module Fvwm2  "FvwmGoodStuff" w C_NO_CONTEXT [];;

execute_fvwm_module "FvwmWharf" file w C_NO_CONTEXT ;;


config_info;;
