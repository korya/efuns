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

open Concur
open Unix
open Xtypes

let debug = ref false

let mutex = Concur.Mutex.create ()
  
type display = {
    mutable dis_rejected : bool;
    dis_display : Xtypes.display;
    dis_handlers : (xevent -> unit) Wintbl.t;
    dis_defhandler : (xevent -> unit);
    mutable dis_last_event : event;
    mutable dis_last_time : time;
  }

let exit_eloop = ref false
let displays = ref []
  
let after_event_hooks = ref []
let after_events_hooks = ref []
  
let add_after_event_hook f = after_event_hooks := f:: !after_event_hooks
let add_after_events_hook f = after_events_hooks := f:: !after_events_hooks
  
let event_time = ref currentTime
let last_event = ref (KeymapNotifyEvent { Xkeymap.keys = "" })
let button = ref 0
  
let update_event_time ev =
  last_event := ev.ev_event;
  try
    let time = 
      match ev.ev_event with
        KeyPressEvent e ->  e.Xkey.time
      | KeyReleaseEvent e ->  e.Xkey.time
      | ButtonPressEvent e ->  e.Xbutton.time
      | ButtonReleaseEvent e ->  e.Xbutton.time
      | MotionNotifyEvent e ->  e.Xmotion.time
      | EnterNotifyEvent e ->  e.Xcrossing.time
      | LeaveNotifyEvent e ->  e.Xcrossing.time
      | PropertyNotifyEvent e ->  e.Xproperty.time
      | SelectionClearEvent e ->  e.Xselectionclear.time
      | SelectionRequestEvent e ->  e.Xselectionrequest.time
      | SelectionNotifyEvent e ->   e.Xselection.time
      | _ -> raise Not_found
    in
    event_time := time;
  with
    _ -> ()
  
let update_time dis ev =
  update_event_time ev;
  dis.dis_last_event <- ev.ev_event;
  dis.dis_last_time <- !event_time

let handle_event drepr ev =
  let handler = 
    try
      Wintbl.find drepr.dis_handlers ev.ev_window
    with
      Not_found ->
        if !debug then
          Printf.printf "Eloop.default for %d\n" 
            (window_to_id ev.ev_window);
        drepr.dis_defhandler
  in
  update_time drepr ev;
  (try handler ev with _ -> ());
  List.iter (fun f -> try f () with _ -> ()) !after_event_hooks
  
let lst_it = ref 0

let rec one_display no list =
  match list with
    [] -> no
  | drepr :: tail ->
      if drepr.dis_rejected ||
        Xlib.nextEventWait drepr.dis_display then 
        one_display (no+1) tail
      else 
        begin
          (try
              let ev = Xlib.nextEvent drepr.dis_display in
              Mutex.lock mutex;
              handle_event drepr ev;
              Mutex.unlock mutex;
            with
              e -> Mutex.unlock mutex);
          one_display no tail
        end
        
let rec one_loop num =
  let len = List.length !displays in
  let no = one_display 0 !displays in
  if no = len then
    ( Mutex.lock mutex;
      List.iter (fun f -> try f () with _ -> ()) !after_events_hooks;
      Mutex.unlock mutex;
      num = 0)
  else
    one_loop (num+1)
    
let handle_events wait = 
  if wait then Concur.iterator lst_it;
  one_loop 0

let rec event_loop () =
  let _ = handle_events false in
  Concur.iterator lst_it;
  event_loop ()
      
let exit () = exit_eloop := true
  
let add_display dpy def_handler = 
  let rec iter = function 
      [] -> 
        let dis = {
            dis_rejected = false;
            dis_display = dpy;
            dis_handlers = Wintbl.create 117;
            dis_defhandler = def_handler;
            dis_last_time = currentTime;
            dis_last_event = (KeymapNotifyEvent { Xkeymap.keys = "" });
          } in
        displays := dis :: !displays;
        dis
    | dis :: tail ->
        if dis.dis_display == dpy then dis else iter tail
  in
  iter !displays
  
let list_removeq list ele =
  List.fold_left (fun list e ->
                    if e == ele then list
                    else e ::  list) [] list
  
let remove_display dis =
  displays := list_removeq !displays dis
  
let display dis = dis.dis_display
  
let add_window dis win handler =
  if dis.dis_rejected then failwith "Event_loop.add_window: display closed";
  if !debug then Printf.printf "Eloop.add_window %d\n" (window_to_id win);
  try
    let old_handler = Wintbl.find dis.dis_handlers win in
    Wintbl.add dis.dis_handlers win (fun ev ->
        (try old_handler ev with _ -> ());
        (try handler ev with _ -> ())  
    )
  with _ -> 
  Wintbl.add dis.dis_handlers win handler
  
let remove_window dis win = 
  if !debug then Printf.printf "Eloop.remove_window %d\n" (window_to_id win);
  Wintbl.remove dis.dis_handlers win
  
let known_window dis win =
  try
    let _ = Wintbl.find dis.dis_handlers win in
    true
  with
    _ -> false
      
let add_timer dis time f = Concur.Thread.add_timer time f
  
let last_time dis = dis.dis_last_time
let last_event dis = dis.dis_last_event