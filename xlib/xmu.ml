(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


open Xtypes
  
exception Window of window
    
  (* XmuClientWindow ... *)
let clientWindow dpy win =
  let wm_state = X.internAtom dpy "WM_STATE" true
  in
  if atom_to_id wm_state=0 then win
  else
    let rec try_win w =
      try
        let p = X.getProperty dpy w false wm_state anyPropertyType 0 0
        in
        raise (Window w)
      with
        Not_found -> 
          let tree = X.queryTree dpy w in
          List.iter try_win tree.qt_subwindows;
          ()
    in
    try
      try_win win;
      win
    with
      Window w -> w
                
      (* SelectWindow (with the mouse) *)
let selectWindow dpy root cursor =
  try
    X.grabPointer dpy root false
      [ButtonPressMask;ButtonReleaseMask] GrabModeSync
      GrabModeAsync root cursor currentTime;
    let buttons = ref 0 in
    try
      while true do
        X.allowEvents dpy SyncPointer currentTime;
        let event = Xlib.nextEvent dpy in
        let win = event.ev_window
        and t = event.ev_type
        in
        (match event.ev_event with
            ButtonPressEvent e -> incr buttons
          | ButtonReleaseEvent e ->
              if (!buttons) > 0 then
                raise (Window e.Xbutton.child)
          | _ -> ())
      done; 
      noWindow
    with
      e -> X.ungrabPointer dpy currentTime; raise e
  with
    Window win ->
      if win == noWindow then root else win
        