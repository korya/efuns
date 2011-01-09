(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xtypes
open Concur

  
(************************************************ DISPLAY *)
let openDisplay = Display.openDisplay
let closeDisplay = Display.closeDisplay

let defaultRoot dpy = 
  dpy.dpy_roots.(dpy.dpy_screen_default).scr_root
let defaultScreen dpy =
  dpy.dpy_roots.(dpy.dpy_screen_default)
let defaultWhite dpy =  
  dpy.dpy_roots.(dpy.dpy_screen_default).scr_white_pixel
let defaultBlack dpy = 
  dpy.dpy_roots.(dpy.dpy_screen_default).scr_black_pixel
let defaultColormap dpy = 
  dpy.dpy_roots.(dpy.dpy_screen_default).scr_default_colormap
let defaultDepth dpy =
  dpy.dpy_roots.(dpy.dpy_screen_default).scr_root_depth
  
  
  (*********************************************** EVENTS *)
(*
  abcde fghij
nextEvent  ----> a
   bcde fghij
  
  abcde fghij
checkEvent ----> f (non-blocking)
  abcde  ghij  
  
  abcde fghij
peekEvent  ----> f
  abcde  ghij  
  
  abcde  ghij
putBackEvent <---- f
  abcdef ghij  
  
  abcde fghij
readEvent  -----> f  
  abcde fghij
  
  
********************************************)  
  
  (*[Xlib.nextEvent] nextEvent dpy :
  - take next event from the queue
  - the event can not be read again
  - the event can not be put back
  - the event might have already been read
  - the call can block until an event is available
  *)
let nextEvent dpy =
  Mutex.lock xlib_mutex;
  while Equeue.empty_take dpy.event_queue do
    Condition.wait xlib_wait xlib_mutex
  done;
  let ev = Equeue.take dpy.event_queue in
  Mutex.unlock xlib_mutex;
  ev

  (*[Xlib.nextEventWait] nextEventWait dpy :
  - check if a call to nextEvent will be blocked
  *)
let nextEventWait dpy =
  Mutex.lock xlib_mutex;
  let r = Equeue.empty_take dpy.event_queue in
  Mutex.unlock xlib_mutex;
  r
    
  (*[X.checkEvent] checkEvent dpy :
  - take next event from the queue
  - the event can be put back in the queue
  - the call can not block
  *)

let checkEvent dpy =
  Mutex.lock xlib_mutex;
  if Equeue.empty_read dpy.event_queue then 
    (Mutex.unlock xlib_mutex; raise Not_found)
  else
  let ev = Equeue.peek dpy.event_queue in
  Mutex.unlock xlib_mutex;
  ev

  (*[Xlib.peekEventWait] peekEventWait dpy :
  - check if a call to peekEvent will be blocked
  - check if a call to readEvent will be blocked
  *)
let peekEventWait dpy =
  Mutex.lock xlib_mutex;
  let r = not (Equeue.empty_read dpy.event_queue) in
  Mutex.unlock xlib_mutex;
  r
  
    (*[X.readEvent] readEvent dpy :
  - take next event from the queue
  - the event is not removed from the queue
  - the event can NOT be put back in the queue
  - the call can block if no event is available
  *)

let readEvent dpy =
  Mutex.lock xlib_mutex;
  while Equeue.empty_read dpy.event_queue do
    Condition.wait xlib_wait xlib_mutex
  done;
  let ev = Equeue.read dpy.event_queue in
  Mutex.unlock xlib_mutex;
  ev

    (*[X.peekEvent] peekEvent dpy :
  - take next event from the queue
  - the event can be put back in the queue
  - the call can block if no event is available
  *)

let peekEvent dpy =
  Mutex.lock xlib_mutex;
  while Equeue.empty_read dpy.event_queue do
    Condition.wait xlib_wait xlib_mutex
  done;
  let ev = Equeue.peek dpy.event_queue in
  Mutex.unlock xlib_mutex;
  ev

let putBackEvent dpy ev =
  Mutex.lock xlib_mutex;
  Equeue.put_back dpy.event_queue ev;
  Mutex.unlock xlib_mutex

let cleanEvents dpy =
  Mutex.lock xlib_mutex;
  Equeue.clean dpy.event_queue;
  Mutex.unlock xlib_mutex

  (*** Il faut clairement changer l'architecture de la librairie de telle
  sorte que:
  
  --> Lecture depuis le serveur X
     --> Queue d'events
     --> Queue de reponses/erreurs
  --> Gestion des reponses/erreurs
  ***)
  
let actions = ref (-1)
let exit = Exit
let checkPredEvent dpy pre clean =
  (try
      for i = 0 to 10 do
        if not (Concur.poll ()) then raise Exit;
      done with _ -> ());
  Mutex.lock xlib_mutex;
  if clean then Equeue.clean dpy.event_queue;
  let rec iter () =
    let ev = Equeue.peek dpy.event_queue in
    if pre ev then
      begin
        Mutex.unlock xlib_mutex;         
        Some ev 
      end
    else
      begin
        Equeue.put_back dpy.event_queue ev;
        iter ()
      end
  in
  try
    iter ()
  with
    e -> 
      Mutex.unlock xlib_mutex; 
      None

let removePredEvent dpy pre =
  (try
      for i = 0 to 10 do
        if not (Concur.poll ()) then raise Exit;
      done with _ -> ());
  Mutex.lock xlib_mutex;
  Equeue.clean dpy.event_queue;
  let rec iter () =
    let ev = Equeue.peek dpy.event_queue in
    if not (pre ev) then
      Equeue.put_back dpy.event_queue ev;
    iter ()
  in
  try
    iter ()
  with
    e -> 
      Mutex.unlock xlib_mutex

let waitPredEvent dpy pre clean =
  Mutex.lock xlib_mutex;
  if clean then Equeue.clean dpy.event_queue;
  let rec iter () =
    while Equeue.empty_read dpy.event_queue do
      Condition.wait xlib_wait xlib_mutex
    done;
    let ev = Equeue.peek dpy.event_queue in
    if pre ev then
      begin
        Mutex.unlock xlib_mutex;         
        ev 
      end
    else
      begin
        Equeue.put_back dpy.event_queue ev;
        iter ()
      end
  in
  try
    let e = iter () in
    e
  with
    e -> (* Should never happen *)
      Mutex.unlock xlib_mutex; 
      raise e

let checkTypedEvent dpy list clean =
  checkPredEvent dpy (fun ev -> List.mem ev.ev_type list) clean

let checkTypedWindowEvent dpy window list clean =
  checkPredEvent dpy (fun ev -> 
      ev.ev_window = window && List.mem ev.ev_type list) clean
  
      (********************************************* WINDOWS *)
  
let createSimpleWindow dpy parent x y width height border_width args =
  let screen = dpy.dpy_roots.(dpy.dpy_screen_default) in
  X.createWindow dpy parent x y width height 
    screen.scr_root_depth InputOutput 
    screen.scr_root_visual_id  border_width args
      
let moveResizeWindow dpy win x y dx dy =
  X.configureWindow dpy win [CWX x; CWY y;CWWidth (max dx 1); CWHeight (max dy 1)]

  let raiseWindow dpy win =
  X.configureWindow dpy win [CWStackMode Above]
    
let lowerWindow dpy win =
  X.configureWindow dpy win [CWStackMode Below]

  
let selectInput dpy win list =
  Xasync.changeWindowAttributes dpy win [CWEventMask list]

  
let moveWindow dpy win x y =
  X.configureWindow dpy win [CWX x; CWY y]

let resizeWindow dpy win dx dy =
  X.configureWindow dpy win [CWWidth (max dx 1); CWHeight (max dy 1)]

let clearWindow dpy win = X.clearArea dpy win 0 0 0 0 false
  (******************************************** DRAWINGS *)
  
let drawSubString dpy win gc x y str pos len =
  let max = String.length str - pos in
  let len = if len > max then max else len in
  X.polyText8 dpy win gc x y [Text8 (0,str,pos,len)]

let drawString dpy win gc x y str =
  drawSubString dpy win gc x y str 0 (String.length str)
  
let imageSubString dpy win gc x y str pos len =
  let max = String.length str - pos in
  let len = if len > max then max else len in
  X.imageSubText8 dpy win gc x y str pos len
  
let imageString dpy win gc x y str =
  imageSubString dpy win gc x y str 0 (String.length str)
  
let drawRectangle dpy win gc x y dx dy =
  X.polyRectangle dpy win gc [x,y,dx,dy]
  
let drawSegment dpy win gc x1 y1 x2 y2 =
  X.polySegment dpy win gc [x1,y1,x2,y2]
let drawLine = drawSegment
let drawArc dpy win gc x y rx ry a1 a2 =
  X.polyArc dpy win gc [x,y,rx,ry,a1,a2]
  
let drawPoint dpy win gc x y =
  X.polyPoint dpy win gc Origin [x,y]

let fillRectangle dpy win gc x y dx dy =
  X.polyFillRectangle dpy win gc [x,y,dx,dy]
  
  (*********************************** ERRORS *)
  
let xerror_to_string = Display.xerror_to_string
let printXError error = Display.print_xerror error

  
(************************************ CURSORS *)
let createFontCursor dpy which =
  let cursorFont = X.openFont dpy "cursor" in
  X.createGlyphCursor dpy cursorFont cursorFont which
    (which+1) 0 0 0 0xFFFF 0xFFFF 0xFFFF
  
    (************************************* PROPERTIES *)
(* get one property *)
let getWholeProperty dpy win id =
  let prop =
    X.getProperty dpy win false id anyPropertyType 0 0
  in
  X.getProperty dpy win false id prop.gp_type 0 (prop.gp_left/4+1)
    
    
  (* ListProperties (atoms and values) *)
let listProperties dpy win =
  let list = X.listProperties dpy win
  in
  List.map (function id ->
    let prop = getWholeProperty dpy win id
    in
    (X.getAtomName dpy id,X.getAtomName dpy prop.gp_type,
     prop.gp_format,prop.gp_length,prop.gp_value)
      ) list
  
let setForeground dpy gc pixel =  X.changeGC dpy gc [GCforeground pixel]
  
  
let set_line_style display gc width style cap join =
  X.changeGC display gc [
    GCline_style style; 
    GCcap_style cap; 
    GCjoin_style join;
    GCline_width width]
