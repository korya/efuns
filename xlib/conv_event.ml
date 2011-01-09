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
open Xbuffer

let eventNum event = ((getCard8 event 0) land 127)-2;;
let serial event = getCard16 event 2;;
let sent event = ((getCard8 event 0) land 128)<>0;;

(* convertir un coreEvent en un event 

   coreEvent -> (eventType,window,serial,sent,event)

*)

let debug = ref false

let default_win event = 0

let extension_handlers = Array.create 256 default_win
let set_extension_target ev_num f = 
  extension_handlers.(ev_num-2) <- f
  
let convertCore2Event event serial_in =
  let t = eventNum event in
  let win = ref (getEnum32 event 4) in
  let ev_event =
    (*
    if !debug then 
    (Printf.printf "Event %s" events.(t); print_newline ());
  *)
    match t with
      0 -> 
        win := getCard32 event 12;
        KeyPressEvent  {
          Xkey.detail = getCard8 event 1;
          Xkey.time = getTime event  4;
          Xkey.root = getEnum32 event  8;
          Xkey.event = getEnum32 event 12;
          Xkey.child = getEnum32 event 16;
          Xkey.x_root = getInt16 event 20;
          Xkey.y_root = getInt16 event 22;
          Xkey.x_event = getInt16 event 24;
          Xkey.y_event = getInt16 event 26;
          Xkey.state = getCard16 event 28;
          Xkey.same_screen = getEnum8 event 30
        }
    |1 ->
        win := getCard32 event 12;          
        KeyReleaseEvent  {
          Xkey.detail = getCard8 event 1;
          Xkey.time = getTime event 4;
          Xkey.root = getEnum32 event 8;
          Xkey.event = getEnum32 event 12;
          Xkey.child = getEnum32 event 16;
          Xkey.x_root = getInt16 event 20;
          Xkey.y_root = getInt16 event 22;
          Xkey.x_event = getInt16 event 24;
          Xkey.y_event = getInt16 event 26;
          Xkey.state = getCard16 event 28;
          Xkey.same_screen = getEnum8 event 30
        }
    |2 ->
        win := getCard32 event 12;          
        ButtonPressEvent {
          Xbutton.detail = getEnum8 event 1;
          Xbutton.time = getTime event 4;
          Xbutton.root = getEnum32 event 8;
          Xbutton.event = getEnum32 event 12;
          Xbutton.child = getEnum32 event 16;
          Xbutton.x_root = getInt16 event 20;
          Xbutton.y_root = getInt16 event 22;
          Xbutton.x_event = getInt16 event 24;
          Xbutton.y_event = getInt16 event 26;
          Xbutton.state = getCard16 event 28;
          Xbutton.same_screen = getEnum8 event 30
        }
    
    |3 ->
        win := getCard32 event 12;          
        ButtonReleaseEvent {
          Xbutton.detail = getEnum8 event 1;
          Xbutton.time = getTime event 4;
          Xbutton.root = getEnum32 event 8;
          Xbutton.event = getEnum32 event 12;
          Xbutton.child = getEnum32 event 16;
          Xbutton.x_root = getInt16 event 20;
          Xbutton.y_root = getInt16 event 22;
          Xbutton.x_event = getInt16 event 24;
          Xbutton.y_event = getInt16 event 26;
          Xbutton.state = getCard16 event 28;
          Xbutton.same_screen = getEnum8 event 30 
        }
    
    |4 ->
        win := getCard32 event 12;          
        MotionNotifyEvent  {
          Xmotion.detail = getEnum8 event 1;
          Xmotion.time = getTime event 4;
          Xmotion.root = getEnum32 event 8;
          Xmotion.event = getEnum32 event 12;
          Xmotion.child = getEnum32 event 16;
          Xmotion.x_root = getInt16 event 20;
          Xmotion.y_root = getInt16 event 22;
          Xmotion.x_event = getInt16 event 24;
          Xmotion.y_event = getInt16 event 26;
          Xmotion.state = getCard16 event 28;
          Xmotion.same_screen = getEnum8 event 30 
        }
    
    |5 ->
        win := getCard32 event 12;          
        EnterNotifyEvent {
          Xcrossing.detail = getEnum8 event 1;
          Xcrossing.time = getTime event 4;
          Xcrossing.root = getEnum32 event 8;
          Xcrossing.event = getEnum32 event 12;
          Xcrossing.child = getEnum32 event 16;
          Xcrossing.x_root = getInt16 event 20;
          Xcrossing.y_root = getInt16 event 22;
          Xcrossing.x_event = getInt16 event 24;
          Xcrossing.y_event = getInt16 event 26;
          Xcrossing.state  =  getCard16 event 28;
          Xcrossing.mode = getEnum8 event 30;
          Xcrossing.same_screen =
          ((getCard8 event 31)land 1)<>0;
          Xcrossing.focus=
          ((getCard8 event 31) land 2)<>0
        }
    
    |6 ->
        win := getCard32 event 12;          
        LeaveNotifyEvent {
          Xcrossing.detail = getEnum8 event 1;
          Xcrossing.time = getTime event 4;
          Xcrossing.root = getEnum32 event 8;
          Xcrossing.event = getEnum32 event 12;
          Xcrossing.child = getEnum32 event 16;
          Xcrossing.x_root = getInt16 event 20;
          Xcrossing.y_root = getInt16 event 22;
          Xcrossing.x_event = getInt16 event 24;
          Xcrossing.y_event = getInt16 event 26;
          Xcrossing.state = getCard16 event 28;
          Xcrossing.mode = getEnum8 event 30;
          
          Xcrossing.same_screen = 
          ((getCard8 event 31) land 1)<>0;
          Xcrossing.focus =
          ((getCard8 event 31) land 2)<>0
        }
    
    |7 ->
        FocusInEvent {
          Xfocus.event = getEnum32 event 4;
          Xfocus.detail = getEnum8 event 1;
          Xfocus.mode = getEnum8 event 8 
        }
    
    |8 ->
        FocusOutEvent {
          Xfocus.event = getEnum32 event 4;
          Xfocus.detail = getEnum8 event 1;
          Xfocus.mode = getEnum8 event 8 
        }
    
    |9 ->
        win := 0;          
        KeymapNotifyEvent {
          Xkeymap.keys = getString event 1 31
        }
    
    |10 ->
        ExposeEvent {
          Xexpose.window = getEnum32 event 4;
          Xexpose.x  = getCard16 event 8;
          Xexpose.y  = getCard16 event 10;
          Xexpose.width  = getCard16 event 12;
          Xexpose.height  = getCard16 event 14;
          Xexpose.count  = getCard16 event 16 
        }
    
    |11 ->
        GraphicsExposeEvent {
          Xgraphicsexpose.drawable = getEnum32 event 4;
          Xgraphicsexpose.x = getCard16 event 8;
          Xgraphicsexpose.y = getCard16 event 10;
          Xgraphicsexpose.width = getCard16 event 12;
          Xgraphicsexpose.height = getCard16 event 14;
          Xgraphicsexpose.minor_opcode = 
          getCard16 event 16;
          Xgraphicsexpose.count = getCard16 event 18;
          Xgraphicsexpose.major_opcode =
          getCard16 event 20 
        }
    
    |12 ->
        NoExposeEvent  {
          Xnoexpose.drawable = getEnum32 event 4;
          Xnoexpose.minor_opcode = getCard16 event 8;
          Xnoexpose.major_opcode = getCard8 event 10 
        }
    
    |13 ->
        VisibilityNotifyEvent {
          Xvisibility.window = getEnum32 event 4;
          Xvisibility.state = getEnum8 event 8 
        }
    
    |14 ->
        CreateNotifyEvent {
          Xcreatewindow.parent = getEnum32 event 4;
          Xcreatewindow.window = getEnum32 event 8;
          Xcreatewindow.x = getInt16 event 12;
          Xcreatewindow.y = getInt16 event 14 ;
          Xcreatewindow.width = getCard16 event 16;
          Xcreatewindow.height = getCard16 event 18;
          Xcreatewindow.border_width = getCard16 event 20;
          Xcreatewindow.override_redirect = getEnum8 event 22 
        }
    
    |15 ->
        DestroyNotifyEvent {
          Xdestroywindow.event = getEnum32 event 4;
          Xdestroywindow.window = getEnum32 event 8 
        }
    
    |16 ->
        UnmapNotifyEvent {
          Xunmap.event = getEnum32 event 4;
          Xunmap.window = getEnum32 event 8;
          Xunmap.from_configure = getEnum8 event 12 
        }
    
    |17 ->
        MapNotifyEvent {
          Xmap.event = getEnum32 event 4;
          Xmap.window = getEnum32 event 8;
          Xmap.override_redirect = getEnum8 event 12 
        }
    
    |18 ->
        MapRequestEvent {
          Xmaprequest.parent = getEnum32 event 4;
          Xmaprequest.window = getEnum32 event 8 
        }
    
    |19 ->
        ReparentNotifyEvent {
          Xreparent.event = getEnum32 event 4;
          Xreparent.window = getEnum32 event 8;
          Xreparent.parent = getEnum32 event 12;
          Xreparent.x = getInt16 event 16;
          Xreparent.y = getInt16 event 18;
          Xreparent.override_redirect = getEnum8 event 20 
        }
    
    |20 ->
        ConfigureNotifyEvent {
          Xconfigure.event = getEnum32 event 4;
          Xconfigure.window = getEnum32 event 8;
          Xconfigure.above_sibling = getEnum32 event 12;
          Xconfigure.x = getInt16 event 16;
          Xconfigure.y = getInt16 event 18;
          Xconfigure.width = getCard16 event 20;
          Xconfigure.height = getCard16 event 22;
          Xconfigure.border_width = getCard16 event 24;
          Xconfigure.override_redirect = getEnum8 event 26
        }
    
    |21 ->
        ConfigureRequestEvent {
          Xconfigurerequest.parent = getEnum32 event 4;
          Xconfigurerequest.window = getEnum32 event 8;
          Xconfigurerequest.x =
          if ((getCard16 event 26) land 1) = 0
          then None
          else Some (getInt16 event 16);
          Xconfigurerequest.y = 
          if ((getCard16 event 26) land 2) = 0
          then  None
          else Some (getInt16 event 18);
          Xconfigurerequest.width = 
          if ((getCard16 event 26) land 4) = 0
          then   None
          else Some (getCard16 event 20);
          Xconfigurerequest.height = 
          if ((getCard16 event 26) land 8) = 0
          then   None
          else Some (getCard16 event 22);
          Xconfigurerequest.border_width = 
          if ((getCard16 event 26) land 16) = 0
          then   None
          else Some (getCard16 event 24);
          Xconfigurerequest.sibling =
          if ((getCard16 event 26) land 32) = 0
          then  None
          else Some (getEnum32 event 12);
          Xconfigurerequest.stack_mode = 
          if ((getCard16 event 26) land 64) = 0
          then  None
          else Some (getEnum8 event 1)
        }
    
    |22 ->
        GravityNotifyEvent {
          Xgravity.event = getEnum32 event 4;
          Xgravity.window = getEnum32 event 8;
          Xgravity.x = getInt16 event 12;
          Xgravity.y = getInt16 event 14 
        }
    
    |23 ->
        ResizeRequestEvent {
          Xresizerequest.window = getEnum32 event 4;
          Xresizerequest.width = getCard16 event 8;
          Xresizerequest.height = getCard16 event 10 
        }
    
    |24 ->
        CirculateNotifyEvent {
          Xcirculate.event = getEnum32 event 4;
          Xcirculate.window = getEnum32 event 8;
          Xcirculate.place = getEnum8 event 16 
        }
    
    |25 ->
        CirculateRequestEvent {
          Xcirculaterequest.parent = getEnum32 event 4;
          Xcirculaterequest.window = getEnum32 event 8;
          Xcirculaterequest.place = getEnum8 event 16  
        }
    
    |26 ->
        PropertyNotifyEvent {
          Xproperty.window = getEnum32 event 4;
          Xproperty.atom = getEnum32 event 8;
          Xproperty.time = getTime event 12;
          Xproperty.state = getEnum8 event 16 
        }
    
    |27 ->
        win := 0;                    
        SelectionClearEvent {
          Xselectionclear.time = getTime event 4;
          Xselectionclear.owner = getEnum32 event 8;
          Xselectionclear.selection = getEnum32 event 12 
        }
    
    |28 ->
        win := 0;                    
        SelectionRequestEvent {
          Xselectionrequest.time = getTime event 4;
          Xselectionrequest.owner = getEnum32 event 8;
          Xselectionrequest.requestor= getEnum32 event 12;
          Xselectionrequest.selection = getEnum32 event 16;
          Xselectionrequest.target = getEnum32 event 20;
          Xselectionrequest.property = getEnum32 event 24 
        }
    
    |29 ->
        win := 0;                    
        SelectionNotifyEvent {
          Xselection.time = getTime event 4;
          Xselection.requestor = getEnum32 event 8;
          Xselection.selection = getEnum32 event 12;
          Xselection.target = getEnum32 event 16;
          Xselection.property = getEnum32 event 20 
        }
    
    |30 ->
        ColormapNotifyEvent {
          Xcolormap.window = getEnum32 event 4;
          Xcolormap.colormap = getEnum32 event 8;
          Xcolormap.newp = getEnum8 event 12;
          Xcolormap.state = getEnum8 event 13 
        }
    
    |31 ->
        ClientMessageEvent {
          Xclient.format =  (getCard8 event 1)/8;
          Xclient.window = getEnum32 event 4;
          Xclient.datatype = getEnum32 event 8;
          Xclient.data = getString event 12 20
        }
    
    |32 ->
        win := 0;                    
        MappingNotifyEvent {
          Xmapping.request = getEnum8 event 4;
          Xmapping.first_keycode = getCard8 event 5;
          Xmapping.count = getCard8 event 6 
        }
    
    | _ -> 
        win:= extension_handlers.(t) event; CoreEvent (String.copy event)
  in
  {
    ev_num = t;
    ev_type = enum_of_int t;
    ev_window = id_to_window !win;
    ev_serial_out = serial event;
    ev_serial_in = serial_in ;
    ev_sent = sent event;
    ev_event = ev_event;
  }

let convertEvent2Core disp =
  let b = newString 8
  in
  (
    match disp with
      KeyPressEvent event -> 
        setCard8 b 0 2 ;
        setCard8 b 1 event.Xkey.detail;
        setTime b 4 event.Xkey.time;
        setEnum32 b 8 event.Xkey.root;
        setEnum32 b 12 event.Xkey.event;
        setEnum32 b 16 event.Xkey.child;
        setCard16 b 20 event.Xkey.x_root  ;
        setCard16 b 22 event.Xkey.y_root;
        setCard16 b 24 event.Xkey.x_event;
        setCard16 b 26 event.Xkey.y_event;
        setCard16 b 28 event.Xkey.state;
        setEnum8 b 30 event.Xkey.same_screen   
    
    | KeyReleaseEvent event -> 
        setCard8 b 0 3;
        setCard8 b 1 event.Xkey.detail;
        setTime b 4 event.Xkey.time ;
        setEnum32 b 8 event.Xkey.root;
        setEnum32 b 12 event.Xkey.event;
        setEnum32 b 16 event.Xkey.child;
        setCard16 b 20 event.Xkey.x_root;
        setCard16 b 22 event.Xkey.y_root;
        setCard16 b 24 event.Xkey.x_event ;
        setCard16 b 26 event.Xkey.y_event  ;
        setCard16 b 28 event.Xkey.state;
        setEnum8 b 30 event.Xkey.same_screen
    
    | ButtonPressEvent event -> 
        setCard8 b 0 4;
        setEnum8 b 1 event.Xbutton.detail;
        setTime b 4 event.Xbutton.time;
        setEnum32 b 8 event.Xbutton.root;
        setEnum32 b 12 event.Xbutton.event;
        setEnum32 b 16 event.Xbutton.child;
        setCard16 b 20 event.Xbutton.x_root;
        setCard16 b 22 event.Xbutton.y_root;
        setCard16 b 24 event.Xbutton.x_event;
        setCard16 b 26 event.Xbutton.y_event;
        setCard16 b 28 event.Xbutton.state;
        setEnum8 b 30 event.Xbutton.same_screen
    
    | ButtonReleaseEvent event -> 
        setCard8 b 0 5;
        setEnum8 b 1 event.Xbutton.detail;
        setTime b 4 event.Xbutton.time;
        setEnum32 b 8 event.Xbutton.root;
        setEnum32 b 12 event.Xbutton.event;
        setEnum32 b 16 event.Xbutton.child;
        setCard16 b 20 event.Xbutton.x_root;
        setCard16 b 22 event.Xbutton.y_root;
        setCard16 b 24 event.Xbutton.x_event;
        setCard16 b 26 event.Xbutton.y_event;
        setCard16  b 28 event.Xbutton.state;
        setEnum8 b 30 event.Xbutton.same_screen
    
    | MotionNotifyEvent event -> 
        setCard8 b 0 6;
        setEnum8 b 1 event.Xmotion.detail;
        setTime b 4 event.Xmotion.time;
        setEnum32 b 8 event.Xmotion.root;
        setEnum32 b 12 event.Xmotion.event;
        setEnum32 b 16 event.Xmotion.child;
        setCard16 b 20 event.Xmotion.x_root;
        setCard16 b 22 event.Xmotion.y_root;
        setCard16 b 24 event.Xmotion.x_event ;
        setCard16 b 26 event.Xmotion.y_event ;
        setEnum16 b 28 event.Xmotion.state;
        setEnum8 b 30 event.Xmotion.same_screen
    
    | EnterNotifyEvent event -> 
        setCard8 b 0 7;
        setEnum8 b 1 event.Xcrossing.detail;
        setTime b 4 event.Xcrossing.time;
        setEnum32 b 8 event.Xcrossing.root;
        setEnum32 b 12 event.Xcrossing.event;
        setEnum32 b 16 event.Xcrossing.child;
        setCard16 b 20 event.Xcrossing.x_root;
        setCard16 b 22 event.Xcrossing.y_root;
        setCard16 b 24 event.Xcrossing.x_event;
        setCard16 b 26 event.Xcrossing.y_event;
        setEnum16 b 28 event.Xcrossing.state;
        
        setCard8 b 31
          (
          (if event.Xcrossing.same_screen then 1 else 0)
          lor
            (if event.Xcrossing.same_screen then 2 else 0)
        )
    
    | LeaveNotifyEvent event -> 
        setCard8 b 0 8;
        setEnum8 b 1 event.Xcrossing.detail;
        setTime b 4 event.Xcrossing.time;
        setEnum32 b 8 event.Xcrossing.root;
        setEnum32 b 12 event.Xcrossing.event;
        setEnum32 b 16 event.Xcrossing.child;
        setCard16 b 20 event.Xcrossing.x_root;
        setCard16 b 22 event.Xcrossing.y_root;
        setCard16 b 24 event.Xcrossing.x_event ;
        setCard16 b 26 event.Xcrossing.y_event;
        setEnum16 b 28 event.Xcrossing.state;
        setEnum8 b 30 event.Xcrossing.mode;        
        setCard8 b 31
          (
          (if event.Xcrossing.same_screen then 1 else 0)
          lor
            (if event.Xcrossing.same_screen then 2 else 0)
        )
    
    | FocusInEvent event -> 
        setCard8 b 0 9;
        setEnum32 b 4 event.Xfocus.event;
        setEnum8 b 1 event.Xfocus.detail;
        setEnum8 b 8 event.Xfocus.mode
    
    | FocusOutEvent event -> 
        setCard8 b 0 10;
        setEnum32 b 4 event.Xfocus.event;
        setEnum8 b 1 event.Xfocus.detail;
        setEnum8 b 8 event.Xfocus.mode
    
    | KeymapNotifyEvent event -> 
        setCard8 b 0 11;
        setString b 1 event.Xkeymap.keys 
    
    | ExposeEvent event -> 
        setCard8 b 0 12;
        setEnum32 b 4 event.Xexpose.window;
        setCard16 b 8 event.Xexpose.x;
        setCard16 b 10 event.Xexpose.y;
        setCard16 b 12 event.Xexpose.width;
        setCard16 b 14 event.Xexpose.height;
        setCard16 b 16 event.Xexpose.count
    
    | GraphicsExposeEvent event -> 
        setCard8 b 0 13;
        setEnum32 b 4 event.Xgraphicsexpose.drawable;
        setCard16 b 8 event.Xgraphicsexpose.x;
        setCard16 b 10 event.Xgraphicsexpose.y;
        setCard16 b 12 event.Xgraphicsexpose.width;
        setCard16 b 14 event.Xgraphicsexpose.height;
        setCard16 b 16 event.Xgraphicsexpose.minor_opcode;
        setCard16 b 18 event.Xgraphicsexpose.count;
        setCard16 b 20 event.Xgraphicsexpose.major_opcode
    
    | NoExposeEvent event -> 
        setCard8 b 0 14;
        setEnum32 b 4 event.Xnoexpose.drawable;
        setCard16 b 8 event.Xnoexpose.minor_opcode;
        setCard8 b 10 event.Xnoexpose.major_opcode
    
    | VisibilityNotifyEvent event -> 
        setCard8 b 0 15;
        setEnum32 b 4 event.Xvisibility.window;
        setEnum8 b 8 event.Xvisibility.state
    
    | CreateNotifyEvent event -> 
        setCard8 b 0 16;
        setEnum32 b 4 event.Xcreatewindow.parent;
        setEnum32 b 8 event.Xcreatewindow.window;
        setCard16 b 12 event.Xcreatewindow.x;
        setCard16 b 14  event.Xcreatewindow.y;
        setCard16 b 16 event.Xcreatewindow.width;
        setCard16 b 18 event.Xcreatewindow.height;
        setCard16 b 20 event.Xcreatewindow.border_width;
        setEnum8 b 22 event.Xcreatewindow.override_redirect
    
    | DestroyNotifyEvent event -> 
        setCard8 b 0 17;
        setEnum32 b 4 event.Xdestroywindow.event;
        setEnum32 b 8 event.Xdestroywindow.window
    
    | UnmapNotifyEvent event -> 
        setCard8 b 0 18;
        setEnum32 b 4 event.Xunmap.event;
        setEnum32 b 8 event.Xunmap.window;
        setEnum8 b 12 event.Xunmap.from_configure
    
    | MapNotifyEvent event -> 
        setCard8 b 0 19;
        setEnum32 b 4 event.Xmap.event;
        setEnum32 b 8 event.Xmap.window;
        setEnum8 b 12 event.Xmap.override_redirect
    
    | MapRequestEvent event -> 
        setCard8 b 0 20;
        setEnum32 b 4 event.Xmaprequest.parent;
        setEnum32 b 8 event.Xmaprequest.window
    
    | ReparentNotifyEvent event -> 
        setCard8 b 0 21;
        setEnum32 b 4 event.Xreparent.event;
        setEnum32 b 8 event.Xreparent.window;
        setEnum32 b 12 event.Xreparent.parent;
        setCard16 b 16 event.Xreparent.x;
        setCard16 b 18 event.Xreparent.y;
        setEnum8 b 20 event.Xreparent.override_redirect
    
    | ConfigureNotifyEvent event -> 
        setCard8 b 0 22;
        setEnum32 b 4 event.Xconfigure.event;
        setEnum32 b 8 event.Xconfigure.window;
        setEnum32 b 12 event.Xconfigure.above_sibling;
        setCard16 b 16 event.Xconfigure.x;
        setCard16 b 18 event.Xconfigure.y;
        setCard16 b 20 event.Xconfigure.width;
        setCard16 b 22 event.Xconfigure.height;
        setCard16 b 24 event.Xconfigure.border_width;
        setEnum8 b 26 event.Xconfigure.override_redirect
    
    | ConfigureRequestEvent event -> 
        setCard8 b 0 23;
        setEnum32 b 4 event.Xconfigurerequest.parent;
        setEnum32 b 8 event.Xconfigurerequest.window;
        let mask = ref 0 in
        (
          match event.Xconfigurerequest.x with
            None -> ()
          | Some x -> setCard16 b 16 x;
              mask := (!mask) lor 1
        );
        (
          match event.Xconfigurerequest.y with
            None -> ()
          | Some y  -> setCard16 b 18 y;
              mask := (!mask) lor 2
        );
        (
          match event.Xconfigurerequest.width with
            None -> ()
          | Some width -> setCard16 b 20 width ;
              mask := (!mask) lor 4
        );
        (
          match event.Xconfigurerequest.height with
            None -> ()
          | Some height  -> setCard16 b 22 height ;
              mask := (!mask) lor 8
        );
        (
          match event.Xconfigurerequest.border_width with
            None -> ()
          | Some border_width -> setCard16 b 24 border_width ;
              mask := (!mask) lor 16
        );
        (
          match event.Xconfigurerequest.sibling with
            None -> ()
          | Some sibling  -> setEnum32 b 12 sibling ;
              mask := (!mask) lor 32
        );
        (
          match event.Xconfigurerequest.stack_mode with
            None -> ()
          | Some stack_mode  -> setEnum8 b 1 stack_mode ;
              mask := (!mask) lor 64
        );
        setCard16 b 26 (!mask)
    
    | GravityNotifyEvent event -> 
        setCard8 b 0 24;
        setEnum32 b 4 event.Xgravity.event;
        setEnum32 b 8 event.Xgravity.window;
        setCard16 b 12 event.Xgravity.x;
        setCard16 b 14 event.Xgravity.y
    
    | ResizeRequestEvent event -> 
        setCard8 b 0 25;
        setEnum32 b 4 event.Xresizerequest.window;
        setCard16 b 8 event.Xresizerequest.width;
        setCard16 b 10 event.Xresizerequest.height
    
    | CirculateNotifyEvent event -> 
        setCard8 b 0 26;
        setEnum32 b 4 event.Xcirculate.event;
        setEnum32 b 8 event.Xcirculate.window;
        setEnum8 b 16 event.Xcirculate.place
    
    | CirculateRequestEvent event -> 
        setCard8 b 0 27;
        setEnum32 b 4 event.Xcirculaterequest.parent;
        setEnum32 b 8 event.Xcirculaterequest.window;
        setEnum8 b 16 event.Xcirculaterequest.place
    
    | PropertyNotifyEvent event -> 
        setCard8 b 0 28;
        setEnum32 b 4 event.Xproperty.window;
        setEnum32 b 8 event.Xproperty.atom;
        setTime b 12 event.Xproperty.time;
        setEnum8 b 16 event.Xproperty.state
    
    | SelectionClearEvent event -> 
        setCard8 b 0 29;
        setTime b 4 event.Xselectionclear.time;
        setEnum32 b 8 event.Xselectionclear.owner;
        setEnum32 b 12 event.Xselectionclear.selection
    
    | SelectionRequestEvent event -> 
        setCard8 b 0 30;
        setTime b 4 event.Xselectionrequest.time;
        setEnum32 b 8 event.Xselectionrequest.owner;
        setEnum32 b 12 event.Xselectionrequest.requestor;
        setEnum32 b 16 event.Xselectionrequest.selection;
        setEnum32 b 20 event.Xselectionrequest.target;
        setEnum32 b 24 event.Xselectionrequest.property

    | SelectionNotifyEvent event -> 
        setCard8 b 0 31;
        setTime b 4 event.Xselection.time;
        setEnum32 b 8 event.Xselection.requestor;
        setEnum32 b 12 event.Xselection.selection;
        setEnum32 b 16 event.Xselection.target;
        setEnum32 b 20 event.Xselection.property
    
    | ColormapNotifyEvent event ->
        setCard8 b 0 32;
        setEnum32 b 4 event.Xcolormap.window;
        setEnum32 b 8 event.Xcolormap.colormap;
        setEnum8 b 12 event.Xcolormap.newp;
        setEnum8 b 13 event.Xcolormap.state
    
    | ClientMessageEvent event -> 
        setCard8 b 0 33;
        setCard8 b 1 (event.Xclient.format*8);
        setEnum32 b 4 event.Xclient.window;
        setEnum32 b 8 event.Xclient.datatype;
        setString b 12 event.Xclient.data
    
    | MappingNotifyEvent event -> 
        setCard8 b 0 34;
        setEnum8 b 4 event.Xmapping.request;
        setCard8 b 5 event.Xmapping.first_keycode;
        setCard8 b 6 event.Xmapping.count
    | CoreEvent buffer ->
        String.blit buffer 32 b 0 32
  );
  b
  
  