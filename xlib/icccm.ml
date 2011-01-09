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
open Xlib
  
(* peut generer Not_found ou Found_but *)      
let getStringProperty dpy win id = 
  let prop = X.getProperty dpy win false id XA.xa_string 0 0
  in
  if prop.gp_format <> 1 then raise Not_found
  else
  let prop =  X.getProperty dpy win false id XA.xa_string 0
      (prop.gp_left/4+1)
  in
  prop.gp_value


let getWM_NAME dpy win = getStringProperty dpy win XA.xa_wm_name
let getWM_ICON_NAME dpy win = getStringProperty dpy win XA.xa_wm_icon_name
let getWM_CLIENT_MACHINE dpy win = getStringProperty dpy win XA.xa_wm_client_machine


let setStringProperty dpy win id string =
  X.changeProperty dpy win PropModeReplace id XA.xa_string 1 string

let setWM_NAME dpy win wm_name = 
  setStringProperty dpy win XA.xa_wm_name wm_name
let setWM_ICON_NAME dpy win wm_icon_name =
  setStringProperty dpy win XA.xa_wm_icon_name wm_icon_name
let setWM_CLIENT_MACHINE dpy win wm_client_machine =
  setStringProperty dpy win XA.xa_wm_client_machine wm_client_machine


let getStringListProperty dpy win id =
  let buf = getStringProperty dpy win id
  and list = ref []
  and c = ref 0
  in
  for i=0 to (String.length buf)-1 do
    if buf.[i]='\000' then
      begin
        list := (String.sub buf (!c) (i-(!c)))::(!list);
        c := i+1
      end
  done;
  !list

let getWM_CLASS dpy win = getStringListProperty dpy win XA.xa_wm_class
let getWM_COMMAND dpy win = getStringListProperty dpy win XA.xa_wm_command

let setStringListProperty dpy win id list final =
  let len = List.fold_left (fun len s -> String.length s + 1 + len) 0 list in
  let len = if final then len+1 else len in
(*
  let len = ref 0
  in
List.iter (function str -> len := (!len)+1+(String.length str)) list;
  *)  
  let prop = String.create len in
  let pos = List.fold_left (fun pos s ->
        let len = String.length s in
        String.blit s 0 prop pos len;
        prop.[pos+len] <- '\000';
        pos+len+1) 0 list in
  if final then prop.[pos] <- '\000';
(*
  List.iter (function str -> 
        let len = String.length str
        in
        String.blit str 0 prop (!c) len;
        c := (!c) + len +1;
        prop.[(!c)-1] <- '\000'
  ) list;
  *)
  setStringProperty dpy win id prop
;;
let setWM_CLASS dpy win list = 
  setStringListProperty dpy win XA.xa_wm_class list false
let setWM_COMMAND dpy win list =
  setStringListProperty dpy win XA.xa_wm_command list false


type wm_size_hintsMask =
  USPosition
| USSize
| PPosition
| PSize
| PMinSize
| PMaxSize
| PResizeInc
| PAspect
| PBaseSize
| PWinGravity


let getWM_SIZE_HINTS dpy win id =
  let prop = getWholeProperty dpy win id
  in
  if (prop.gp_format<>4) || (prop.gp_type<>XA.xa_wm_size_hints) then
    raise Not_found
  else
  let flag = getCard32 prop.gp_value 0
  in
  
  {
    user_position = (flag land 1)<>0;
    user_size = ( flag land 2)<>0;
    program_position = ( flag land 4)<>0;
    program_size = (flag land 8)<>0;
    min_size =
    if (flag land 16) = 0 then None
    else Some (getInt32 prop.gp_value 20,getCard32 prop.gp_value 24);
    max_size =
    if (flag land 32) = 0 then None
    else Some (getInt32 prop.gp_value 28,getCard32 prop.gp_value 32);
    resize_inc =
    if (flag land 64) = 0 then None
    else Some (getInt32 prop.gp_value 36,getCard32 prop.gp_value 40);
    aspect =
    if (flag land 128) = 0 then None
    else Some (getInt32 prop.gp_value 44,getCard32 prop.gp_value 48,
        getInt32 prop.gp_value 52,getCard32 prop.gp_value 56);
    base_size =
    if (flag land 256) = 0 then None
    else Some (getInt32 prop.gp_value 60,getCard32 prop.gp_value 64);
    win_gravity =
    if (flag land 512) = 0 then None
    else Some (enum_of_int (getInt32 prop.gp_value 68))
  }
;;

let setWM_SIZE_HINTS dpy win id data =
  let hints = newString 18
  and flags = ref 0
  in
  if data.user_position then flags := (!flags) lor 1;
  if data.user_size then flags := (!flags) lor 2;    
  if data.program_position then flags := (!flags) lor 4;    
  if data.program_size then flags := (!flags) lor 8;
  (
    match data.min_size with
      Some (min_width,min_height) ->
        flags := (!flags) lor 16;
        setCard32 hints 20 min_width;
        setCard32 hints 24 min_height
    | _ -> ()
  );    
  (
    match data.max_size with
      Some (max_width,max_height) ->
        flags := (!flags) lor 32;
        setCard32 hints 28 max_width;
        setCard32 hints 32 max_height
    | _ -> ()
  );
  (
    match data.resize_inc with
      Some (width_inc,height_inc) ->
        flags := (!flags) lor 64;
        setCard32 hints 36 width_inc;
        setCard32 hints 40 height_inc
    | _ -> ()
  );
  (
    match data.aspect with
      Some (min_width,min_height,max_width,max_height) ->
        flags := (!flags) lor 128;
        setCard32 hints 44 min_width;
        setCard32 hints 48 min_height;
        setCard32 hints 52 max_width;
        setCard32 hints 56 max_height
    | _ -> ()
  );
  (
    match data.base_size with
      Some (base_width,base_height) ->
        flags := (!flags) lor 256;
        setCard32 hints 60 base_width;
        setCard32 hints 64 base_height
    | _ -> ()
  );
  (
    match data.win_gravity with
      Some win_gravity ->
        flags := (!flags) lor 512;
        setEnum32 hints 68 win_gravity
    | _ -> ()
  );
  setCard32 hints 0 (!flags);
  X.changeProperty dpy win PropModeReplace id XA.xa_wm_size_hints 4 hints
;;

let newWM_SIZE_HINTS () =
  {
    user_position = false;
    user_size = false;
    program_position = false;
    program_size = false;
    min_size = None;
    max_size = None;
    resize_inc = None;
    base_size = None;
    aspect = None;
    win_gravity = None
  }
;;

let setWM_NORMAL_HINTS dpy win h =
  setWM_SIZE_HINTS dpy win XA.xa_wm_normal_hints h
;;


let getWM_HINTS dpy win id =
  let prop = getWholeProperty dpy win id
  in
  if (prop.gp_format<>4) || (prop.gp_type<>XA.xa_wm_hints) then
    raise Not_found
  else
  let flag = getCard32 prop.gp_value 0
  in
  {
    input = (
      if (flag land 1) = 0 then None
      else Some (getEnum32 prop.gp_value 4));
    initial_state = ( 
      if (flag land 2) = 0 then None
      else Some (getEnum32 prop.gp_value 8));
    icon_pixmap = (
      if (flag land 4) = 0 then noWindow
      else getWindow prop.gp_value 12);
    icon_window = (
      if (flag land 8) = 0 then noWindow
      else getEnum32 prop.gp_value 16);
    icon_position = (
      if (flag land 16) = 0 then None
      else Some (getCard32 prop.gp_value 20,getCard32 prop.gp_value 24));
    icon_mask = (
      if (flag land 32) = 0 then noWindow
      else getWindow prop.gp_value 28);
    window_group = (
      if (flag land 64) = 0 then noWindow
      else getWindow prop.gp_value 32);
    urgency_hint = (flag land 256)<>0
  }
;;

let setWM_HINTS dpy win data =
  let hints = newString 9
  and flags = ref 0
  in
  if data.urgency_hint then flags := (!flags) lor 256;
  (
    match data.input with
      Some input ->
        flags := (!flags) lor 1;
        setEnum32 hints 4 input
    | _ -> ()
  );    
  (
    match data.initial_state with
      Some initial_state ->
        flags := (!flags) lor 2;
        setEnum32 hints 8 initial_state
    | _ -> ()
  );
  if data.icon_pixmap <> noWindow then (
      flags := (!flags) lor 4;
      setWindow hints 12 data.icon_pixmap);
  if data.icon_window <> noWindow then (
      flags := (!flags) lor 8;
      setWindow hints 16 data.icon_window);
  (match data.icon_position with None -> () |
      Some (icon_x,icon_y) ->
        flags := (!flags) lor 16;
        setCard32 hints 20 icon_x;
        setCard32 hints 24 icon_y);
  if data.icon_mask <> noWindow then (
      flags := (!flags) lor 32;
      setWindow hints 28 data.icon_mask);
  if data.window_group <> noWindow then (
      flags := (!flags) lor 64;
      setEnum32 hints 32 data.window_group);
  setCard32 hints 0 (!flags);
  X.changeProperty dpy win PropModeReplace XA.xa_wm_hints XA.xa_wm_hints 4 hints
;;

let newWM_HINTS () =
  {
    urgency_hint = false;
    input = None;
    initial_state = None;
    icon_pixmap = noWindow;
    icon_window = noWindow;
    icon_position = None;
    icon_mask = noWindow;
    window_group = noWindow
  }
;;

let getEnum32Property dpy win id t' =
  let prop = getWholeProperty dpy win id
  in
  if (prop.gp_type<>t') || (prop.gp_format<>4) then raise Not_found;
  let list = ref []
  in
  for i=0 to prop.gp_length-1 do
    list := (getEnum32 prop.gp_value (4*i))::(!list)
  done;
  !list
;;

let getWM_TRANSIENT_FOR dpy win =
  match getEnum32Property dpy win XA.xa_wm_transient_for XA.xa_window with
    [w] -> (w : window)
  | _ -> raise Not_found (* should not happen *)
;;
let getWM_PROTOCOLS dpy win =
  getEnum32Property dpy win (X.internAtom dpy "WM_PROTOCOLS" true)
  XA.xa_atom
;;
let getWM_COLORMAP_WINDOWS dpy win =
  getEnum32Property dpy win (X.internAtom dpy "WM_COLORMAP_WINDOWS" true)
  XA.xa_window
;;


let setEnum32Property dpy win id t' list =
  let prop = newString (List.length list)
  and i = ref 0
  in
  List.iter (function v -> 
        setEnum32 prop ((!i)*4) v;
        incr i) list;
  X.changeProperty dpy win PropModeReplace id t' 4 prop
;;

let setWM_TRANSIENT_FOR dpy win (w : window) =
  setEnum32Property dpy win XA.xa_wm_transient_for XA.xa_window [w]
;;
let setWM_PROTOCOLS dpy win list =
  setEnum32Property dpy win (X.internAtom dpy "WM_PROTOCOLS" true)
  XA.xa_atom list
;;
let setWM_COLORMAP_WINDOWS dpy win list =
  setEnum32Property dpy win (X.internAtom dpy "WM_COLORMAP_WINDOWS" true)
  XA.xa_atom list
;;

let getWM_STATE dpy win =
  let wm_state = X.internAtom dpy "WM_STATE" true
  in
  let  prop = 
    getWholeProperty dpy win wm_state      
  in
  if (prop.gp_format <>4) || (prop.gp_type<>wm_state) then
    raise Not_found;
  (((getEnum32 prop.gp_value 0) : wmState),(getWindow prop.gp_value 4 : window))
;;

let setWM_STATE dpy win ((state : wmState),(icon : window)) =
  let prop = newString 2
  and wm_state = X.internAtom dpy "WM_STATE" true
  in
  setEnum32 prop 0 state;
  setWindow prop 4 icon;
  X.changeProperty dpy win PropModeReplace wm_state wm_state 4 prop
;;

let setWM_ICON_SIZE dpy win list =
  let n = List.length list
  and i = ref 0
  in
  let prop = newString (6*n)
  and wm_icon_size = X.internAtom dpy "WM_ICON_SIZE" true
  in
  List.iter (function data ->
        setInt32 prop ((24*(!i))+0) data.min_width;
        setInt32 prop ((24*(!i))+4) data.min_height;
        setInt32 prop ((24*(!i))+8) data.max_width;
        setInt32 prop ((24*(!i))+12) data.max_height;
        setInt32 prop ((24*(!i))+16) data.width_inc;
        setInt32 prop ((24*(!i))+20) data.height_inc;
        incr i) list;
  X.changeProperty dpy win PropModeReplace wm_icon_size 
    wm_icon_size 4 prop
;;

let getWM_ICON_SIZE dpy win =
  let wm_icon_size = X.internAtom dpy "WM_ICON_SIZE" true
  in
  let  prop = getWholeProperty dpy win wm_icon_size      
  in
  if (prop.gp_format <>4) || (prop.gp_type<> wm_icon_size) then
    raise Not_found;
  let list = ref [] 
  in
  for i=0 to prop.gp_length/6 do
    list :=
    {
      min_width = getInt32 prop.gp_value ((24*i)+0);
      min_height = getInt32 prop.gp_value  ((24*i)+4);
      max_width = getInt32 prop.gp_value  ((24*i)+8);
      max_height = getInt32 prop.gp_value  ((24*i)+12);
      width_inc = getInt32 prop.gp_value  ((24*i)+16);
      height_inc = getInt32 prop.gp_value  ((24*i)+20)
    }::(!list)
  done;
  !list

let withdrawWindow dpy win root =
  X.unmapWindow dpy win;
  X.sendEvent dpy root false
    [SubstructureNotifyMask;SubstructureRedirectMask]
    (UnmapNotifyEvent {
      Xunmap.event = root;
      Xunmap.window = win;
      Xunmap.from_configure = false
    })

let iconifyWindow dpy win root =
  let wm_state = X.internAtom dpy "WM_CHANGE_STATE" false
  in
  if wm_state = noAtom then raise Not_found;
  X.sendEvent dpy root false
    [SubstructureRedirectMask;SubstructureNotifyMask]
    ( ClientMessageEvent { 
      Xclient.format = 4;
      Xclient.window =  win;
      Xclient.datatype = wm_state;
      Xclient.data = let str = String.create 20
      in 
      setEnum32 str 0 IconicState;
      str
    })



let safe_getWM_SIZE_HINTS display win =
  try
    getWM_SIZE_HINTS display win XA.xa_wm_normal_hints
  with
    _ ->
      {
        user_position = false;
        user_size = false;
        program_position = false;
        program_size = false;
        min_size = None;
        max_size = None;
        resize_inc = None;
        aspect = None;
        base_size = None;
        win_gravity = None;
      }

let safe_getWM_HINTS dpy win =
  try
    getWM_HINTS dpy win XA.xa_wm_hints
  with
    _ -> newWM_HINTS ()
      
      