(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* GNOME compliance. These functions are taken from Enlightment,
  but with Public Domain license... *)

open Xtypes
open Xbuffer
open Xlib
open Gwml


type hints = {
    skiptask : bool;
    skipfocus : bool;
    skipwinlist : bool;
    focusclick : bool;
    never_use_area : bool;
  }

type hint_state = {
    shaded : bool;
    sticky : bool;
    fixed_pos : bool;
    ignorearrange : bool;
  }
  
let getCurrentArea sw = 0,0
let currentDesk sw = 0
let getAreaSize sw = 1,1
  
let getAreaValues sw =
  let s = sw.w_screen in
  let scr = s.s_scr in
  let clients = Stdconfig.list_clients sw in
  let min_x = ref 0 in
  let min_y = ref 0 in
  let max_x = ref (scr.scr_width - 1) in
  let max_y = ref (scr.scr_height - 1) in
  List.iter (fun (c,w) ->
      let tw = w.w_top in
      let g = tw.w_geometry in
      if c.c_wm_state = NormalState then begin
          min_x := min !min_x g.x;
          min_y := min !min_y g.y;
          max_x := max !max_x g.x;
          max_y := max !max_y g.y;
        end) clients;
  let px = if !min_x < 0 then 
      ((- !min_x) / scr.scr_width) + 1 else 0 in
  let py = if !min_y < 0 then
      ((- !min_y) / scr.scr_height) + 1 else 0 in
  let dx = !max_x  / scr.scr_width + 1 in
  let dy = !max_y  / scr.scr_height + 1 in
  px,py, px+dx, py+dy
  
let deskNames sw = []
let destopsNumber sw = 1
let getWinArea w = 0,0
let getWinDesktop w = 0
(*********************************************************)
(* Properties set on the root window (or desktop window) *)
(*                                                       *)
(* Even though the rest of E is GPL consider this file   *)
(* Public Domain - use it howvere you see fit to make    *)
(* your WM gnome compiant                                *)
(*********************************************************)

(* WIN_WM_NAME STRING - contains a string identifier for the WM's name *)
let xa_WIN_WM_NAME = X.internAtom display "_WIN_WM_NAME" false

(* WIN_WM_NAME VERSION - contains a string identifier for the WM's version *)
let xa_WIN_WM_VERSION = X.internAtom display "_WIN_WM_VERSION" false

(* WIN_AREA CARD32[2] contains the current desktop area X,Y *)
let xa_WIN_AREA = X.internAtom display "_WIN_AREA" false

(* WIN_AREA CARD32[2] contains the current desktop area size WxH *)
let xa_WIN_AREA_COUNT = X.internAtom display "_WIN_AREA_COUNT" false

(* array of atoms - atom being one of the following atoms *)
let xa_WIN_PROTOCOLS = X.internAtom display "_WIN_PROTOCOLS" false

(* array of iocn in various sizes *)
(* Type: array of CARD32 *)
(*       first item is icon count (n) *)
(*       second item is icon record length (in CARD32s) *)
(*       this is followed by (n) icon records as follows *)
(*           pixmap (XID) *)
(*           mask (XID) *)
(*           width (CARD32) *)
(*           height (CARD32) *)
(*           depth (of pixmap, mask is assumed to be of depth 1) (CARD32) *)
(*           drawable (screen root drawable of pixmap) (XID) *)
(*           ... additional fields can be added at the end of this list *)
let xa_WIN_ICONS = X.internAtom display "_WIN_ICONS" false

(* WIN_WORKSPACE CARD32 contains the current desktop number *)
let xa_WIN_WORKSPACE = X.internAtom display "_WIN_WORKSPACE" false

(* WIN_WORKSPACE_COUNT CARD32 contains the number of desktops *)
let xa_WIN_WORKSPACE_COUNT = X.internAtom display "_WIN_WORKSPACE_COUNT" false

(* WIN_WORKSPACE_NAMES StringList (Text Property) of workspace names *)
(* unused by enlightenment *)
let xa_WIN_WORKSPACE_NAMES = X.internAtom display "_WIN_WORKSPACE_NAMES" false

(* ********** Don't use this.. iffy at best. *********** *)
(* The available work area for client windows. The WM can set this and the WM *)
(* and/or clients may change it at any time. If it is changed the WM and/or  *)
(* clients should honor the changes. If this property does not exist a client *)
(* or WM can create it. *)
(*
 * CARD32              min_x;
 * CARD32              min_y;
 * CARD32              max_x;
 * CARD32              max_y;
 *)
let xa_WIN_WORKAREA = X.internAtom display "_WIN_WORKAREA" false
(* array of 4 CARD32's *)

(* This is a list of window id's the WM is currently managing - primarily *)
(* for being able to have external "tasklist" apps *)
let xa_WIN_CLIENT_LIST = X.internAtom display "_WIN_CLIENT_LIST" false
(* array of N XID's *)

(*********************************************************)
(* Properties on client windows                          *)
(*********************************************************)

(* The layer the window exists in *)
(*      0 = Desktop *)
(*      1 = Below *)
(*      2 = Normal (default app layer) *)
(*      4 = OnTop *)
(*      6 = Dock (always on top - for panel) *)
(* The app sets this alone, not the WM. If this property changes the WM *)
(* should comply and change the appearance/behavior of the Client window *)
(* if this hint does nto exist the WM Will create it ont he Client window *)
let win_LAYER_DESKTOP =                0
let win_LAYER_BELOW =                  2
let win_LAYER_NORMAL =                 4
let win_LAYER_ONTOP =                  6
let win_LAYER_DOCK =                   8
let win_LAYER_ABOVE_DOCK =             10
let win_LAYER_MENU =                   12
let xa_WIN_LAYER = X.internAtom display "_WIN_LAYER" false
(* WIN_LAYER = CARD32 *)

(* flags for the window's state. The WM will change these as needed when *)
(* state changes. If the property contains info on client map, E will modify *)
(* the windows state accordingly. if the Hint does not exist the WM will *)
(* create it on the client window. 0 for the bit means off, 1 means on. *)
(* unused (default) values are 0 *)

(* removed Minimized - no explanation of what it really means - ambiguity *)
(* should not be here if not clear *)
let win_STATE_STICKY =          (1 lsl 0)	(* everyone knows sticky *)
let win_STATE_RESERVED_BIT1 =   (1 lsl 1)	(* removed minimize here *)
let win_STATE_MAXIMIZED_VERT =  (1 lsl 2)	(* window in maximized V state *)
let win_STATE_MAXIMIZED_HORIZ = (1 lsl 3)	(* window in maximized H state *)
let win_STATE_HIDDEN =          (1 lsl 4)	(* not on taskbar but window visible *)
let win_STATE_SHADED =          (1 lsl 5)	(* shaded (NeXT style) *)
let win_STATE_HID_WORKSPACE =   (1 lsl 6)	(* not on current desktop *)
let win_STATE_HID_TRANSIENT =   (1 lsl 7)	(* owner of transient is hidden *)
let win_STATE_FIXED_POSITION =  (1 lsl 8)	(* window is fixed in position even *)
let win_STATE_ARRANGE_IGNORE =  (1 lsl 9)	(* ignore for auto arranging *)
					 (* when scrolling about large *)
					 (* virtual desktops ala fvwm *)
let xa_WIN_STATE = X.internAtom display "_WIN_STATE" false
(* WIN_STATE = CARD32 *)

(* Preferences for behavior for app *)
(* ONLY the client sets this *)
let win_HINTS_SKIP_FOCUS =             (1 lsl 0)		(* "alt-tab" skips this win *)
let win_HINTS_SKIP_WINLIST =           (1 lsl 1)		(* not in win list *)
let win_HINTS_SKIP_TASKBAR =           (1 lsl 2)		(* not on taskbar *)
let win_HINTS_GROUP_TRANSIENT =        (1 lsl 3)		(* ??????? *)
let win_HINTS_FOCUS_ON_CLICK =         (1 lsl 4)		(* app only accepts focus when clicked *)
let win_HINTS_DO_NOT_COVER =           (1 lsl 5)		(* attempt to not cover this window *)
let xa_WIN_HINTS = X.internAtom display "_WIN_HINTS" false
(* WIN_HINTS = CARD32 *)

(* Application state - also "color reactiveness" - the app can keep changing *)
(* this property when it changes its state and the WM or monitoring program *)
(* will pick this up and display somehting accordingly. ONLY the client sets *)
(* this. *)
let win_APP_STATE_NONE =                 0
let win_APP_STATE_ACTIVE1 =              1
let win_APP_STATE_ACTIVE2 =              2
let win_APP_STATE_ERROR1 =               3
let win_APP_STATE_ERROR2 =               4
let win_APP_STATE_FATAL_ERROR1 =         5
let win_APP_STATE_FATAL_ERROR2 =         6
let win_APP_STATE_IDLE1 =                7
let win_APP_STATE_IDLE2 =                8
let win_APP_STATE_WAITING1 =             9
let win_APP_STATE_WAITING2 =             10
let win_APP_STATE_WORKING1 =             11
let win_APP_STATE_WORKING2 =             12
let win_APP_STATE_NEED_USER_INPUT1 =     13
let win_APP_STATE_NEED_USER_INPUT2 =     14
let win_APP_STATE_STRUGGLING1 =          15
let win_APP_STATE_STRUGGLING2 =          16
let win_APP_STATE_DISK_TRAFFIC1 =        17
let win_APP_STATE_DISK_TRAFFIC2 =        18
let win_APP_STATE_NETWORK_TRAFFIC1 =     19
let win_APP_STATE_NETWORK_TRAFFIC2 =     20
let win_APP_STATE_OVERLOADED1 =          21
let win_APP_STATE_OVERLOADED2 =          22
let win_APP_STATE_PERCENT000_1 =         23
let win_APP_STATE_PERCENT000_2 =         24
let win_APP_STATE_PERCENT010_1 =         25
let win_APP_STATE_PERCENT010_2 =         26
let win_APP_STATE_PERCENT020_1 =         27
let win_APP_STATE_PERCENT020_2 =         28
let win_APP_STATE_PERCENT030_1 =         29
let win_APP_STATE_PERCENT030_2 =         30
let win_APP_STATE_PERCENT040_1 =         31
let win_APP_STATE_PERCENT040_2 =         32
let win_APP_STATE_PERCENT050_1 =         33
let win_APP_STATE_PERCENT050_2 =         34
let win_APP_STATE_PERCENT060_1 =         35
let win_APP_STATE_PERCENT060_2 =         36
let win_APP_STATE_PERCENT070_1 =         37
let win_APP_STATE_PERCENT070_2 =         38
let win_APP_STATE_PERCENT080_1 =         39
let win_APP_STATE_PERCENT080_2 =         40
let win_APP_STATE_PERCENT090_1 =         41
let win_APP_STATE_PERCENT090_2 =         42
let win_APP_STATE_PERCENT100_1 =         43
let win_APP_STATE_PERCENT100_2 =         44
let xa_WIN_APP_STATE = X.internAtom display "_WIN_APP_STATE" false
(* WIN_APP_STATE = CARD32 *)

(* Expanded space occupied - this is the area on screen the app's window *)
(* will occupy when "expanded" - ie if you have a button on an app that *)
(* "hides" it by reducing its size, this is the geometry of the expanded *)
(* window - so the window manager can allow for this when doign auto *)
(* positioing of client windows assuming the app can at any point use *)
(* this area and thus try and keep it clear. ONLY the client sets this *)
(*
 * CARD32              x;
 * CARD32              y;
 * CARD32              width;
 * CARD32              height;
 *)
let xa_WIN_EXPANDED_SIZE = X.internAtom display "_WIN_EXPANDED_SIZE" false
(* array of 4 CARD32's *)

(* CARD32 that contians the desktop number the application is on If the *)
(* application's state is "sticky" it is irrelevant. Only the WM should *)
(* change this. *)
let xa_WIN_WORKSPACE = X.internAtom display "_WIN_WORKSPACE" false

(* This atom is a 32-bit integer that is either 0 or 1 (currently). *)
(* 0 denotes everything is as per usual but 1 denotes that ALL configure *)
(* requests by the client on the client window with this property are *)
(* not just a simple "moving" of the window, but the result of a user *)
(* moving the window BUT the client handling that interaction by moving *)
(* its own window. The window manager should respond accordingly by assuming *)
(* any configure requests for this window whilst this atom is "active" in *)
(* the "1" state are a client move and should handle flipping desktops if *)
(* the window is being dragged "off screem" or across desktop boundaries *)
(* etc. This atom is ONLY ever set by the client *)
let xa_WIN_CLIENT_MOVING = X.internAtom display "_WIN_CLIENT_MOVING" false
(* WIN_CLIENT_MOVING = CARD32 *)

(* Designed for checking if the WIN_ supporting WM is still there  *)
(* and kicking about - basically check this property - check the window *)
(* ID it points to - then check that window Id has this property too *)
(* if that is the case the WIN_ supporting WM is there and alive and the *)
(* list of WIN_PROTOCOLS is valid *)
let xa_WIN_SUPPORTING_WM_CHECK = X.internAtom display "_WIN_SUPPORTING_WM_CHECK" false
(* CARD32 *)

(*********************************************************)
(* How an app can modify things after mapping            *)
(*********************************************************)
(*
(* For a client to change layer or state it should send a client message *)
(* to the root window as follows: *)
(*
 * Display             *disp;
 * Window               root, client_window;
 * XClientMessageEvent  xev;
 * CARD32                new_layer;
 * 
 *     xev.type = ClientMessage;
 *     xev.window = client_window;
 *     xev.message_type = XInternAtom(disp, xa_WIN_LAYER, False);
 *     xev.format = 32;
 *     xev.data.l[0] = new_layer;
 *     xev.data.l[1] = CurrentTime;
 *     XSendEvent(disp, root, False, SubstructureNotifyMask, (XEvent * ) &xev);
  *)
  
  
  
(*
 * Display             *disp;
 * Window               root, client_window;
 * XClientMessageEvent  xev;
 * CARD32               mask_of_members_to_change, new_members;
 * 
 *     xev.type = ClientMessage;
 *     xev.window = client_window;
 *     xev.message_type = XInternAtom(disp, xa_WIN_STATE, False);
 *     xev.format = 32;
 *     xev.data.l[0] = mask_of_members_to_change;
 *     xev.data.l[1] = new_members;
 *     xev.data.l[2] = CurrentTimep;
 *     XSendEvent(disp, root, False, SubstructureNotifyMask, (XEvent * ) &xev);
 *)
(*
 * Display             *disp;
 * Window               root, client_window;
 * XClientMessageEvent  xev;
 * CARD32               new_desktop_number;
 * 
 *     xev.type = ClientMessage;
 *     xev.window = client_window;
 *     xev.message_type = XInternAtom(disp, xa_WIN_WORKSPACE, False);
 *     xev.format = 32;
 *     xev.data.l[0] = new_desktop_number;
 *     xev.data.l[2] = CurrentTimep;
 *     XSendEvent(disp, root, False, SubstructureNotifyMask, (XEvent * ) &xev);
 *)
*)
  
let gnome_GetHintIcons w = 
  try
    let gp = getWholeProperty display w.w_window xa_WIN_ICONS in
    if gp.gp_type == XA.xa_pixmap then
      (getWindow gp.gp_value 0, getWindow gp.gp_value 4) (* pixmap, mask *)
    else (noPixmap, noPixmap)
  with Not_found -> (noPixmap, noPixmap)
      
let gnome_GetHintLayer w = 
  try
  let gp = getWholeProperty display w.w_window xa_WIN_LAYER in
  if gp.gp_type == XA.xa_cardinal then getWindow gp.gp_value 0 
  else noWindow
  with Not_found -> noWindow
      
let gnome_GetHintState w = 
  let v = try
    let gp = getWholeProperty display w.w_window xa_WIN_STATE in
    if gp.gp_type == XA.xa_cardinal then getCard32 gp.gp_value 0 
      else 0 with Not_found -> 0
  in      
  { shaded = (v land win_STATE_SHADED <> 0);
    sticky = (v land win_STATE_STICKY <> 0);
    fixed_pos = (v land win_STATE_FIXED_POSITION <> 0);
    ignorearrange = (v land win_STATE_ARRANGE_IGNORE <> 0);
  }

let gnome_GetHintAppState w = 
  try
    let gp = getWholeProperty display w.w_window xa_WIN_APP_STATE in
    if gp.gp_type == XA.xa_cardinal then getCard32 gp.gp_value 0 
    else (-1) with Not_found -> (-1)
      
let gnome_GetHintDesktop w = 
  try
  let gp = getWholeProperty display w.w_window xa_WIN_WORKSPACE in
  if gp.gp_type == XA.xa_cardinal then getCard32 gp.gp_value 0 
    else (-1) with Not_found -> (-1)
    
let gnome_GetHint w =
  let v = try
      let gp = getWholeProperty display w.w_window xa_WIN_HINTS in
      if gp.gp_type == XA.xa_cardinal then getCard32 gp.gp_value 0 
      else 0 with Not_found -> 0
  in
  { skiptask = (v land win_HINTS_SKIP_TASKBAR <> 0);
    skipfocus = (v land win_HINTS_SKIP_FOCUS <> 0);
    skipwinlist = (v land win_HINTS_SKIP_WINLIST <> 0);
    focusclick = (v land win_HINTS_FOCUS_ON_CLICK <> 0);
    never_use_area = (v land win_HINTS_DO_NOT_COVER <> 0);
  }

  
let gnome_GetExpandedSize w = 
  try
    let v =     
      let gp = getWholeProperty display w.w_window xa_WIN_EXPANDED_SIZE in
      if gp.gp_type == XA.xa_cardinal then gp.gp_value else raise Not_found
    in  
    Some { x = getCard32 v 0; y = getCard32 v 4; 
      width = getCard32 v 8; height = getCard32 v 12;
      border = 0 }
  with
    Not_found -> None

  
let gnome_SetHint w hs = 
  let v = 0 in
  let v = if hs.sticky then v lor win_STATE_STICKY else v in
  let v = if hs.shaded then v lor win_STATE_SHADED else v in
  let v = if hs.fixed_pos then v lor win_STATE_FIXED_POSITION else v in
  let b = String.create 4 in
  setCard32 b 0 v;
  X.changeProperty display w.w_window PropModeReplace xa_WIN_STATE 
    XA.xa_cardinal 4 b

let gnome_SetEwinArea w = 
  let b = String.create 8 in
  let area_x, area_y = getWinArea w in
  setCard32 b 0 area_x;
  setCard32 b 4 area_y;
  X.changeProperty display w.w_window PropModeReplace xa_WIN_AREA 
    XA.xa_cardinal 4 b

let gnome_SetEwinDesk w = 
  let b = String.create 4 in
  let d = getWinDesktop w in
  setCard32 b 0 d;
  X.changeProperty display w.w_window PropModeReplace xa_WIN_WORKSPACE 
    XA.xa_cardinal 4 b  
  
let gnome_SetUsedHints sw =
  let b = String.create (4*10) in
  Xbuffer.setAtom b 0 xa_WIN_LAYER;
  Xbuffer.setAtom b (4) xa_WIN_STATE;
  Xbuffer.setAtom b (8) xa_WIN_HINTS;
  Xbuffer.setAtom b (12) xa_WIN_APP_STATE;
  Xbuffer.setAtom b (16) xa_WIN_EXPANDED_SIZE;
  Xbuffer.setAtom b (5*4) xa_WIN_ICONS;
  Xbuffer.setAtom b (6*4) xa_WIN_WORKSPACE;
  Xbuffer.setAtom b (7*4) xa_WIN_WORKSPACE_COUNT;
  Xbuffer.setAtom b (8*4) xa_WIN_WORKSPACE_NAMES;
  Xbuffer.setAtom b (9*4) xa_WIN_CLIENT_LIST;
  X.changeProperty display sw.w_window PropModeReplace xa_WIN_PROTOCOLS 
   XA.xa_atom 4 b
    
let gnome_SetAreaCount_aux sw ax ay =
  let b = String.create 8 in
  setCard32 b 0 ax;
  setCard32 b 4 ay;
  X.changeProperty display sw.w_window PropModeReplace 
    xa_WIN_AREA_COUNT XA.xa_cardinal 4 b

let gnome_SetAreaCount sw =
  let ax,ay = getAreaSize sw in
  gnome_SetAreaCount_aux sw ax ay
  
let gnome_SetCurrentArea_aux sw ax ay =
  let b = String.create (2 * 4) in
  setCard32 b 0 ax;
  setCard32 b 4 ay;
  X.changeProperty display sw.w_window PropModeReplace xa_WIN_AREA
    XA.xa_cardinal 4 b

let gnome_SetCurrentArea sw =
  let ax, ay = getCurrentArea sw  in
  gnome_SetCurrentArea_aux sw ax ay

let gnome_updateArea sw =
  let px, py, dx, dy = getAreaValues sw in
  gnome_SetAreaCount_aux sw dx dy;
  gnome_SetCurrentArea_aux sw px py
  
let gnome_SetCurrentDesk sw =
  let d = currentDesk sw in
  let b = String.create 4 in
  setCard32 b 0 d;
  X.changeProperty display sw.w_window PropModeReplace xa_WIN_WORKSPACE
    XA.xa_cardinal 4 b;
  gnome_SetCurrentArea sw

let gnome_SetWMCheck sw =
  let b = String.create 4 in
  let win = X.createWindow display sw.w_window (-200) (-200) 5 5 0
      CopyClassFromParent copyVisualFromParent 8 [CWOverrideRedirect true] in
  setWindow b 0 win;
  X.changeProperty display sw.w_window PropModeReplace 
    xa_WIN_SUPPORTING_WM_CHECK XA.xa_cardinal 4 b;
  X.changeProperty display win PropModeReplace 
    xa_WIN_SUPPORTING_WM_CHECK XA.xa_cardinal 4 b

  
let gnome_SetDeskCount sw =
  let b = String.create 4 in
  let n = destopsNumber sw in
  setCard32 b 0 n;
  X.changeProperty display sw.w_window PropModeReplace 
    xa_WIN_WORKSPACE_COUNT XA.xa_cardinal 4 b
    
let gnome_SetDeskNames sw = 
  let list = deskNames sw in
  Icccm.setStringListProperty display sw.w_window xa_WIN_WORKSPACE_NAMES list
  true

let gnome_SetClientList s =
  let list = ref [] in
  Wintbl.iter (fun win (c,_) ->
      if win = c.c_window then list := win :: !list
  ) s.s_clients;
  let list = !list in
  let len = List.length list in
  let b = String.create (len*4) in
  let _ = List.fold_left (fun pos win -> setWindow b pos win; pos+4) 0 list in
  X.changeProperty display s.s_scr.scr_root PropModeReplace
    xa_WIN_CLIENT_LIST XA.xa_cardinal 4 b

let gnome_SetWMNameVer sw =
  let wm_name = "GwML" in
  let wm_version =  Version.gwml_version in
  X.changeProperty display sw.w_window PropModeReplace xa_WIN_WM_NAME
    XA.xa_string 1 wm_name;
  X.changeProperty display sw.w_window PropModeReplace xa_WIN_WM_VERSION 
  XA.xa_string 1 wm_version

let gnome_DelHints cwin =
  X.deleteProperty display cwin xa_WIN_WORKSPACE;
  X.deleteProperty display cwin xa_WIN_LAYER;
  X.deleteProperty display cwin xa_WIN_STATE;
  X.deleteProperty display cwin xa_WIN_HINTS;
  X.deleteProperty display cwin xa_WIN_APP_STATE;
  X.deleteProperty display cwin xa_WIN_WORKSPACE;
  X.deleteProperty display cwin xa_WIN_AREA

type gnome_hint = {
    g_desktop : int;
    g_icon : pixmap * pixmap;
    g_layer : window;
    g_state : hint_state;
    g_appState : int;
    g_hint : hints;
    g_expanded_size : geometry option;
  }
  
let gnome_GetHints cwin =
  { g_desktop = gnome_GetHintDesktop cwin;
    g_icon = gnome_GetHintIcons cwin;
    g_layer = gnome_GetHintLayer cwin;
    g_state = gnome_GetHintState cwin;
    g_appState = gnome_GetHintAppState cwin;
    g_hint = gnome_GetHint cwin;
    g_expanded_size =   gnome_GetExpandedSize cwin;
  } 

let setHints sw =
  gnome_SetWMNameVer sw;
  gnome_SetUsedHints sw;
  gnome_SetDeskCount sw;
  gnome_SetDeskNames sw;
  gnome_updateArea sw;
  gnome_SetWMCheck sw
