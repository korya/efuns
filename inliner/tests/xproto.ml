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
open Display

let (simpleWindowRequest: requestOpcode -> window -> string) = simpleRequest
let (simpleFontRequest: requestOpcode -> font -> string) = simpleRequest
let (simpleColormapRequest: requestOpcode -> colormap -> string) = 
  simpleRequest

  
let rec insert tag v res =
  match res with
    [] -> [v]
  | head :: tail ->
      if tag > (Obj.tag (Obj.repr head)) then 
        head :: (insert tag v tail)
      else
        v :: res
  
let rec iter1 list mask res count =
  match list with
    [] -> 
      (*      Printf.printf "count_args: %d" mask; print_newline (); *)
      mask, count, res
  | v :: tail ->
      let tag = Obj.tag (Obj.magic v) in
      let bit = 1 lsl tag in
      if bit land mask <> 0 then
        iter1 tail mask res count
      else
        iter1 tail (mask lor bit) (insert tag v res) (count+1)
        
let count_args list = iter1 list 0 [] 0

let rec iter2 b pos list =
  match list with
    [] -> ()
  | head :: tail -> 
      (match head with
        | CWBackPixmap pixmap -> 
            setEnum32 b pos pixmap; 
        | CWBackPixel pixel ->
            setEnum32 b pos pixel; 
        | CWBorderPixmap pixmap ->
            setEnum32 b pos pixmap; 
        | CWBorderPixel pixel ->
            setEnum32 b pos pixel; 
        | CWBitGravity bitGravity ->
            setEnum32 b pos bitGravity; 
        | CWWinGravity bitGravity ->
            setEnum32 b pos bitGravity; 
        | CWBackingStore backingStore ->
            setEnum32 b pos backingStore; 
        | CWBackingPlanes planes ->
            setEnum32 b pos planes; 
        | CWBackingPixel pixel ->
            setEnum32 b pos pixel; 
        | CWOverrideRedirect bool ->
            setEnum32 b pos bool; 
        | CWSaveUnder bool ->
            setEnum32 b pos bool; 
        | CWEventMask eventMask ->
            setEnum32 b pos (mask_of_list eventMask); 
        | CWDontPropagate eventMask ->
            setEnum32 b pos (mask_of_list eventMask); 
        | CWColormap colormap ->
            setEnum32 b pos colormap; 
        | CWCursor cursor ->
            setEnum32 b pos cursor);
      iter2 b (pos+4) tail
          
let setWindowAttributes b pos list = iter2 b (pos+4) list

let rec iter3 b pos list =
  match list with
    [] -> ()
  | head :: tail -> 
      (match head with
          KBKeyClickPercent perc ->
            setCard32 b pos perc; 
        | KBBellPercent perc ->
            setCard32 b pos perc; 
        | KBBellPitch pitch ->
            setCard32 b pos pitch; 
        | KBBellDuration dur ->
            setCard32 b pos dur; 
        | KBLed led ->
            setCard32 b pos led; 
        | KBLedMode led ->
            setEnum32 b pos led; 
        | KBKey key ->
            setCard32 b pos key; 
        | KBAutoRepeatMode mode ->
            setEnum32 b pos mode; 
      );
      iter3 b (pos+4) tail
      
let changeKeyboardControl b pos list =
  iter3 b (pos+4) list

let rec iter4 b pos list =
  match list with
    [] -> ()
  | head :: tail -> 
      (match head with
          CWX int ->
            setEnum32 b pos int;
        | CWY int ->
            setEnum32 b pos int;
        | CWWidth int ->
            setEnum32 b pos int;
        | CWHeight int ->
            setEnum32 b pos int;
        | CWBorderWidth int ->
            setEnum32 b pos int;
        | CWSibling window ->
            setEnum32 b pos window;
        | CWStackMode stackMode ->
            setEnum32 b pos stackMode);
      iter4 b (pos+4) tail
      
let setConfigureWindow b pos list =
  iter4 b (pos+4) list  (* (Sort.list (<) list)) *)

type t5 = Always of Obj.t
  
let rec iter5 b pos list =
  match (Obj.magic list) with
    [] -> ()
  | (Always field0) :: tail -> 
      setEnum32 b pos field0;
      (*
      (match head with
        | GCfonction gxFunction ->
            setEnum32 b pos gxFunction;
        | GCplane_mask int  ->
            setEnum32 b pos int;
        | GCforeground pixel   ->
            setEnum32 b pos pixel;
        | GCbackground pixel   ->
            setEnum32 b pos pixel;
        | GCline_width int   ->
            setEnum32 b pos int;
        | GCline_style lineStyle   ->
            setEnum32 b pos lineStyle;
        | GCcap_style capStyle   ->
            setEnum32 b pos capStyle;
        | GCjoin_style joinStyle   ->
            setEnum32 b pos joinStyle;
        | GCfill_style fillStyle   ->
            setEnum32 b pos fillStyle;
        | GCfill_rule fillRule   ->
            setEnum32 b pos fillRule;
        | GCtile pixmap   ->
            setEnum32 b pos pixmap;
        | GCstipple pixmap   ->
            setEnum32 b pos pixmap;
        | GCts_x_origin coord   ->
            setEnum32 b pos coord;
        | GCts_y_origin coord   ->
            setEnum32 b pos coord;
        | GCfont font   ->
            setEnum32 b pos font;
        | GCsubwindow_mode subwindowMode   ->
            setEnum32 b pos subwindowMode;
        | GCgraphics_exposures bool  ->
            setEnum32 b pos bool;
        | GCclip_x_origin coord   ->
            setEnum32 b pos coord;
        | GCclip_y_origin coord   ->
            setEnum32 b pos coord;
        | GCclip_mask pixmap  ->
            setEnum32 b pos pixmap;
        | GCdash_offset int   ->
            setEnum32 b pos int;
        | GCdashes int  ->
            setEnum32 b pos int;
        | GCarc_mode arcMode   ->
      setEnum32 b pos arcMode);
  *)
      iter5 b (pos+4) (Obj.magic tail) 
      
let setGCattributes b pos list =
  iter5 b (pos+4) list (* (Sort.list (<) list)) *)

let rec strListLength = function
	 [] -> 0
       | nom::tl ->
	      1+(String.length nom)+(strListLength tl)

let getCharInfo buffer pos =
    {
    char_lbearing = getInt16 buffer pos;
    char_rbearing = getInt16 buffer (pos+2);
    char_width = getInt16 buffer (pos+4);
    char_ascent = getInt16 buffer (pos+6);
    char_descent = getInt16 buffer (pos+8);
    char_attributes = getCard16 buffer (pos+10)
   }

let getFontProp buffer pos =
    (getEnum32 buffer pos,getCard32 buffer (pos+4))

let getOneFontInfo b =
   {
  font_min_bounds = getCharInfo b 8;
  font_max_bounds = getCharInfo b 24;
  font_min_char_or_byte2 = getCard16 b 40;
  font_max_char_or_byte2 = getCard16 b 42;
  font_default_char = getCard16 b 44;
  font_draw_direction = getEnum8 b 48;
  font_min_byte1 = getCard8 b 49;
  font_max_byte1 = getCard8 b 50;
  font_all_char_exist = getEnum8 b 51;
  font_ascent = getInt16 b 52;
  font_descent = getInt16 b 54;
  font_properties = 
  (let (p,n) = (ref [],getCard16 b 46)
  in
  for i=0 to n-1 do
    p:=(getFontProp b (60+i*8))::(!p)
  done; !p)              
}

let copyDatas b (gc:gc) (src:window) src_x src_y (dst:window) dst_x dst_y dx dy =
  setEnum32 b 4 src;
  setEnum32 b 8 dst;
  setEnum32 b 12 gc;
  setCard16 b 16 src_x;
  setCard16 b 18 src_y;
  setCard16 b 20 dst_x;
  setCard16 b 22 dst_y;
  setCard16 b 24 dx;
  setCard16 b 26 dy

let rec iter61 setOne b pos list =
  match list with 
    [] -> ()
  | head :: tail ->
      setOne b pos head;
      iter61 setOne b (pos+4) tail

let rec iter62 setOne b pos list =
  match list with 
    [] -> ()
  | head :: tail ->
      setOne b pos head;
      iter62 setOne b (pos+8) tail

let rec iter63 setOne b pos list =
  match list with 
    [] -> ()
  | head :: tail ->
      setOne b pos head;
      iter63 setOne b (pos+12) tail
      
let rec iter61setOnePoint b pos list =
  match list with 
    [] -> ()
  | (x,y) :: tail ->
      setCard16 b pos x;
      setCard16 b (pos+2) y;
      iter61setOnePoint b (pos+4) tail
      
let drawPoly1setOnePoint opcode (window:window) (gc: gc) list =
  let n = List.length list  in
  let b = newWinRequest opcode (3+n) window  in
  setEnum32 b 8 gc;
  iter61setOnePoint b 12 list;
  b
      
let drawPoly2 setOne opcode (window:window) (gc: gc) list =
  let n = List.length list  in
  let b = newWinRequest opcode (3+(n*2)) window  in
  setEnum32 b 8 gc;
  iter62 setOne b 12 list;
  b

let rec iter62setOneSegment b pos list =
  match list with 
    [] -> ()
  | (x1,y1,x2,y2) :: tail ->
      setCard16 b pos x1;
      setCard16 b (pos+2) y1;
      setCard16 b (pos+4) x2;
      setCard16 b (pos+6) y2;
      iter62setOneSegment b (pos+8) tail

let drawPoly2setOneSegment opcode (window:window) (gc: gc) list =
  let n = List.length list  in
  let b = newWinRequest opcode (3+(n*2)) window  in
  setEnum32 b 8 gc;
  iter62setOneSegment b 12 list;
  b
  
let drawPoly3 setOne opcode (window:window) (gc: gc) list =
  let n = List.length list  in
  let b = newWinRequest opcode (3+(n*3)) window  in
  setEnum32 b 8 gc;
  iter63 setOne b 12 list;
  b

let setOnePoint  b pos (x,y) =
    setCard16 b pos x;
    setCard16 b (pos+2) y
  
let setOneSegment  b pos (x1,y1,x2,y2) =
    setCard16 b pos x1;
    setCard16 b (pos+2) y1;
    setCard16 b (pos+4) x2;
    setCard16 b (pos+6) y2

   
let setOneArc b pos (x1,y1,x2,y2,start,stop) =
      setCard16 b pos x1;
      setCard16 b (pos+2) y1;
      setCard16 b (pos+4) x2;
      setCard16 b (pos+6) y2;
      setCard16 b (pos+8) start;
      setCard16 b (pos+10) stop
        
   
let textItemLength = function
    Text8 (delta,string,pos,len) -> 2+len
  | Text16 (tab,delta) -> 2+2*(Array.length tab)
  | ShiftFont fid -> 5 


let rec textItemListLength = function
    [] -> 0
  | h::tl -> 
      (textItemLength h)+(textItemListLength tl)

let rec setTextItem8 b pos items = 
  match items with
    [] -> ()
  | (Text8 (delta,string,spos,len)) :: tail -> 
      setCard8 b pos len;
      setCard8 b (pos+1) delta;
      String.blit string spos b (pos+2) len;
      setTextItem8 b (pos+2+len) tail
  | (ShiftFont fid) :: tail ->
      setCard8 b pos 255;
      setCard8 b (pos+1) (font_to_id fid lsr 24);
      setCard8 b (pos+2) ((font_to_id fid lsr 16) land 255);
      setCard8 b (pos+3) ((font_to_id fid lsr 8) land 255);
      setCard8 b (pos+4) (font_to_id fid land 255);
      setTextItem8 b (pos+5) tail
  | _ -> raise (Invalid_argument "setTextItem")

let rec setTextItem16 b pos items = 
  match items with
    [] -> ()
  | (Text16 (tab,delta)) :: tail  ->
      let n = Array.length tab
      in
      setCard8 b pos n;
      setCard8 b (pos+1) delta;
      setString16 b (pos+2) tab;
      setTextItem16 b (pos+2+2*n) tail
  | (ShiftFont fid) :: tail ->
      setCard8 b pos 255;
      setCard8 b (pos+1) (font_to_id fid lsr 24);
      setCard8 b (pos+2) ((font_to_id fid lsr 16) land 255);
      setCard8 b (pos+3) ((font_to_id fid lsr 8) land 255);
      setCard8 b (pos+4) (font_to_id fid land 255);
      setTextItem16 b (pos+5) tail
  | _ -> raise (Invalid_argument "setTextItem")
      

(* REquest + REply *)

let createWindowReq 
    wid (parent: window)
  x y dx dy depth wa_class (visual: visual) border_width swa =
  let mask, n, swa = count_args swa in
(*  let n = List.length swa in *)
  let b = newWinRequest X_CreateWindow (8+n) wid
  in
  setCard8 b 1 depth;
  setEnum32 b 8 parent;
  setCard16 b 12 x;
  setCard16 b 14 y;
  setCard16 b 16 dx;
  setCard16 b 18 dy;
  setCard16 b 20 border_width;
  setEnum16 b 22 wa_class;
  setEnum32 b 24 visual;
  setCard32 b 28 mask;
  setWindowAttributes b 28 swa;
  b

let changeWindowAttributesReq window swa =
  let mask, n, swa = count_args swa in
(*  let n = List.length swa in *)
  let b = newWinRequest X_ChangeWindowAttributes (3+n) window
  in
  setCard32 b 8 mask;
  setWindowAttributes b 8 swa;
  b

let getWindowAttributesReq = simpleWindowRequest X_GetWindowAttributes

let getWindowAttributesRep wa =
  { gwa_backing_store = getEnum8 wa 1;
    gwa_visual  = getEnum32 wa 8;
    gwa_wa_class = getEnum16 wa 12;
    gwa_bit_gravity = getEnum8 wa 14;
    gwa_win_gravity = getEnum8 wa 15;
    gwa_backing_planes = getCard32 wa 16;
    gwa_backing_pixel = getEnum32 wa 20;
    gwa_save_under  = getEnum8 wa 24;
    gwa_map_is_installed = getEnum8 wa 25;
    gwa_map_state  = getEnum8 wa 26;
    gwa_override_redirect = getEnum8 wa 27;
    gwa_colormap  = getEnum32 wa 28;
    gwa_all_event_mask = list_of_mask (getCard32 wa 32);
    gwa_your_event_mask = list_of_mask (getCard32 wa 36);
    gwa_dont_propagate_mask = list_of_mask (getCard16 wa 40);
  }
    
let destroyWindowReq = simpleWindowRequest X_DestroyWindow

let destroySubwindowsReq = simpleWindowRequest X_DestroySubwindows

let changeSaveSetReq window op =
  let b = newWinRequest X_ChangeSaveSet 2 window
  in
  setEnum8 b 1 op;
  b

let reparentWindowReq window (parent: window) x y =
  let b = newWinRequest X_ReparentWindow 4 window
  in
  setEnum32 b 8 parent;
  setCard16 b 12 x;
  setCard16 b 14 y;
  b

let mapWindowReq = simpleWindowRequest X_MapWindow 
    
let mapSubwindowsReq = simpleWindowRequest X_MapSubwindows

let unmapWindowReq = simpleWindowRequest X_UnmapWindow

let unmapSubwindowsReq = simpleWindowRequest X_UnmapSubwindows

let configureWindowReq win cw =
  let mask, n, cw = count_args cw in
(*  let n = List.length cw in *)
  let b = newWinRequest X_ConfigureWindow (3+n) win
  in
  setCard32 b 8 mask;
  setConfigureWindow b 8 cw;
  b

let circulateWindowReq (win : window) (sens : direction) =
    let b = newWinRequest X_CirculateWindow 2 win in
    setEnum8 b 1 sens;
    b

let getGeometryReq win = newWinRequest X_GetGeometry 2 win
let getGeometryRep r =
  {
  gg_root = getEnum32 r 8;
  gg_x = getInt16 r 12;
  gg_y = getInt16 r 14;
  gg_width = getCard16 r 16;
  gg_height =getCard16 r 18;
  gg_border_width = getCard16 r 20;
  gg_depth = getCard8 r 1;
} 

let queryTreeReq win = newWinRequest X_QueryTree 2 win
let queryTreeRep r =
  let n = getCard16 r 16 in
  let list = ref [] in
  for i = 1 to n do
    list := (getEnum32 r ((7+i)*4))::(!list)
  done;
  { qt_root = getEnum32 r 8;
    qt_parent = getEnum32 r 12;
    qt_subwindows = !list;
  } 

let internAtomReq string only_if_exists =
  let n = String.length string
  in
  let b = newRequest X_InternAtom (2+(strLen n))
  in
  setEnum8 b 1 only_if_exists;
  setString b 8 string;
  setCard16 b 4 n;
  b

let internAtomRep r = (getEnum32 r 8: atom)

let getAtomNameReq (atom: atom) =
  let b = newRequest X_GetAtomName 2
  in
  setEnum32 b 4 atom;
  b

let getAtomNameRep r = getString r 32 (getCard16 r 8)

let changePropertyReq window mode (property:atom) (property_type:atom) format buffer =
  let n = String.length buffer
  in
  if (n mod format)<>0 then
    raise (Invalid_argument "ChangeProperty: incompatible format")
  else
    let b = newWinRequest X_ChangeProperty (6+(strLen n)) window
    in
    setEnum8 b 1 mode;
    setEnum32 b 8 property;
    setEnum32 b 12 property_type;
    setCard8 b 16 (format*8);
    setCard32 b 20 (n/format);
    setString b 24 buffer;
    b

let deletePropertyReq win (property:atom) =
  let b = newWinRequest X_DeleteProperty 3 win
  in
  setEnum32 b 8 property;
  b
      
let getPropertyReq win mode (property:atom) (property_type:atom) offset lenght =
  let b = newWinRequest X_GetProperty 6 win
  in
  setEnum8 b 1 mode;
  setEnum32 b 8 property;
  setEnum32 b 12 property_type;
  setCard32 b 16 offset;
  setCard32 b 20 lenght;	
  b
    
let getPropertyRep r = 
  let t = getEnum32 r 8
  and format = (getCard8 r 1)/8
  and reste = getCard32 r 12
  in
  if atom_to_id t=0 then
    raise Not_found
  else
    { gp_type = t;
      gp_format = format;
      gp_length = getCard32 r 16;
      gp_value = getString r 32 ((getCard32 r 16) *format);
      gp_left = reste;
    }

let listPropertiesReq = simpleWindowRequest X_ListProperties

let listPropertiesRep r =
  let n = getCard16 r 8
  in
  let list = ref []
  in
  for i = 1 to n do
    list := (getEnum32 r ((7+i)*4))::(!list)
  done;
  (!list: atom list)

let setSelectionOwnerReq win (selection : atom) (time : time) =
  let b = newWinRequest X_SetSelectionOwner 4 win
  in
  setEnum32 b 8 selection;
  setTime b 12 time;
  b

let (getSelectionOwnerReq : atom -> string) = 
  simpleRequest X_GetSelectionOwner

let getSelectionOwnerRep r = (getEnum32 r 8 : window)

let convertSelectionReq win (selection : atom)
    (target : atom) (property : atom) (time : time) =
  let b = newWinRequest X_ConvertSelection 6 win
  in
  setEnum32 b 8 selection;
  setEnum32 b 12 target;
  setEnum32 b 16 property;
  setTime b 20 time;
  b

let sendEventReq win (propagate : bool)
    (event_mask : eventMask list) (event : event) =
  let b = newWinRequest X_SendEvent 11 win
  in
  setEnum8 b 1 propagate;
  setCard32 b 8 (mask_of_list event_mask);
  setString b 12 (Conv_event.convertEvent2Core event);
  b

let grabPointerReq win owner_events event_mask pointer_mode
    keyboard_mode confine_to cursor time =
  let b = newWinRequest X_GrabPointer 6 win
  in
  setEnum8 b 1 owner_events;
  setEnum32 b 4 win;
  setCard16 b 8 (mask_of_list event_mask);
  setEnum8 b 10 pointer_mode;
  setEnum8 b 11 keyboard_mode;
  setEnum32 b 12 confine_to;
  setEnum32 b 16 cursor;
  setTime b 20 time;
  b

let grabPointerRep r =
  let status = getEnum8 r 1
  in
  if status <>0 then
    raise (GrabError (Obj.magic status))

let ungrabPointerReq time = 
  let b = newRequest X_UngrabPointer 2 in
  setTime b 4 time;
  b

let grabButtonReq win owner_events event_mask pointer_mode
    keyboard_mode (confine_to: window) (cursor: cursor) button modifiers =
  let b = newWinRequest X_GrabButton 6 win
  in
  setEnum8 b 1 owner_events;
  setCard16 b 8 (mask_of_list event_mask);
  setEnum8 b 10 pointer_mode;
  setEnum8 b 11 keyboard_mode;
  setEnum32 b 12 confine_to;
  setEnum32 b 16 cursor;
  setEnum8 b 20 (int_of_enum button);
  setCard16 b 22 modifiers;
  b

let ungrabButtonReq win button modifiers =
  let b = newWinRequest X_UngrabButton 3 win
  in
  setEnum8 b 1 button;
  setCard16 b 8 modifiers;
  b

let changeActivePointerGrabReq event_mask (cursor: cursor) time =
  let b = newRequest X_ChangeActivePointerGrab 4
  in
  setEnum32 b 4 cursor;
  setTime b 8 time;
  setCard16 b 12 (mask_of_list event_mask);
  b

let grabKeyboardReq win owner_events pointer_mode keyboard_mode time =
  let b = newWinRequest X_GrabKeyboard 4 win
  in
  setEnum8 b 1 owner_events;
  setTime b 8 time;
  setEnum8 b 12 pointer_mode;
  setEnum8 b 13 keyboard_mode;
  b

let grabKeyboardRep = grabPointerRep

let ungrabKeyboardReq time  = 
  let b = newRequest X_UngrabKeyboard 2 in
  setTime b 4 time;
  b
    

let grabKeyReq win owner_events pointer_mode keyboard_mode key modifiers =
  let b = newWinRequest X_GrabKey 4 win
  in
  setEnum8 b 1 owner_events;
  setCard16 b 8 modifiers;
  setCard8 b 10 key;
  setEnum8 b 11 pointer_mode;
  setEnum8 b 12 keyboard_mode;
  b

let ungrabKeyReq win key modifiers =
  let b = newWinRequest X_UngrabKey 3 win
  in
  setCard8 b 1 key;
  setCard16 b 8 modifiers;
  b

let allowEventsReq mode time =
  let b= newRequest X_AllowEvents 2
  in
  setEnum8 b 1 mode;
  setTime b 4 time;
  b

let grabServerReq () = emptyRequest X_GrabServer

let ungrabServerReq () = emptyRequest X_UngrabServer

let queryPointerReq = simpleWindowRequest X_QueryPointer

let queryPointerRep r =
  if (getEnum8 r 1)=0 then
    raise OutOfScreen;
  { qp_root = getEnum32 r 8;
    qp_win = getEnum32 r 12;
    qp_root_x = getInt16 r 16;
    qp_root_y = getInt16 r 18;
    qp_win_x = getInt16 r 20;
    qp_win_y = getInt16 r 22;
    qp_modifiers = list_of_mask (getCard16 r 24) 
  } 

let getMotionEventsReq win start stop =
  let b = newWinRequest X_GetMotionEvents 3 win
  in
  setTime b 8 start;
  setTime b 12 stop;
  b

let getMotionEventsRep r =
  let  list = ref []
  in 
  let n = getCard32 r 8
  in
  for i = 1 to n do
    list := { me_win = getEnum32 r (24+i*8);
	      me_x = getInt16 r (26+i*8);
	      me_y = getInt16 r (28+i*8);
            } 
       ::(!list)
  done;
  !list

let translateCoordinatesReq src_window (dst_window: window) x y =
  let b = newWinRequest X_TranslateCoordinates 4 src_window
  in
  setEnum32 b 8 dst_window;
  setCard16 b 12 x;
  setCard16 b 14 y;
  b

let translateCoordinatesRep r =
  if (getEnum8 r 1)=0 then
    raise OutOfScreen;
  { me_win =getEnum32 r 8;
    me_x = getInt16 r 12;
    me_y = getInt16 r 14 }

let warpPointerReq src_window src_x src_y src_width src_height
    (dst_window: window) x y =
  let b = newWinRequest X_WarpPointer 6 src_window
  in
  setEnum32 b 8 dst_window;
  setCard16 b 12 src_x;
  setCard16 b 14 src_y;
  setCard16 b 16 src_width;
  setCard16 b 18 src_height;
  setCard16 b 20 x;
  setCard16 b 22 y;
  b

let setInputFocusReq window revert_to time =
  let b = newWinRequest X_SetInputFocus 3 window
  in
  setEnum8 b 1 revert_to;
  setTime b 8 time;
  b

let getInputFocusReq () = emptyRequest X_GetInputFocus

let getInputFocusRep r = { gif_win = getEnum32 r 8;
                           gif_mode = getEnum8 r 1 }

let queryKeymapReq () =  emptyRequest X_QueryKeymap

let queryKeymapRep r =
  let (t: int array) = Array.create 32 0
  in
  for i=0 to 31 do
    t.(i) <- getCard8 r (i+8)
  done;
  t

let openFontReq (fid: font) name =
  let n = String.length name
  in
  let b = newRequest X_OpenFont (3+(strLen n))
  in
  setString b 12 name;
  setEnum32 b 4 fid;
  setCard16 b 8 n;
  b

let closeFontReq = simpleFontRequest X_CloseFont 

let queryFontReq = simpleFontRequest X_QueryFont

let queryFontRep b =
  let nchars = getCard32 b 56 in
  let chars = Array.init nchars 
      (fun i -> getCharInfo b (60+(getCard16 b 46)*8+12*i))
  in
  { qf_info = getOneFontInfo b;
    qf_chars = chars }

let queryTextExtentsReq (fid: font) string =
  let n = String.length string
  in
  let b = newRequest X_QueryTextExtents (2+(strLen (n*2)))
  in
  setCard8 b 1 1;
  setEnum32 b 4 fid;
  for i=0 to (n-1) do
    setEnum16 b (8+2*i) string.[i]
  done;
  b
	
let queryTextExtentsRep r =
  { qte_direction =	getEnum8 r 1;
    qte_f_ascent = getInt16 r 8;
    qte_f_descent = getInt16 r 10;
    qte_ovll_ascent = getInt16 r 12;
    qte_ovll_descent = getInt16 r 14;
    qte_ovll_width = getInt32 r 16;
    qte_ovll_left = getInt32 r 20;
    qte_ovll_right = getInt32 r 24;
  } 

let listFontsReq pattern max_names =
  let n = String.length pattern in
  let b = newRequest X_ListFonts (2+(strLen n))
  in
  setCard16 b 4 max_names;
  setCard16 b 6 n;
  setString b 8 pattern;
  b

let listFontsRep r = get_str_list r 32 (getCard16 r 8)

let listFontsWithInfoReq pattern max_names = 
  failwith "listFontsWithInfoReq: Not implemented"
let listFontsWithInfoRep r = 
  failwith "listFontsWithInfoRep: Not implemented"

let setFontPathReq list =
  let n = strListLength list
  in
  let b = newRequest X_SetFontPath (2+(strLen n))
  in
  setCard16 b 4 (List.length list);
  set_str_list b 8 list;
  b

let getFontPathReq () = emptyRequest X_GetFontPath

let getFontPathRep r = get_str_list r 32 (getCard16 r 8)

let createPixmapReq (pid: pixmap) (window: window) dx dy depth =
  let b = newRequest X_CreatePixmap 4
  in 
  setCard8 b 1 depth;
  setEnum32 b 4 pid;
  setEnum32 b 8 window;
  setCard16 b 12 dx;
  setCard16 b 14 dy;
  b

let freePixmapReq = simpleWindowRequest X_FreePixmap

(*  
let createGCReq (gcid: gc) (win: window) sgca =
  let b = setGCattributes X_CreateGC 4 sgca in
  setEnum32 b 4 gcid;
  setEnum32 b 8 win;
  b

let changeGCReq (gcid: gc) sgca =
  let b = setGCattributes X_ChangeGC 3 sgca in
  setEnum32 b 4 gcid;
  b
    *)

let createGCReq (gcid: gc) (win: window) sgca =
  let mask, n, sgca = count_args sgca in
(*  let n = List.length sgca in *)
  let b = newRequest X_CreateGC (4+n)
  in
  setEnum32 b 4 gcid;
  setEnum32 b 8 win;
  setCard32 b 12 mask;
  setGCattributes b 12 sgca;
  b

let changeGCReq (gcid: gc) sgca =
  let mask, n, sgca = count_args sgca in
(*  let n = List.length sgca in *)  
  let b = newRequest X_ChangeGC (3+n)
  in
  setEnum32 b 4 gcid;
  setCard32 b 8 mask;
  setGCattributes b 8 sgca;
  b


let copyGCReq (src: gc) (dst: gc) mask =
  let b = newRequest X_CopyGC 4
  in
  setEnum32 b 4 src;
  setEnum32 b 8 dst;
  setCard32 b 12 (mask_of_list mask);
  b

let setDashesReq (gc: gc) dash_offset dashes =
  let n = String.length dashes in
  let b = newRequest X_SetDashes (3+(strLen n))
  in
  setEnum32 b 4 gc;
  setCard16 b 8 dash_offset;
  setCard16 b 10 n;
  setString b 12 dashes;
  b

    let rec iter8 b pos list =
    match list with
      [] -> ()
    | (x,y,dx,dy) :: tail ->
        setCard16 b pos x;
        setCard16 b (pos+2) y;
        setCard16 b (pos+4) dx;
        setCard16 b (pos+6) dy;
        iter8 b (pos+8) tail

let setClipRectanglesReq (gc: gc) ordering clip_x clip_y rects =
  let n = List.length rects in
  let b = newRequest X_SetClipRectangles (3+n*2)
  in
  setEnum8 b 1 ordering;
  setEnum32 b 4 gc;
  setCard16 b 8 clip_x;
  setCard16 b 10 clip_y;
  iter8 b 12 rects;
  b

let (freeGCReq: gc -> string) = simpleRequest X_FreeGC

let clearAreaReq window x y dx dy exposures =
  let b =  newWinRequest X_ClearArea 4 window
  in
  setEnum8 b 1 exposures;
  setCard16 b 8 x;
  setCard16 b 10 y;
  setCard16 b 12 dx;
  setCard16 b 14 dy;
  b

let clearWindowReq window =
  let b = Display.newBuffer 4 in
  String.unsafe_fill b 0 16 '\000';
  setEnum8 b 0 X_ClearArea;
  setCard16 b 2 4;
  setEnum32 b 4 window;
  b

let copyAreaReq (gc:gc) (src:window) src_x src_y (dst:window) dst_x dst_y dx dy =
  let b = newRequest X_CopyArea 7
  in
  setEnum32 b 4 src;
  setEnum32 b 8 dst;
  setEnum32 b 12 gc;
  setCard16 b 16 src_x;
  setCard16 b 18 src_y;
  setCard16 b 20 dst_x;
  setCard16 b 22 dst_y;
  setCard16 b 24 dx;
  setCard16 b 26 dy;
  b

let copyPlaneReq gc src src_x src_y dst dst_x dst_y dx dy planes =
  let b = newRequest X_CopyPlane 8
  in
  setCard32 b 28 planes;
  setEnum32 b 4 src;
  setEnum32 b 8 dst;
  setEnum32 b 12 gc;
  setCard16 b 16 src_x;
  setCard16 b 18 src_y;
  setCard16 b 20 dst_x;
  setCard16 b 22 dst_y;
  setCard16 b 24 dx;
  setCard16 b 26 dy;
  b

let polyPointReq win gc mode list  = 
  let b = drawPoly1setOnePoint X_PolyPoint win gc list in
  setEnum8 b 1 mode;
  b

let polyLineReq win gc mode list = 
  let b = drawPoly1setOnePoint X_PolyLine win gc list in
  setEnum8 b 1 mode;
  b

let polySegmentReq = drawPoly2setOneSegment X_PolySegment

let polyRectangleReq = drawPoly2setOneSegment X_PolyRectangle

let polyArcReq = drawPoly3 setOneArc X_PolyArc

let rec iter9 b pos list =
  match list with
    [] -> ()
  | point :: tail ->
      setOnePoint b pos point;
      iter9 b (pos+4) tail
      
let fillPolyReq window (gc: gc) shape coord_mode points =
  let n = List.length points
  in
  let b = newWinRequest X_FillPoly (4+n) window
  in
  setEnum32 b 8 gc;
  setEnum8 b 12 shape;
  setEnum8 b 13 coord_mode;
  iter9 b 16 points;
  b

let polyFillRectangleReq = drawPoly2 setOneSegment X_PolyFillRectangle
  
let polyFillArcReq = drawPoly3 setOneArc X_PolyFillArc

let putImageReq (gc: gc) window x y dx dy left_pad depth format image =
  let n = String.length image
  in
  let b = newWinRequest X_PutImage (6+(strLen n)) window
  in
  setEnum8 b 1 format;
  setEnum32 b 8 gc;
  setCard16 b 12 dx;
  setCard16 b 14 dy;
  setCard16 b 16 x;
  setCard16 b 18 y;
  setCard8  b 20 left_pad;
  setCard8  b 21 depth;
  setString b 24 image;
  b

let getImageReq window x y dx dy plane_mask format =
  let b = newWinRequest X_GetImage 5 window
  in
  setEnum8 b 1 format;
  setCard16 b 8 x;
  setCard16 b 10 y;
  setCard16 b 12 dx;
  setCard16 b 14 dy;
  setCard32 b 16 plane_mask;
  b
	
let getImageRep r =
  { gi_depth = getCard8 r 1;
    gi_visual = getEnum32 r 8;
    gi_image = getString r 32 (4*(getCard32 r 4))
  } 

let polyText8Req window (gc: gc) x y list =
  let n = textItemListLength list
  in 
  let b = newWinRequest X_PolyText8 (4+(strLen n)) window
  in
  setEnum32 b 8 gc;
  setCard16 b 12 x;
  setCard16 b 14 y;
  setTextItem8 b 16 list;
  for i=16+n to 15+(strLen n)*4 do
    setCard8 b i 0
  done;
  b

let polyText16Req window (gc: gc) x y list =
  let n = textItemListLength list
  in 
  let b = newWinRequest X_PolyText8 (4+(strLen n)) window
  in
  setEnum32 b 8 gc;
  setCard16 b 12 x;
  setCard16 b 14 y;
  setTextItem16 b 16 list;
  for i=16+n to 15+(strLen n)*4 do
    setCard8 b i 0
  done;
  b

let imageText8Req window (gc: gc) x y string =
  let n = String.length string
  in
  let b = newWinRequest X_ImageText8 (4+(strLen n)) window
  in
  setCard8 b 1 n;
  setEnum32 b 8 gc;
  setCard16 b 12 x;
  setCard16 b 14 y;
  setString b 16 string;
  b
    
let imageSubText8Req window (gc: gc) x y string pos len =
  let b = newWinRequest X_ImageText8 (4+(strLen len)) window
  in
  setCard8 b 1 len;
  setEnum32 b 8 gc;
  setCard16 b 12 x;
  setCard16 b 14 y;
  String.blit string pos b 16 len;
  b
    
let imageText16Req window (gc: gc) x y array =
  let n = Array.length array
  in
  let b = newWinRequest X_ImageText16 (4+(strLen (2*n))) window
  in
  setCard8 b 1 n;
  setEnum32 b 8 gc;
  setCard16 b 12 x;
  setCard16 b 14 y;
  for i = 0 to n - 1 do
    setCard16 b (16+i) array.(i)
  done;
  b
	  
let createColormapReq (cid: colormap)  (window: window) (visual: visual)
  alloc =
  let b = newRequest X_CreateColormap 4
  in
  setEnum8 b 1 alloc;
  setEnum32 b 4 cid;
  setEnum32 b 8 window;
  setEnum32 b 12 visual;
  b

let freeColormapReq = simpleColormapRequest X_FreeColormap

let copyColormapAndFreeReq (new_cid: colormap) (old_cid: colormap) =
  doubleRequest X_CopyColormapAndFree new_cid old_cid

let installColormapReq = simpleColormapRequest X_InstallColormap

let uninstallColormapReq = simpleColormapRequest X_UninstallColormap

let listInstalledColormapsReq = simpleWindowRequest X_ListInstalledColormaps

let (listInstalledColormapsRep : string -> colormap list) = function r ->
  get_enum32_list r 32 (getCard16 r 8)

let allocColorReq (cmap: colormap) red green blue =
  let b = newRequest X_AllocColor 4
  in
  setEnum32 b 4 cmap;
  setCard16 b 8 red;
  setCard16 b 10 green;
  setCard16 b 12 blue;
  b
	
let allocColorRep r =
  { ac_pixel = getEnum32 r 16;
    ac_color = 
    { red = getCard16 r 8;
      green = getCard16 r 10;
      blue = getCard16 r 12;
    } 
  } 

let allocNamedColorReq (cmap: colormap) name =
  let n = String.length name
  in
  let b = newRequest X_AllocNamedColor (3+(strLen n))
  in
  setEnum32 b 4 cmap;
  setCard16 b 8 n;
  setString b 12 name;
  b

let allocNamedColorRep r =
  {anc_pixel = getEnum32 r 8;
   anc_exact = { red = getCard16 r 12;
                 green = getCard16 r 14;
                 blue = getCard16 r 16;
               };
    anc_visual = { red = getCard16 r 18;
                   green = getCard16 r 20;
                   blue = getCard16 r 22; 
                 } 
  } 

let allocColorCellsReq (cmap: colormap) colors planes contiguous =
  let b = newRequest X_AllocColorCells 3
  in
  setEnum8 b 1 contiguous;
  setEnum32 b 4 cmap;
  setCard16 b 8 colors;
  setCard16 b 10 planes;
  b

let allocColorCellsRep r =
  let n = getCard16 r 8
  in
  { acc_pixels = Array.of_list (get_enum32_list r 32 n);
    acc_masks = Array.of_list (get_card32_list r (32+4*n) (getCard16 r 10))
  } 

let allocColorPlanesReq (cmap : colormap) colors reds greens blues contiguous =
  let b = newRequest X_AllocColorPlanes 4
  in
  setEnum8 b 1 contiguous;
  setEnum32 b 4 cmap;
  setCard16 b 8 colors;
  setCard16 b 10 reds;
  setCard16 b 12 greens;
  setCard16 b 14 blues;
  b
	
let allocColorPlanesRep r = 
  let n = getCard16 r 8
  in
  { acp_pixels = Array.of_list (get_enum32_list r 32 n);
    acp_mask = { red = getCard32 r 12;
                 green = getCard32 r 16;
                 blue = getCard32 r 20;
               } 
  } 

let freeColorsReq (cmap: colormap) plane_mask (pixels: pixel list) =
  let n = List.length pixels
  in
  let b = newRequest X_FreeColors (3+n)
  in
  setEnum32 b 4 cmap;
  setCard32 b 8 plane_mask;
  set_enum32_list b 12 pixels;
  b

let rec setColorItem b pos = function 
    [] -> ()
  | (pixel,red,green,blue,mask)::tl ->
      setEnum32 b pos (pixel : pixel);
      setCard16 b (pos+4) red;
      setCard16 b (pos+6) green;
      setCard16 b (pos+8) blue;
      setCard8 b (pos+10) (mask_of_list mask);
      setColorItem b (pos+12) tl
      
let storeColorsReq (cmap : colormap) list =
  let n = List.length list  in
  let b = newRequest X_StoreColors (2+3*n)  in
  setEnum32 b 4 cmap;
  setColorItem b 8 list;
  b

let storeNamedColorReq (cmap: colormap) (pixel: pixel) mask name =
  let n = String.length name
  in
  let b = newRequest X_StoreNamedColor (4+(strLen n))
  in
  setCard8 b 1 (mask_of_list mask);
  setEnum32 b 4 cmap;
  setEnum32 b 8 pixel;
  setCard16 b 12 n;
  setString b 16 name;
  b
  
let queryColorsReq (cmap: colormap) list =
  let n = List.length list
  in
  let b = newRequest X_QueryColors (2+n)
  in
  setEnum32 b 4 cmap;
  set_enum32_list b 8 list;
  b
	

let queryColorsRep r =
  let n = getCard16 r 8
  and res = ref []
  in
  for i = 0 to n-1 do
    res := {
       red = getCard16 r (32+8*i);
       green = getCard16 r (34+8*i);
       blue = getCard16 r (36+8*i);
     } 
       :: (!res)
  done;
  Array.of_list  (!res)

let lookupColorReq (cmap: colormap) name =
  let n = String.length name
  in
  let b = newRequest X_LookupColor (3+(strLen n))
  in
  setEnum32 b 4 cmap;
  setCard16 b 8 n;
  setString b 12 name;
  b

let lookupColorRep r =
  { lc_exact = { red = getCard16 r 8;
                 green = getCard16 r 10;
                 blue = getCard16 r 12;
               } ;
    lc_visual = { red = getCard16 r 14;
                  green = getCard16 r 16;
                  blue = getCard16 r 18;
                } 
  }
  
let createCursorReq (cid: cursor) (src: pixmap) (mask: pixmap)
  fred fgreen fblue bred bgreen bblue x y =
  let b = newRequest X_CreateCursor 8
  in	
  setEnum32 b 4 cid;
  setEnum32 b 8 src;
  setEnum32 b 12 mask;
  setCard16 b 16 fred;
  setCard16 b 18 fgreen;
  setCard16 b 20 fblue;
  setCard16 b 22 bred;
  setCard16 b 24 bgreen;
  setCard16 b 26 bblue;
  setCard16 b 28 x;
  setCard16 b 30 y;
  b

let createGlyphCursorReq (cid: cursor) (src: font) (mask: font) src_char mask_char 
    fred fgreen fblue bred bgreen bblue =
  let b = newRequest X_CreateGlyphCursor 8
  in	
  setEnum32 b 4 cid;
  setEnum32 b 8 src;
  setEnum32 b 12 mask;
  setCard16 b 16 src_char;
  setCard16 b 18 mask_char;
  setCard16 b 20 fred;
  setCard16 b 22 fgreen;
  setCard16 b 24 fblue;
  setCard16 b 26 bred;
  setCard16 b 28 bgreen;
  setCard16 b 30 bblue;
  b

let (freeCursorReq : cursor -> string) = simpleRequest X_FreeCursor 

let recolorCursorReq (cursor: cursor) fred fgreen fblue bred bgreen bblue =
  let b = newRequest X_RecolorCursor 5
  in
  setEnum32 b 4 cursor;
  setCard16 b 8 fred;
  setCard16 b 10 fgreen;
  setCard16 b 12 fblue;
  setCard16 b 14 bred;
  setCard16 b 16 bgreen;
  setCard16 b 18 bblue;
  b

let queryBestSizeReq objet (window: window) width height =
  let b = newRequest X_QueryBestSize 3
  in
  setEnum8 b 1 objet;
  setEnum32 b 4 window;
  setCard16 b 8 width;
  setCard16 b 10 height;
  b
	
let queryBestSizeRep r = 
  { qbs_width = getCard16 r 8;
    qbs_height =getCard16 r 8;
  } 

let queryExtensionReq name =
  let n = String.length name
  in
  let b = newRequest X_QueryExtension (2+(strLen n))
  in
  setCard16 b 4 n;
  setString b 8 name;
  b
	
let queryExtensionRep r =
  { qe_present = getEnum8 r 8;
    qe_opcode = getCard8 r 9;
    qe_event = getCard8 r 10;
    qe_error = getCard8 r 11;
  } 

let listExtensionsReq () = emptyRequest X_ListExtensions

let listExtensionsRep r = get_str_list r 32 (getCard8 r 1)

let changeKeyboardMappingReq first array =
  let n = Array.length array in
  let m = Array.length array.(0) in
  let b= newRequest X_ChangeKeyboardMapping (2+n*m)
  in
  setCard8 b 1 n;
  setCard8 b 4 first;
  setCard8 b 5 m;
  for i = 0 to n-1 do
    set_card32_array b (8+i*m*4) array.(i)
  done;
  b

let getKeyboardMappingReq first m =
  let b = newRequest X_GetKeyboardMapping 2
  in
  setCard8 b 4 first;
  setCard8 b 5 m;
  b
	
let getKeyboardMappingRep r = 
  let n = getCard8 r 1 in
  let nm = getCard32 r 4 in
  let m = nm / n in
  let keycodes = Array.create m (get_card32_array r 32 n) in
   for i = 1 to m-1 do
     keycodes.(i) <- get_card32_array r (32+4*i*n) n
   done;
  { gkm_keysyms_per_keycode = n;
    gkm_keycodes = keycodes;
  } 

let changeKeyboardControlReq arg = 
  let mask, n, arg = count_args arg in
  (*  let n = List.length arg in *)
  let b = newRequest X_ChangeKeyboardControl (2+n) in
  changeKeyboardControl b 4 arg;
  setCard32 b 4 mask;
  b

let getKeyboardControlReq () = emptyRequest X_GetKeyboardControl
let getKeyboardControlRep r = 
  { 
    gkc_global_auto_repeat = getEnum8 r 1;
    gkc_led_mask = getCard32 r 8;
    gkc_key_click_percent = getCard8 r 12;
    gkc_bell_percent = getCard8 r 13;
    gkc_bell_pitch = getCard16 r 14;
    gkc_bell_duration = getCard16 r 16;
    gkc_auto_repeats = getString r 20 32;
  }

let bellReq percent =
  let b = newRequest X_Bell 1
  in
  setCard8 b 1 percent;
  b

let changePointerControlReq do_acc acc_num acc_den do_thres threshold =
  let b = newRequest X_ChangePointerControl 3
  in
  setCard16 b 4 acc_num;
  setCard16 b 6 acc_den;
  setCard16 b 8 threshold;
  setEnum8 b 10 do_acc;
  setEnum8 b 11 do_thres;
  b

let getPointerControlReq () = emptyRequest X_GetPointerControl

let getPointerControlRep r =
  { gpc_acc_num = getCard16 r 8;
    gpc_acc_den = getCard16 r 10;
    gpc_threshold = getCard16 r 12;
  } 

let setScreenSaverReq timeout interval prefer_blanking allow_exposures =
  let b = newRequest X_SetScreenSaver 3
  in
  setCard16 b 4 timeout;
  setCard16 b 6 interval;
  setEnum8 b 8 prefer_blanking;
  setEnum8 b 9 allow_exposures;
  b

let getScreenSaverReq () = emptyRequest X_GetScreenSaver

let getScreenSaverRep r =
  { gss_timeout = getCard16 r 8;
    gss_interval = getCard16 r 10;
    gss_pref_blanc = getEnum8 r 12;
    gss_allow_exp = getEnum8 r 13;
  } 

let changeHostsReq mode family name =
  let n = String.length name
  in
  let b = newRequest X_ChangeHosts (2+(strLen n))
  in
  setEnum8 b 1 mode;
  setEnum8 b 4 family;
  setCard16 b 6 n;
  setString b 8 name;
  b

let listHostsReq () = emptyRequest X_ListHosts

let listHostsRep r =
  let rec getHosts r pos nbr =
    if nbr = 0 then 
      []
    else
      let n = getCard16 r (pos+2)
      in
      (getEnum8 r pos,
       getString r (pos+4) n
	 )::(getHosts r (pos+4+4*(strLen n)) (nbr-1))
  in
  { lh_control = getEnum8 r 1;
    lh_list = getHosts r 32 (getCard16 r 8);
  }

let setAccessControlReq access = littleRequest X_SetAccessControl access

let setCloseDownModeReq mode = littleRequest X_SetCloseDownMode mode

let (killClientReq: int -> string)  = simpleRequest X_KillClient

let rotatePropertiesReq window delta (list: atom list) =
  let n = List.length list
  in
  let b = newWinRequest X_RotateProperties (3+n) window
  in
  setCard16 b 8 n;
  setCard16 b 10 delta;
  set_enum32_list b 12 list;
  b

let forceScreenSaverReq state = littleRequest X_ForceScreenSaver state

let setPointerMappingReq map =
  let n = String.length map
  in
  let b = newRequest X_SetPointerMapping (1+(strLen n))
  in
  setCard8 b 1 n;
  setString b 4 map;
  b

let setPointerMappingRep r =
  if (getEnum8 r 1) = 1 then
    raise MappingBusy

let getPointerMappingReq () = emptyRequest X_GetPointerMapping

let getPointerMappingRep r = getString r 32 (getCard8 r 1)

let setModifierMappingReq list =
  let n = List.length list
  in
  let b = newRequest X_SetModifierMapping (1+2*n)
  in
  setCard8 b 1 n;
  let rec setModifiers pos = function
      [] -> ()
    | h::t -> 
	if (Array.length h) <> 8 then
	  raise 
	    (Invalid_argument "setModifierMapping");
	for i = 0 to 7 do
	  setCard8 b (pos+i) h.(i)
	done;
	setModifiers (pos+8) t
  in
  setModifiers 4 list;
  b
	
let setModifierMappingRep r =
  let status =  getEnum8 r 1
  in
  if status = 1 then
    raise MappingBusy
  else
    if status = 2 then
      raise MappingFailed

let getModifierMappingReq () = emptyRequest X_GetModifierMapping

let getModifierMappingRep r =
  let n = getCard8 r 1 in
  Array.init 8 (fun i ->
    let tab = Array.create n 0
    in
    for j=0 to n-1 do
      tab.(j) <- getCard8 r (32+i*n+j)
    done;
    tab)
