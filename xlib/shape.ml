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

open Xtypes
open Xbuffer
open Display
open Xproto

module OldX = X

  
  
let install_extension dpy ext_name e =
  Conv_event.set_extension_target e.first_event
    (fun b -> 
      let win = window_to_id (getWindow b 4) in
      win
  );
  dpy.dpy_extensions <- (ext_name,e) :: dpy.dpy_extensions

let get_extension dpy ext_name = List.assq ext_name dpy.dpy_extensions 

  (************** types and constants **********)

let ext_name = "SHAPE"

type shapeKind = Bounding | Clip
type shapeOp = Set | Union | Intersect | Substract | Invert

type opcodes =
  X_ShapeQueryVersion
| X_ShapeRectangles
| X_ShapeMask
| X_ShapeCombine
| X_ShapeOffset
| X_ShapeQueryExtents
| X_ShapeSelectInput
| X_ShapeInputSelected
| X_ShapeGetRectangles

type shapeQueryVersionRep = {
    sqv_major_version : int;
    sqv_minor_version : int }

type shapeQueryExtentsRep = {
    sqe_bounding : bool;
    sqe_clip : bool;
    sqe_bounding_extents : rect;
    sqe_clip_extents : rect;
  }

type shapeGetRectanglesRep = {
    sgr_clip_order : clipOrder;
    sgr_rectangles : rect array;
  }
  
  (**************  protocole *******************)

let shapeQueryVersionReq shape_info =
  let b = newBuffer 1 in
  setEnum8 b 0 shape_info.major_opcode;
  setEnum8 b 1 X_ShapeQueryVersion;
  setCard16 b 2 1;
  b

let shapeQueryVersionRep r =
  { sqv_major_version = getCard16 r 8;
    sqv_minor_version = getCard16 r 10 }

let shapeRectanglesReq shape_info (dest_win : window) shape_kind shape_op 
    xoff yoff rects ordering =
  let n = List.length rects in
  let b = newBuffer (4+2*n) in
  setEnum8 b 0 shape_info.major_opcode;
  setEnum8 b 1 X_ShapeRectangles;
  setCard16 b 2 (4+2*n);
  setEnum8 b 4 shape_op;
  setEnum8 b 5 shape_kind;
  setEnum8 b 6 ordering;
  setWindow b 8 dest_win;
  setCard16 b 12 xoff;
  setCard16 b 14 yoff;
  let rec iter pos list =
    match list with
      (x,y,dx,dy) :: tail ->
        setCard16 b pos x;
        setCard16 b (pos+2) y;
        setCard16 b (pos+4) dx;
        setCard16 b (pos+6) dy;
        iter (pos+8) tail        
    | [] -> b
  in
  iter 16 rects

let shapeMaskReq shape_info (dest_win : window) dest_kind shape_op 
    xoff yoff (src_pix : pixmap) =
  let b = newBuffer 5 in
  setEnum8 b 0 shape_info.major_opcode;
  setEnum8 b 1 X_ShapeMask;
  setCard16 b 2 5;
  setEnum8 b 4 shape_op;
  setEnum8 b 5 dest_kind;
  setWindow b 8 dest_win;
  setCard16 b 12 xoff;
  setCard16 b 14 yoff;
  setWindow b 16 src_pix;
  b

let shapeCombineReq  shape_info (dest_win : window) dest_kind shape_op xoff yoff
    (src_win : window) src_kind =
  let b = newBuffer 5 in
  setEnum8 b 0 shape_info.major_opcode;
  setEnum8 b 1 X_ShapeCombine;
  setCard16 b 2 5;
  setEnum8 b 4 shape_op;
  setEnum8 b 5 dest_kind;
  setEnum8 b 6 src_kind;
  setWindow b 8 dest_win;
  setCard16 b 12 xoff;
  setCard16 b 14 yoff;
  setWindow b 16 src_win;
  b


let shapeOffsetReq shape_info (dest_win : window) dest_kind xoff yoff =
  let b = newBuffer 4 in
  setEnum8 b 0 shape_info.major_opcode;
  setEnum8 b 1 X_ShapeOffset;
  setCard16 b 2 4;
  setEnum8 b 4 dest_kind;
  setWindow b 8 dest_win;
  setCard16 b 12 xoff;
  setCard16 b 14 yoff;
  b

let shapeQueryExtentsReq shape_info (dest_win : window) =
  let b = newBuffer 2 in
  setEnum8 b 0 shape_info.major_opcode;
  setEnum8 b 1 X_ShapeQueryExtents;
  setCard16 b 2 2;
  setWindow b 4 dest_win;
  b

let shapeQueryExtentsRep r =
  { sqe_bounding = getEnum8 r 8;
    sqe_clip = getEnum8 r 9;
    sqe_bounding_extents =
    { xx = getInt16 r 12;
      yy = getInt16 r 14;
      dx = getCard16 r 16;
      dy = getCard16 r 18
    };
    sqe_clip_extents =
    { xx = getInt16 r 20;
      yy = getInt16 r 22;
      dx = getCard16 r 24;
      dy = getCard16 r 26
    }
  }

let shapeSelectInputReq shape_info (dest_win : window) enable =
  let b = newBuffer 3 in
  setCard8 b 0 shape_info.major_opcode;
  setEnum8 b 1 X_ShapeSelectInput;
  setCard16 b 2 3;
  setWindow b 4 dest_win;
  setEnum8 b 8 enable;
  b

let shapeInputSelectedReq shape_info (dest_win : window) =
  let b = newBuffer 2 in
  setEnum8 b 0 shape_info.major_opcode;
  setEnum8 b 1 X_ShapeGetRectangles;
  setCard16 b 2 2;
  setWindow b 4 dest_win;
  b

let shapeInputSelectedRep r = getEnum8 r 1

let shapeGetRectanglesReq shape_info (win : window) kind =
  let b = newBuffer 3 in
  setEnum8 b 0 shape_info.major_opcode;
  setEnum8 b 1 X_ShapeGetRectangles;
  setCard16 b 2 3;
  setWindow b 4 win;
  setEnum8 b 8 kind;
  b

let shapeGetRectanglesRep r =
  let n = getCard32 r 8 in
  { sgr_clip_order = getEnum8 r 1;
    sgr_rectangles = Array.init n
      (function i ->
          { xx = getInt16 r (32+i*8);
            yy = getInt16 r (34+i*8);
            dx = getCard16 r (36+i*8);
            dy = getCard16 r (38+i*8)
          }
    )
  }
    
    (*************** Events *************************)

module XshapeNotify = struct
    type t = {
        shape_kind : shapeKind;
        window : window;
        extents : rect;
        time : time;
        shaped : bool;
      }
  end
  
  (*************** Shape values *******************)

let queryExtension display =
  let qe = X.queryExtension display ext_name in
  if qe.qe_present then
    begin
      let e =  {
          major_opcode = qe.qe_opcode;
          first_event = qe.qe_event;  (* 1 seul event *)
          last_event = qe.qe_event;
          first_error = qe.qe_error;  (* pas d'erreur specifique *)
          last_error = qe.qe_error - 1;  
        }
      in
      install_extension display ext_name e
    end;
  qe.qe_present



module X = struct
    
(* Synchronisation *)
    
    let sendRequest dpy buffer = let _ = send_alone dpy buffer in ()
    
    let sendRequestAndGetReply dpy buffer wrapper =
      let event = Jeton.create ()
      in
      send_with_wrapper dpy buffer (parse_buffer event wrapper);
      Jeton.wait event

(* Requests *)
    
    let shapeQueryVersion dpy = 
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy (shapeQueryVersionReq shape_info)
      shapeQueryVersionRep
    
    let shapeRectangles dpy dest_win dest_kind xoff yoff rects op order =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy 
        (shapeRectanglesReq shape_info dest_win dest_kind op 
          xoff yoff rects order)
    
    let shapeMask dpy dest_win dest_kind xoff yoff src_pix op =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy
        (shapeMaskReq  shape_info dest_win dest_kind
          op xoff yoff src_pix)
    
    let shapeCombine dpy dest destKind xOff yOff src srcKind op =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy
        (shapeCombineReq shape_info  dest destKind op xOff yOff
          src srcKind)
    
    let shapeQueryExtents dpy win = 
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy
        (shapeQueryExtentsReq shape_info  win) shapeQueryExtentsRep
    
    let shapeSelectInput dpy win enable =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy (shapeSelectInputReq shape_info win enable)
    
    let shapeInputSelected dpy win =
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy
        (shapeInputSelectedReq shape_info win)
      shapeInputSelectedRep
    
    let shapeGetRectangles dpy win shape_kind =
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy
        (shapeGetRectanglesReq shape_info win shape_kind)
      shapeGetRectanglesRep
  
  
  end



module Xsync = struct
    
(* Synchronisation *)
    
    let sendRequest dpy buffer = 
      let event = Jeton.create ()
      in
      send_with_wrapper dpy buffer (parse_buffer event (function r -> ()));
      let _ = OldX.getInputFocus dpy in 
      Jeton.wait event
    
      
    let sendRequestAndGetReply dpy buffer wrapper =
      let event = Jeton.create ()
      in
      send_with_wrapper dpy buffer (parse_buffer event wrapper);
      Jeton.wait event

(* Requests *)
    
    let shapeQueryVersion dpy = 
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy (shapeQueryVersionReq shape_info)
      shapeQueryVersionRep
    
    let shapeRectangles dpy dest_win dest_kind xoff yoff rects op order =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy 
        (shapeRectanglesReq shape_info dest_win dest_kind op 
          xoff yoff rects order)
    
    let shapeMask dpy dest_win dest_kind xoff yoff src_pix op =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy
        (shapeMaskReq  shape_info dest_win dest_kind
          op xoff yoff src_pix)
    
    let shapeCombine dpy dest destKind xOff yOff src srcKind op =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy
        (shapeCombineReq shape_info  dest destKind op xOff yOff
          src srcKind)
    
    let shapeQueryExtents dpy win = 
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy
        (shapeQueryExtentsReq shape_info  win) shapeQueryExtentsRep
    
    let shapeSelectInput dpy win enable =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy (shapeSelectInputReq shape_info win enable)
    
    let shapeInputSelected dpy win =
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy
        (shapeInputSelectedReq shape_info win)
      shapeInputSelectedRep
    
    let shapeGetRectangles dpy win shape_kind =
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy
        (shapeGetRectanglesReq shape_info win shape_kind)
      shapeGetRectanglesRep
  
  
  end



module Xasync = struct
    
(* Synchronisation *)
    
    let sendRequest dpy buffer = 
      let event = Jeton.create ()
      in
      send_with_wrapper dpy buffer (parse_buffer event (function r -> ()));
      event
    
    let sendRequestAndGetReply dpy buffer wrapper =
      let event = Jeton.create ()
      in
      send_with_wrapper dpy buffer (parse_buffer event wrapper);
      event

(* Requests *)
    
    let shapeQueryVersion dpy = 
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy (shapeQueryVersionReq shape_info)
      shapeQueryVersionRep
    
    let shapeRectangles dpy dest_win dest_kind xoff yoff rects op order =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy 
        (shapeRectanglesReq shape_info dest_win dest_kind op 
          xoff yoff rects order)
    
    let shapeMask dpy dest_win dest_kind xoff yoff src_pix op =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy
        (shapeMaskReq  shape_info dest_win dest_kind
          op xoff yoff src_pix)
    
    let shapeCombine dpy dest destKind xOff yOff src srcKind op =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy
        (shapeCombineReq shape_info  dest destKind op xOff yOff
          src srcKind)
    
    let shapeQueryExtents dpy win = 
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy
        (shapeQueryExtentsReq shape_info  win) shapeQueryExtentsRep
    
    let shapeSelectInput dpy win enable =
      let shape_info = get_extension dpy ext_name in
      sendRequest dpy (shapeSelectInputReq shape_info win enable)
    
    let shapeInputSelected dpy win =
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy
        (shapeInputSelectedReq shape_info win)
      shapeInputSelectedRep
    
    let shapeGetRectangles dpy win shape_kind =
      let shape_info = get_extension dpy ext_name in
      sendRequestAndGetReply dpy
        (shapeGetRectanglesReq shape_info win shape_kind)
      shapeGetRectanglesRep
  
  
  end
  
open XshapeNotify
  
let to_shapeEvent dpy b =
  let shape_info = get_extension dpy ext_name in
  if Char.code b.[0] <> shape_info.first_event then raise Not_found;
  {
    shape_kind = getEnum8 b 1;
    window = getWindow b 4;
    extents = 
    {
      xx = getInt16 b 8;
      yy = getInt16 b 10;
      dx = getCard16 b 12;
      dy = getCard16 b 14;
      };
    time = getTime b 16;
    shaped = getEnum8 b 20
  }
  
let from_shapeEvent dpy e =
  let shape_info = get_extension dpy ext_name in
  let b = newString 8 in
  setEnum8 b 0 shape_info.first_event;
  setEnum8 b 1 e.shape_kind;
  setWindow b 4 e.window;
  setInt16 b 8 e.extents.xx;
  setInt16 b 10 e.extents.yy;
  setCard16 b 12 e.extents.dx;
  setCard16 b 14 e.extents.dy;
  setTime b 16 e.time;
  setEnum8 b 20 e.shaped;
  b
  