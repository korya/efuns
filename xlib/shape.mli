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

val install_extension : Xtypes.display -> string -> Xtypes.extension -> unit
val get_extension : Xtypes.display -> string -> Xtypes.extension

val ext_name : string
type shapeKind = | Bounding | Clip
type shapeOp = | Set | Union | Intersect | Substract | Invert

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

module XshapeNotify :
  sig
    type t =
      { shape_kind: shapeKind;
        window: Xtypes.window;
        extents: Xtypes.rect;
        time: Xtypes.time;
        shaped: bool }
  end
val queryExtension : Xtypes.display -> bool
module X :
  sig
    val shapeQueryVersion : Xtypes.display -> shapeQueryVersionRep
    val shapeRectangles :
      Xtypes.display -> window -> shapeKind -> coord -> coord -> 
    (coord * coord * size * size) list -> shapeOp -> clipOrder -> unit
    val shapeMask :
      Xtypes.display -> window -> shapeKind -> coord -> coord -> pixmap -> 
    shapeOp -> unit
    val shapeCombine :
      Xtypes.display -> window -> shapeKind -> coord -> coord -> window -> 
    shapeKind -> shapeOp -> unit
    val shapeQueryExtents : Xtypes.display -> window -> shapeQueryExtentsRep
    val shapeSelectInput : Xtypes.display -> window -> bool -> unit
    val shapeInputSelected : Xtypes.display -> window -> bool
    val shapeGetRectangles :
      Xtypes.display -> window -> shapeKind -> shapeGetRectanglesRep
  end
module Xsync :
  sig
    val shapeQueryVersion : Xtypes.display -> shapeQueryVersionRep
    val shapeRectangles :
      Xtypes.display -> window -> shapeKind -> coord -> coord -> 
    (coord * coord * size * size) list -> shapeOp -> clipOrder -> unit
    val shapeMask :
      Xtypes.display -> window -> shapeKind -> coord -> coord -> pixmap -> 
    shapeOp -> unit
    val shapeCombine :
      Xtypes.display -> window -> shapeKind -> coord -> coord -> window -> 
    shapeKind -> shapeOp -> unit
    val shapeQueryExtents : Xtypes.display -> window -> shapeQueryExtentsRep
    val shapeSelectInput : Xtypes.display -> window -> bool -> unit
    val shapeInputSelected : Xtypes.display -> window -> bool
    val shapeGetRectangles :
      Xtypes.display -> window -> shapeKind -> shapeGetRectanglesRep
  end
module Xasync :
  sig
    val shapeQueryVersion : Xtypes.display -> shapeQueryVersionRep Jeton.t
    val shapeRectangles :
      Xtypes.display -> window -> shapeKind -> coord -> coord ->
    (coord * coord * size * size) list -> shapeOp -> clipOrder -> unit Jeton.t
    val shapeMask :
      Xtypes.display -> window -> shapeKind -> coord -> coord -> pixmap -> 
    shapeOp -> unit Jeton.t
    val shapeCombine :
      Xtypes.display -> window -> shapeKind -> coord -> coord -> window -> 
    shapeKind -> shapeOp -> unit Jeton.t
    val shapeQueryExtents :
      Xtypes.display -> window -> shapeQueryExtentsRep Jeton.t
    val shapeSelectInput : Xtypes.display -> window -> bool -> unit Jeton.t
    val shapeInputSelected : Xtypes.display -> window -> bool Jeton.t
    val shapeGetRectangles :
      Xtypes.display -> window -> shapeKind -> shapeGetRectanglesRep Jeton.t
  end
  
val to_shapeEvent : display -> string -> XshapeNotify.t
val from_shapeEvent : display -> XshapeNotify.t -> string  