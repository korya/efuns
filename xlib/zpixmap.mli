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

type t =
  { image: string;
    raw_image: string;
    dx: Xtypes.size;
    dy: Xtypes.size;
    depth: int;
    bits_per_pixel: int;
    bytes_per_line: int;
    bytes_order: int array;
    bytes_per_unit: int;
    translate_bits: (char -> char);
    setPixel: (t -> Xtypes.coord -> Xtypes.coord -> Xtypes.pixel -> unit);
    getPixel: (t -> Xtypes.coord -> Xtypes.coord -> Xtypes.pixel);
    mutable modified: bool }
val create : display -> Xtypes.size -> Xtypes.size -> int -> t
val getImage :
  display ->
Xtypes.window ->
Xtypes.coord -> Xtypes.coord -> Xtypes.size -> Xtypes.size -> int -> t
val putImage :
  display ->
Xtypes.window -> Xtypes.gc -> Xtypes.coord -> Xtypes.coord -> t -> unit
val setPixel : t -> Xtypes.coord -> Xtypes.coord -> Xtypes.pixel -> unit
val getPixel : t -> Xtypes.coord -> Xtypes.coord -> Xtypes.pixel
