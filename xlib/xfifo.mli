(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
exception Empty
type 'a t =
  { mutable empty: bool;
    mutable inpos: int;
    mutable outpos: int;
    mutable array: 'a array;
    mutable size: int }
val create : 'a -> 'a t
val realloc : 'a t -> unit
val put : 'a t -> 'a -> unit
val take : 'a t -> 'a
val clear : 'a t -> unit
val read : 'a t -> 'a
val empty : 'a t -> bool
val to_list : 'a t -> 'a list
val length : 'a t -> int
val put_back_ele : 'a t -> 'a -> unit
val put_back : 'a t -> 'a list -> unit
