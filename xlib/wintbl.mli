(*
This file is a partial copy of hashtbl.ml. You think I should probably use
the functor, but I hope the compiler will more optimize this version
  than the functor one.
*)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id: wintbl.mli,v 1.1 1999/09/20 15:12:29 lefessan Exp $ *)

(* Module [Hashtbl]: hash tables and hash functions *)

(* Hash tables are hashed association tables, with in-place modification. *)

(*** Generic interface *)

type 'b t
        (* The type of hash tables from type [Xtypes.window] to type ['b]. *)

val create : int -> 'b t
        (* [Hashtbl.create n] creates a new, empty hash table, with
           initial size [n].  The table grows as needed, so [n] is
           just an initial guess.  Better results are said to be
           achieved when [n] is a prime number.
           Raise [Invalid_argument] if [n] is less than 1. *)

val clear : 'b t -> unit
        (* Empty a hash table. *)

val add : 'b t -> Xtypes.window -> 'b -> unit
        (* [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
           Previous bindings for [x] are not removed, but simply
           hidden. That is, after performing [Hashtbl.remove tbl x],
           the previous binding for [x], if any, is restored.
           (Same behavior as with association lists.) *)

val find : 'b t -> Xtypes.window -> 'b
        (* [Hashtbl.find tbl x] returns the current binding of [x] in [tbl],
           or raises [Not_found] if no such binding exists. *)

val find_all : 'b t -> Xtypes.window -> 'b list
        (* [Hashtbl.find_all tbl x] returns the list of all data
           associated with [x] in [tbl].
           The current binding is returned first, then the previous
           bindings, in reverse order of introduction in the table. *)

val mem :  'b t -> Xtypes.window -> bool
        (* [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)

val remove : 'b t -> Xtypes.window -> unit
        (* [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
           restoring the previous binding if it exists.
           It does nothing if [x] is not bound in [tbl]. *)

val iter : (Xtypes.window -> 'b -> unit) -> 'b t -> unit
        (* [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
           [f] receives the key as first argument, and the associated value
           as second argument. The order in which the bindings are passed to
           [f] is unspecified. Each binding is presented exactly once
           to [f]. *)
