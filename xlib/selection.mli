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

val setSelection :
  Xtypes.display ->
  Xtypes.window ->
  Xtypes.atom -> (Xtypes.atom -> int * string) -> Xtypes.time -> unit
val removeSelection :
  Xtypes.display -> Xtypes.atom -> Xtypes.time -> unit
val getSelection :
  Xtypes.display ->
  Xtypes.window ->
  Xtypes.atom -> Xtypes.atom -> Xtypes.time -> string

val handleSelectionRequest : Xtypes.display -> Xtypes.Xselectionrequest.t -> unit
val handleSelectionClear : 'a -> Xtypes.Xselectionclear.t -> unit
