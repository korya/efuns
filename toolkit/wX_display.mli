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
class from :
  Xtypes.display ->
  object
    val display : Xtypes.display
    val eloop : Eloop.display
    method broken : (unit -> unit) -> unit
    method close : unit
    method display : Xtypes.display
    method eloop : Eloop.display
  end
class t : string -> from
