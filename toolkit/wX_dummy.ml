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

open WX_types
  
class t parent attributes =
  object(self)
  
  inherit WX_object.t parent attributes as super
  
  method realize = ()
  method show = ()
  method name = "dummy"
end

let contained parent = (new t parent [])#contained
let container parent = (new t parent [])#container
