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
type 'a value = | NoValue | Value of 'a | Exc of exn
and 'a t =
  { mutex: Concur.Mutex.t;
    condition: Concur.Condition.t;
    mutable contenu: 'a value }
val create : unit -> 'a t
val signal_value : 'a t -> 'a -> unit
val signal_exception : 'a t -> exn -> unit
val wait : 'a t -> 'a
val check : 'a t -> bool
