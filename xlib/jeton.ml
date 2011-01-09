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

open Concur

type 'a value =
    NoValue
  | Value of 'a
  | Exc of exn

type 'a t =
    {
      mutex: Mutex.t;
      condition: Condition.t;
      mutable contenu: 'a value
    }

let create () =
  { mutex = Mutex.create ();
    condition = Condition.create ();
    contenu = NoValue
  }

let signal_value event v =
  Mutex.lock event.mutex;
  event.contenu <- Value v;
  Condition.signal event.condition;
  Mutex.unlock event.mutex


let signal_exception event e =
  Mutex.lock event.mutex;
  event.contenu <- Exc e;
  Condition.signal event.condition;
  Mutex.unlock event.mutex

let wait event =
  Mutex.lock event.mutex;
(*  Log.printf "<J>%s" ""; *)
  while event.contenu = NoValue do
    Condition.wait event.condition event.mutex;
  done;
  let contenu = event.contenu  in
(*  Log.printf "</J>%s" ""; *)
  event.contenu <- NoValue;
  Mutex.unlock event.mutex;
  match contenu with
    NoValue -> assert false
  | Value v -> 
      v
  | Exc e -> raise e
      
let check event =  event.contenu = NoValue 
