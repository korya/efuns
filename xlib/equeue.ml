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



type 'a t = 
  { mutable lookahead : 'a Xfifo.t;
    mutable queue : 'a Xfifo.t
  }

let create init =
  {
    lookahead = Xfifo.create init;
    queue = Xfifo.create init
  }
  
let put t e = Xfifo.put t.queue e
let take t =
  try
    Xfifo.take t.lookahead 
  with
    Xfifo.Empty -> Xfifo.take t.queue
      
let clear t =
  Xfifo.clear t.lookahead;
  Xfifo.clear t.queue
  
let read t = Xfifo.read t.queue
let peek t = Xfifo.take t.queue
let put_back t e = Xfifo.put t.lookahead e
  
let empty_take t = (Xfifo.empty t.lookahead) && (Xfifo.empty t.queue)
let empty_read t = Xfifo.empty t.queue
  
let to_list t =
  (Xfifo.to_list t.lookahead) @ (Xfifo.to_list t.queue)
  
let rec check t predicat =
  let e = Xfifo.take t.queue in
  if
    try
      predicat e
    with
      e -> Xfifo.put t.lookahead e; raise e
  then e 
  else
    begin
      Xfifo.put t.lookahead e;
      check t predicat
    end
    
let clean t = 
  Xfifo.put_back t.queue (Xfifo.to_list t.lookahead);
  Xfifo.clear t.lookahead

let clear_lookahead t = Xfifo.clear t.lookahead