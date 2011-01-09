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

open Xbuffer
open Xtypes
open Conv_event
open Ll_trans
module Mutex = Concur.Mutex
  
let xopcode opcode = xopcodes.(Obj.magic (opcode : requestOpcode))
let xerror error = xerrors.(Obj.magic (error : errorCode))

let print_xerror error =
  Printf.printf "Warning: Uncaught X Error: %s for request %s\n" (xerror error.err_code) (xopcode error.err_request_code);
  Printf.printf "    Resource: %d  Minor code: %d" error.err_resourceid error.err_minor_code;
  print_newline ()

let xerror_to_string error =
  Printf.sprintf "X Error: %s for request %s\n    Resource: %d  Minor code: %d"  (xerror error.err_code) (xopcode error.err_request_code) error.err_resourceid error.err_minor_code


  (************************************************************
  
  The compatibility with threads is expensive. A special version may
  be useful when there is no threads.
  
  *)
let max_len = 250
let buffer = String.create (max_len*4)
let cur_len = ref 0
  
let newBuffer len = 
  Concur.Mutex.lock xlib_mutex;
  cur_len := len;
  if len > max_len then String.create (len*4) else buffer
  
(**************************************************************

           Le thread de lecture 

***************************************************************)

let read_buffer = String.create (max_len*4)
let readWholeReply socket =
  sock_io_read socket 0 read_buffer 32;
  if read_buffer.[0]='\001' then
    let len = getCard32 read_buffer 4 
    in
    if len=0 then
      read_buffer (* la reponse attendue *)
    else (* il faut lire le reste *)
    let b2 =
      if len > max_len then 
        let b2 = newString (len+8) in
        string_blit read_buffer 0 b2 0 32;
        b2
      else read_buffer in
    sock_io_read socket 32 b2 (len lsl 2);
    b2
  else
    read_buffer

let rec iter_fifo display request list =
  match list with
    [] -> 
(*      Log.printf "SERIAL: not in the queue %d!\n" request; *)
      raise Xfifo.Empty (* not in the queue.. *)
  | (serial, wrapper):: tail ->
      if serial = request then begin
(*          Log.printf "SERIAL: in the queue %d!\n" serial; *)
              (* OK. The request is in the queue. We simply have to remove 
          all preceeding elements. *)
          let rec remove () =
            let (serial,wrapper) = Xfifo.read display.wrappers_queue in
            if serial <> request then 
              let (serial, wrapper) = Xfifo.take display.wrappers_queue
              in                       
              (try
(*                  Log.printf "%s" "O"; *)
                  wrapper " " (* pas de reponse *)
                with
                  _ -> ());
              remove ()
          in
          remove ()
        end else 
        iter_fifo display request tail        

let find_wrapper display request =
  let (serial,wrapper) = Xfifo.read display.wrappers_queue
  in
  if serial <> request then begin
(*      Log.printf "\nSERIAL: request received: %d\n" request;
      Log.printf "SERIAL: waiting: %d\n" serial; *)
      let list = Xfifo.to_list display.wrappers_queue in
      iter_fifo display request list
    end
    
let readReply display =
  let b = readWholeReply display.socket
  in
  match b.[0] with
    '\000' (* une erreur *)
  | '\001' -> (* une reponse *)
      let request = getCard16 b 2 
      in
      Concur.Mutex.lock xlib_mutex;
      (try
          find_wrapper display request;
          let (serial, wrapper) = Xfifo.take display.wrappers_queue in 
(*          Log.printf "\nOUT[%d]\n" serial; *)
          wrapper b;
        with
          Xfifo.Empty when b.[0] = '\000' -> (* Warning for error *)
(*            Log.printf "%s\n" "ERREUR"; *)
            let error = getXError b in
            print_xerror error
        | _ -> (* discard exception *) ());
      Mutex.unlock xlib_mutex
  | _ -> (* un event *)
      Mutex.lock xlib_mutex;
      display.serial_in <- display.serial_in + 1;
      Equeue.put display.event_queue
        (convertCore2Event b display.serial_in);
      Concur.Condition.signal xlib_wait; (* signal for nextEvent *)
      Mutex.unlock xlib_mutex
  
	  
(*************************************************************

     Les fonctions pour les autres threads

**************************************************************)
      
let send_alone display buf =
  let serial = (display.serial_out + 1) land 65535 in
  display.serial_out <- serial;
(*  Log.printf "IN[%d]\n" serial; *)
  sock_io_write display.socket 0 buf (!cur_len lsl 2);
  Mutex.unlock xlib_mutex;
  serial

let send_with_wrapper display buf wrapper =
  let serial =  (display.serial_out + 1) land 65535 in
  display.serial_out <- serial;
  Xfifo.put display.wrappers_queue (serial,wrapper);
(*  Log.printf "INR[%d]\n" serial; *)
  sock_io_write display.socket 0 buf (!cur_len lsl 2);
  Mutex.unlock xlib_mutex

(************************************************************

        La connection avec le serveur X

*************************************************************)

let closeDisplay display = Unix.close display.socket

let openDisplay name =
  let name =
    if name = "" then 
      try
        Sys.getenv "DISPLAY" 
      with
        Not_found -> ":0"
    else name 
  in
  let (sock,server_name,screen_num,auth) = openConnectionWithDisplay name
  in
  let (xauth_name,xauth_data) = auth in
  let len = 3 +(strLen (String.length xauth_name))+
      (strLen (String.length xauth_data)) in
  let b = newString len  in
  setCard8 b 0 0x6C; (* pour PC *)
  setCard16 b 2 11; (* protocol major *)
  setCard16 b 4 0;  (* protocol minor *)
  setCard16 b 6 (String.length xauth_name);  (* auth protocol *)
  setCard16 b 8 (String.length xauth_data);  (* auth data *)
   (* 10:16 unused *)
  if xauth_name<>"" then setString b 12 xauth_name;
  if xauth_data<>"" then 
    setString b (12+4*(strLen (String.length xauth_name))) xauth_data;
  sock_io_write sock 0 b (len lsl 2);
  sock_io_read sock 0 b 8;
  let len = getCard16 b 6 in
  let add = newString (len+1) in
  sock_io_read sock 0 add (len lsl 2);
  if b.[0]='\000' then
    failwith 
      (Printf.sprintf"Connection refused by %s : %s" server_name (getString add 0 (getCard8 b 1)))
  else
  if b.[0]='\002' then
    failwith ("authentification required:"^add)
  else
  if b.[0] = '\001' then
    begin
      let (maj,min)=(getCard16 b 2,getCard16 b 4)
      in
      let b = add
      in
      
      let display  =
        getXServerInfo b sock maj min screen_num server_name
      in
      Concur.Thread.add_reader display.socket
        (function () -> 
            try
              readReply display
            with
              Ll_trans.BrokenConnection ->
                display.dpy_broken ();
                Concur.Thread.remove_reader display.socket);
      display
    end
  else
    failwith "Reponse incoherente"

let alloc_id display =
  let incr = ref (display.resource_incr)
  in
  let id = ref (display.last_resource_id+display.resource_incr)
  in 
  while (!id land display.resource_mask) <= (display.last_resource_id) do
    incr := (!id lor display.resource_mask) lxor display.resource_mask;
    id := (!id)+(!incr)
  done;
  id := (!id lor (!incr - 1)) lxor (!incr - 1);
  display.last_resource_id <- !id;
  !id lor display.resource_base

let newRequest (op : requestOpcode) len =
  let b = newBuffer len
  in
(*  Log.printf "<%d>" (Obj.magic op); *)
  setEnum8 b 0 op;
  setCard16 b 2 len;
  b

let newWinRequest (op : requestOpcode) len win =
  let b = newBuffer len
  in
(*  Log.printf "<%d>" (Obj.magic op); *)
  setEnum8 b 0 op;
  setCard16 b 2 len;
  setEnum32 b 4 win;
  b

let emptyRequest mask =
  newRequest mask 1

let simpleRequest mask data =
  let b = newRequest mask 2
  in
  setEnum32 b 4 data;
  b

let doubleRequest mask data1 data2 =
  let b = newRequest mask 3
  in
  setEnum32 b 4 data1;
  setEnum32 b 8 data2;
  b

let littleRequest opcode data =
  let b = emptyRequest opcode
  in
  setEnum8 b 1 data;
  b

let parse_buffer event wrapper buffer =
(*  Log.printf "?%s" "SIGNAL"; *)
  try
    if buffer.[0]='\000' then
      Jeton.signal_exception event (XError (
          let b = getXError buffer in
          if !Xdebug.debug_flag then
            print_xerror b;
          b))
    else
      Jeton.signal_value event (wrapper buffer)
  with
    e -> Jeton.signal_exception event e
      
      
      
   