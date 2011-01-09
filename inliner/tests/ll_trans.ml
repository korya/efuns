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
open Xtypes
open Xauth

(*
 
    Low-Level Transmission with X-server

*)
open ThreadUnix

exception BrokenConnection
  
let e = BrokenConnection
  
let rec sock_io_read_it sock debut b len =
  let nsent = read sock b debut len in
  if nsent=0 then raise e;
  let left = len - nsent in
  if left > 0 then sock_io_read_it sock (debut+nsent) b left
  
let sock_io_read sock debut b len =
  try
    sock_io_read_it sock debut b len
  with Unix.Unix_error _ -> raise e

let rec sock_io_write_it sock debut b len =
  let nsent = write sock b debut len in
  if nsent=0 then raise e;
  let left = len - nsent in
  if left > 0 then sock_io_write_it sock (debut+nsent) b left
  
let sock_io_write sock debut b len =
  try
    sock_io_write_it sock debut b len
  with Unix.Unix_error _ -> raise e

      
exception NotADigit of int

let discardInt name curs =
  try
    for i=curs downto 0 do
      let c=name.[i]
      in
      if(c>'9') || (c<'0') then
        raise (NotADigit i)
    done;
    (-1,int_of_string (String.sub name 0 (curs+1)))
  with
    NotADigit i ->
      (i,int_of_string (String.sub name (i+1) (curs-i)))


(* retourne: adresse * port * ecran *)
let parseDisplayName name =
  let len = String.length name
  and port = ref 0
  and num = ref 0
  and dname = ref ""
  and semi = ref 0
  in
  try
    let (pos,n1) = discardInt name (len-1)
    in
    if name.[pos]='.' then 
      let (pos,n2) = discardInt name (pos-1)
      in
      port := n2;
      num := n1;
      semi := pos
    else
      (
        port := n1;
        semi := pos
      );
    if(name.[!semi]=':') then
      (String.sub name 0 !semi,!port,!num)
    else
      failwith("Invalid Display Name")
  with
    Failure _ -> failwith("Invalid Display Name")

let local_host = Unix.gethostname ()

let openConnectionWithDisplay name =
  let (server_name,dport,dnum)= parseDisplayName name
  in
  let (domaine,server,auth) =
    if server_name = "" || server_name = local_host then (* Domaine unix *)
      (Unix.PF_UNIX,Unix.ADDR_UNIX ("/tmp/.X11-unix/X"
            ^(string_of_int dport)),
        try
          try
            Xauth.getBestAuth {
              xauth_family = FamilyLocal;
              xauth_address = "";
              xauth_number = string_of_int dnum;
              xauth_name = "";
              xauth_data = ""
            }         
          with
            Not_found ->
              try
                Xauth.getBestAuth {
                  xauth_family = FamilyLocal;
                  xauth_address = local_host;
                  xauth_number = string_of_int dnum;
                  xauth_name = "";
                  xauth_data = ""
                }
              with
                Not_found -> 
                  "", ""
        with
          _ -> "", ""
      )
    else  (* internet *)
    let addr =
      try
        Unix.inet_addr_of_string server_name
      with
        Failure _ -> 
          try
            (Unix.gethostbyname server_name).Unix.h_addr_list.(0)
          with
            Not_found ->
              failwith ("Unknown host"^server_name)
    in
    (Unix.PF_INET,
      (Unix.ADDR_INET 
          (addr, (dport+6000))),
      try
        Xauth.getBestAuth {
          xauth_family = FamilyIP;
          xauth_address = Obj.magic addr;
          xauth_number = string_of_int dnum;
          xauth_name = "";
          xauth_data = ""
        }
      with
        Not_found -> "", ""
    )
  in
  let sock = socket domaine Unix.SOCK_STREAM 0
  in
  Unix.set_close_on_exec sock;
  connect sock server;
  (sock,server_name,dnum,auth)
  
  