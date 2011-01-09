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

(* Without sharing *)
type 'a node = {
    transitions : 'a node option array;
    mutable stop : 'a option;
  }

type 'a t = 'a node
  
let create () =
  { transitions = Array.create 96 None; (* 128 - 32 *)
    stop = None }

let query_code = Char.code '?' - 32 
let star_code = Char.code '*' - 32
  
let find t w =
  let len = String.length w in
  let rec iter t n star =
    try if n = len then match t.stop with
          None -> raise Not_found | Some v -> v else
      let c = Char.code w.[n] - 32 in
      try
        match t.transitions.(c) with None -> raise Not_found
        | Some t -> iter t (n+1) false
      with Not_found ->
          match t.transitions.(query_code) with None -> raise Not_found
          | Some t -> iter t (n+1) false
    with Not_found ->
        match t.transitions.(star_code) with None -> 
            if star then iter t (n+1) true else raise Not_found
        | Some t -> iter t n true
  in
  iter t 0 false
  
let add t w v =
  let len = String.length w in
  let rec iter t n =
    if n = len then t.stop <- Some v else
    let t = 
      let c = Char.code w.[n] - 32 in
      match t.transitions.(c) with Some t -> t | 
        None -> let node = create () in t.transitions.(c) <- Some node; node
    in
    iter t (n+1)
  in
  iter t 0
