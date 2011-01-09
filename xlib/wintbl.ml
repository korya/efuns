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

(* $Id: wintbl.ml,v 1.1 1999/09/20 15:12:21 lefessan Exp $ *)

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

open Xtypes

type 'b t =
  { mutable max_len: int;                     (* max length of a bucket *)
    mutable data: 'b bucketlist array } (* the buckets *)

and 'b bucketlist =
    Empty
  | Cons of Xtypes.window * 'b * 'b bucketlist

let create initial_size =
  if initial_size <= 0 then invalid_arg "Hashtbl.create" else
  { max_len = 3; data = Array.create initial_size Empty }

let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty
  done

let resize tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = 2 * osize + 1 in
  let ndata = Array.create nsize Empty in
  let rec insert_bucket = function
      Empty -> ()
    | Cons(key, data, rest) ->
        insert_bucket rest; (* preserve original order of elements *)
        let nidx = (window_to_id key) mod nsize in
        ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
  for i = 0 to osize - 1 do
    insert_bucket odata.(i)
  done;
  tbl.data <- ndata;
  tbl.max_len <- 2 * tbl.max_len
          
let rec bucket_too_long n bucket =
  if n < 0 then true else
    match bucket with
      Empty -> false
    | Cons(_,_,rest) -> bucket_too_long (n - 1) rest

let add h key info =
  let i = (window_to_id key) mod (Array.length h.data) in
  let bucket = Cons(key, info, h.data.(i)) in
  h.data.(i) <- bucket;
  if bucket_too_long h.max_len bucket then resize h

let remove h key =
  let rec remove_bucket = function
      Empty ->
        Empty
    | Cons(k, i, next) ->
        if k == key then next else Cons(k, i, remove_bucket next) in
  let i = (window_to_id key) mod (Array.length h.data) in
  h.data.(i) <- remove_bucket h.data.(i)

let find h key =
  match h.data.((window_to_id key) mod (Array.length h.data)) with
    Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
      if key == k1 then d1 else
      match rest1 with
        Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
          if key == k2 then d2 else
          match rest2 with
            Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
              if key == k3 then d3 else begin
                let rec find = function
                    Empty ->
                      raise Not_found
                  | Cons(k, d, rest) ->
                      if key == k then d else find rest
                in find rest3
              end

let find_all h key =
  let rec find_in_bucket = function
    Empty ->
      []
  | Cons(k, d, rest) ->
      if k == key then d :: find_in_bucket rest else find_in_bucket rest in
  find_in_bucket h.data.((window_to_id key) mod (Array.length h.data))

let mem h key =
  let rec mem_in_bucket = function
  | Empty ->
      false
  | Cons(k, d, rest) ->
      k = key || mem_in_bucket rest in
  mem_in_bucket h.data.((window_to_id key) mod (Array.length h.data))

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done
