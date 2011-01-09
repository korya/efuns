(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: msetint.ml,v 1.1 1999/11/29 14:49:24 lefessan Exp $ *)

(*****
This implementation uses bit-vectors. It is an implementation of mutable
sets of integers.
****)

(* Sets over ordered types *)

type t = int array ref

let empty () = ref [||]

let is_empty t =
  let t = !t in
  try
    for i = 0 to Array.length t - 1 do
      if t.(i) = 0 then raise Exit 
    done;
    true
  with Exit -> false

let (>>) a b = a lsr b
let (<<) a b = a lsl b
      
let mem e t =
  let t = !t in
  let pos = e >> 4 in
  (Array.length t > pos) && (t.(pos) land (1 << (e land 0xf)) <> 0)

let add e t =
  let pos = e >> 4 in
  let len = Array.length !t in
  if  len > pos then
    let t = !t in
    t.(pos) <- t.(pos) lor (1 << (e land 0xf))
  else 
  let t2 = Array.create (2*pos+1) 0 in
  Array.blit !t 0 t2 0 len;
  t2.(pos) <- t2.(pos) lor (1 << (e land 0xf));
  t := t2
  
let singleton e = let t = empty () in add e t; t

let remove e t =
  let t = !t in
  let pos = e >> 4 in
  let len = Array.length t in
  if  len > pos then
    t.(pos) <- t.(pos) land lnot (1 << (e land 0xf))
    
let union t1 t2 =
  let res = t1 in
  let t1 = !t1 in
  let t2 = !t2 in
  let len1 = Array.length t1 in
  let len2 = Array.length t2 in
  if len1 > len2 then
    for i = 0 to len2 - 1 do
      t1.(i) <- t1.(i) lor t2.(i)
    done
  else
  let t = Array.copy t2 in
  for i = 0 to len1 - 1 do
    t.(i) <- t.(i) lor t1.(i)
  done;
  res := t
  
let inter t1 t2 =
  let res = t1 in
  let t1 = !t1 in
  let t2 = !t2 in
  let len1 = Array.length t1 in
  let len2 = Array.length t2 in
  if len1 < len2 then
    for i = 0 to len1 - 1 do
      t1.(i) <- t1.(i) land t2.(i)
    done
  else
  let t = Array.copy t2 in
  for i = 0 to len2 - 1 do
    t.(i) <- t.(i) land t1.(i)
  done;
  res := t
  
let diff t1 t2 =
  let res = t1 in
  let t1 = !t1 in
  let t2 = !t2 in
  let len1 = Array.length t1 in
  let len2 = Array.length t2 in
  for i = 0 to min len1 len2 - 1 do
    t1.(i) <- 0xffff land (t1.(i) land lnot t2.(i))
  done
  
let copy t = ref (Array.copy !t)
  
exception ExitInt of int

let min (n1:int) (n2: int) = if n1 > n2 then n2 else n1
  
let compare t1 t2 = 
  let t1 = !t1 in let t2 = !t2 in
  let len1 = Array.length t1 in
  let len2 = Array.length t2 in
  try
    for i = 0 to min len1 len2 - 1 do
      if t1.(i) < t2.(i) then raise (ExitInt (-1))  else
      if t1.(i) > t2.(i) then raise (ExitInt 1)
    done;
    if len1>len2 then
      for i = len2 to len1-1 do
        if t1.(i) <> 0 then raise (ExitInt 1)
      done
    else
    if len2 > len1 then
      for i = len1 to len2-1 do
        if t2.(i) <> 0 then raise (ExitInt (-1))
      done;
    0
  with ExitInt n -> n

let equal t1 t2 = compare t1 t2 = 0
let subset t1 t2 = let t3 = copy t1 in inter t3 t2;  equal t1 t3
let iter f t =
  let t = !t in
  for i = 0 to Array.length t - 1 do
    if t.(i) <> 0 then
      for j = 0 to 15 do
        if t.(i) land (1 << j) <> 0 then
          f ((i << 4) + j)
      done
  done
  
let fold f a t =
  let accu = ref a in
  iter (fun x -> accu:= f x (!accu)) t ;
  !accu
  
let cardinal t =
  let t = !t in
  let n = ref 0 in
  for i = 0 to Array.length t - 1 do
    if t.(i) <> 0 then
      for j = 0 to 15 do
      if t.(i) land (1 << j) <> 0 then incr n
    done
  done;
  !n

let elements t = 
  let t = !t in
  let list = ref [] in
  for i = 0 to Array.length t - 1 do
    let v = t.(i) in
    if v <> 0 then
      for j = 0 to 15 do
        if v land (1 << j) <> 0 then
          list := ((i << 4) + j) :: !list
      done
  done;
  !list

let min_elt t =
  let t = !t in
  try
    for i = 0 to Array.length t - 1 do
      let v = t.(i) in
      if v <> 0 then
        for j = 0 to 15 do
          if v land (1 << j) <> 0 then raise (ExitInt ((i << 4) + j))
        done
    done;
    raise Not_found
  with ExitInt n -> n

let max_elt t =
  let t = !t in
  try
    let len = Array.length t in
    for i = 1 to len do
      let v = t.(len-i) in
      if v <> 0 then
        for j = 0 to 15 do
          if v land (1 << j) <> 0 then raise (ExitInt  (((len-i) << 4) + j))
        done
    done;
    raise Not_found
  with ExitInt n -> n

let choose = min_elt
