(***********************************************************************)
(*                                                                     *)
(*                            AsmOpt                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Generic part: can be used anywhere *)


open Args
open Asm

type matcher_type = Alias | Var
  
external compare: 'a -> 'a -> bool = "matcher_compare" "noalloc"
external make_matcher: 'a ref -> matcher_type -> 'a = "matcher_make"
external copy: 'a -> 'a = "matcher_copy"
  
let mkvar v = make_matcher v Var
let mkalias v = make_matcher v Alias

(* Specific part for asmopt *)
  
let registers = Hashtbl.create 31
let constants = Hashtbl.create 31
let states = Hashtbl.create 31
let opcodes = Hashtbl.create 31
let args = Hashtbl.create 31
let count = ref 0

let regs_list = ref []
let consts_list = ref []
let states_list = ref []
let opcodes_list = ref []
let args_list = ref []
  
let clear _ =
  let match_vars = {
      match_regs = !regs_list;
      match_consts = !consts_list;
      match_states = !states_list;
      match_opcodes = !opcodes_list;
      match_args = !args_list;
    } in
  Hashtbl.clear registers;
  Hashtbl.clear constants;
  Hashtbl.clear states;
  Hashtbl.clear opcodes;
  Hashtbl.clear args;
  regs_list := [];
  consts_list := [];
  states_list :=  [];
  opcodes_list := [];
  args_list :=  [];
  count := 0;
  match_vars
  
let mkreg (r: string) =
  try
    Hashtbl.find registers r
  with Not_found -> 
      incr count;
      let rr = ref Eax in
      regs_list := rr :: !regs_list;
      let mr = mkvar rr in
      Hashtbl.add registers r mr;
      mr
  
let mkconst (c: string) =
  try
    Hashtbl.find constants c
  with Not_found ->
      incr count;
      let rc = ref (Const_int 0) in
      consts_list := rc :: !consts_list;
      let mc = mkvar rc in
      Hashtbl.add constants c mc;
      mc
  
let mkstate (a: string) =
  try
    Hashtbl.find states a
  with Not_found ->
      incr count;
      let ra = ref Asm.noapprox in
      states_list := ra :: !states_list;
      let ma = mkvar ra in
      Hashtbl.add states a ma;
      ma
  
let mkopcode (o: string) =
  try
    Hashtbl.find opcodes o
  with Not_found ->
      incr count;
      let ro = ref Incl in
      opcodes_list := ro :: !opcodes_list;
      let mo = mkvar ro in
      Hashtbl.add opcodes o mo;
      mo
  
let  mkarg (v: string) =
  try
    Hashtbl.find args v
  with Not_found ->
      incr count;
      let rv = ref (Register (St 0)) in
      args_list := rv :: !args_list;
      let mv = mkvar rv in
      Hashtbl.add args v mv;
      mv
      
