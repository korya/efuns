(***********************************************************************)
(*                                                                     *)
(*                           Interp.ml                                 *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(*
   This module implement an interpretor for OCAML bytecode. The bytecode is
supposed to be linked (ie symbols are resolved). Closures are compiled as
native closures taking only one argument each time. For example, GRAB creates
a native closure waiting for "arity" arguments, then putting them on the stack
and calling the interpretor with a new pc.
*)


open Instruct
open Obj

  (* a generic type for the GC, but not a float *)  
type t = { mutable dummy1 : int; mutable dummy2: int option }
external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_block : t -> bool = "obj_is_block"
external tag : t -> int = "obj_tag"
external size : t -> int = "%obj_size"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external new_block : int -> int -> t = "obj_block"
external dup : t -> t = "obj_dup"
external truncate : t -> int -> unit = "obj_truncate"

(* caml_globals: table des donnees globales = 
        exceptions predefines + modules natifs + modules bytecode 
*)
(* table: module_name -> caml_globals_slot *)

(* a BUG appear for more than 10000 globals .... but can I clean this ? *)
let caml_globals = Array.create 10000 (repr 0)
let (globals : ( string, int) Hashtbl.t) = Hashtbl.create 53
let next_free_global = ref 9

let caml_prims = Array.create 500 (repr 0)
let (prims : (string,int) Hashtbl.t) = Hashtbl.create 53
let next_free_prim = ref 0
let prims_call = Array.create 500 ""

external asm_getglobal : int -> Obj.t = "asm_getglobal" 
external asm_getmap : unit -> string = "asm_getmap"

let (globals_map : (string * string) list) = 
  Marshal.from_string (asm_getmap ()) 0

let (first_native_module,last_native_module) = 
  let rec iter i names =
    match names with
      [] -> ()
    | (mod_name,crc) :: tail -> 
        Hashtbl.add globals mod_name !next_free_global;
        incr next_free_global;
        iter (i+1) tail
  in
  let first_native_module = !next_free_global in
  iter 0 globals_map;
  (first_native_module,!next_free_global - 1)

let getglobal i = 
  if i>= first_native_module && i<= last_native_module then
    begin
      let modul = (Obj.magic (asm_getglobal (i - first_native_module))) in
      modul
    end
  else
    repr caml_globals.(i)



let setglobal i v = caml_globals.(i) <- v

let stack = Array.create 10000 (repr 0)
let extern_sp = ref 10000

let value_size = Sys.word_size / 8

(*
  Les closures Bytecode sont des fonctions native prenant ses arguments un par
un. Lorsque tous les arguments d'une closure Bytecode sont disponibles,
la fonction native les empile et lance le Bytecode.
*)
exception ExitInterp
exception PopTrap of int * t

type env = {
    code : int array;
    env : t array;
    recenv : int * t array;
    stack : t array;
  }


let instruct (opcode : int) = (magic opcode : Instruct.instruction)
let unit = repr 0
let closure (accu : t) = (magic accu : t -> 'a)

  open Instruct
let instructs = Array.create opBREAK 0

let rec interp (closr: env) (pc:int) (accu:t) =
  
  let stack = stack in
  
  let code = closr.code in
  let env = closr.env in
  let stack = closr.stack in
  
  let pc = ref (pc - 1) in
  let accu = ref accu in 
  let env = env  in
  let sp = ref !extern_sp in
  
  try
    while true do
      incr pc;
(*
      Printf.printf "PC(%d) %s" !pc Instruct.opnames.(unsigned code.(!pc));
      print_newline ();
*)
      match instruct code.(!pc) with
      | OpACC0 -> (*instructs.(opACC0) <- instructs.(opACC0) + 1;*) accu := stack.(!sp) 
      | OpACC1 -> (*instructs.(opACC1) <- instructs.(opACC1) + 1;*) accu := stack.(!sp + 1)
      | OpACC2 -> (*instructs.(opACC2) <- instructs.(opACC2) + 1;*) accu := stack.(!sp + 2)
      | OpACC3 -> (*instructs.(opACC3) <- instructs.(opACC3) + 1;*) accu := stack.(!sp + 3)
      | OpACC4 -> (*instructs.(opACC4) <- instructs.(opACC4) + 1;*) accu := stack.(!sp + 4)
      | OpACC5 -> (*instructs.(opACC5) <- instructs.(opACC5) + 1;*) accu := stack.(!sp + 5)
      | OpACC6 -> (*instructs.(opACC6) <- instructs.(opACC6) + 1;*) accu := stack.(!sp + 6)
      | OpACC7 -> (*instructs.(opACC7) <- instructs.(opACC7) + 1;*) accu := stack.(!sp + 7)
      | OpACC -> (*instructs.(opACC) <- instructs.(opACC) + 1;*) 
          incr pc; let offset = code.(!pc) in
          accu := stack.(!sp + offset)
      | OpPUSH 
      | OpPUSHACC0 -> (*instructs.(opPUSHACC0) <- instructs.(opPUSHACC0) + 1;*)
          decr sp; stack.(!sp) <- !accu
      | OpPUSHACC1 -> (*instructs.(opPUSHACC1) <- instructs.(opPUSHACC1) + 1;*) decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 1)
      | OpPUSHACC2 -> (*instructs.(opPUSHACC2) <- instructs.(opPUSHACC2) + 1;*) decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 2)
      | OpPUSHACC3 -> (*instructs.(opPUSHACC3) <- instructs.(opPUSHACC3) + 1;*) decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 3)
      | OpPUSHACC4 -> (*instructs.(opPUSHACC4) <- instructs.(opPUSHACC4) + 1;*) decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 4)
      | OpPUSHACC5 -> (*instructs.(opPUSHACC5) <- instructs.(opPUSHACC5) + 1;*) decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 5)
      | OpPUSHACC6 -> (*instructs.(opPUSHACC6) <- instructs.(opPUSHACC6) + 1;*) decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 6)
      | OpPUSHACC7 -> (*instructs.(opPUSHACC7) <- instructs.(opPUSHACC7) + 1;*) decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 7)
      | OpPUSHACC -> (*instructs.(opPUSHACC) <- instructs.(opPUSHACC) + 1;*) decr sp; stack.(!sp) <- !accu; 
          incr pc; let offset = code.(!pc) in
          accu := stack.(!sp + offset)
      | OpPOP -> (*instructs.(opPOP) <- instructs.(opPOP) + 1;*) 
          incr pc; let offset = code.(!pc) in
          sp := !sp + offset
      | OpASSIGN -> (*instructs.(opASSIGN) <- instructs.(opASSIGN) + 1;*)
          incr pc; let offset = code.(!pc) in
          stack.(!sp + offset) <- !accu;
          accu := unit
      | OpENVACC1 -> (*instructs.(opENVACC1) <- instructs.(opENVACC1) + 1;*) accu := env.(1)
      | OpENVACC2 -> (*instructs.(opENVACC2) <- instructs.(opENVACC2) + 1;*) accu := env.(2)
      | OpENVACC3 -> (*instructs.(opENVACC3) <- instructs.(opENVACC3) + 1;*) accu := env.(3)
      | OpENVACC4 -> (*instructs.(opENVACC4) <- instructs.(opENVACC4) + 1;*) accu := env.(4)
      | OpENVACC -> (*instructs.(opENVACC) <- instructs.(opENVACC) + 1;*)
          incr pc; let offset = code.(!pc) in
          accu := env.(offset)
      | OpPUSHENVACC1 -> (*instructs.(opPUSHENVACC1) <- instructs.(opPUSHENVACC1) + 1;*)
          decr sp; stack.(!sp) <- !accu; accu := env.(1)
      | OpPUSHENVACC2 -> (*instructs.(opPUSHENVACC2) <- instructs.(opPUSHENVACC2) + 1;*)
          decr sp; stack.(!sp) <- !accu; accu := env.(2)
      | OpPUSHENVACC3 -> (*instructs.(opPUSHENVACC3) <- instructs.(opPUSHENVACC3) + 1;*)
          decr sp; stack.(!sp) <- !accu; accu := env.(3)
      | OpPUSHENVACC4 -> (*instructs.(opPUSHENVACC4) <- instructs.(opPUSHENVACC4) + 1;*)
          decr sp; stack.(!sp) <- !accu; accu := env.(4)
      | OpPUSHENVACC -> (*instructs.(opPUSHENVACC) <- instructs.(opPUSHENVACC) + 1;*)
          incr pc; let offset = code.(!pc) in
          decr sp; stack.(!sp) <- !accu; accu := env.(offset)
      | OpPUSH_RETADDR -> (*instructs.(opPUSH_RETADDR) <- instructs.(opPUSH_RETADDR) + 1;*)
          incr pc; let offset = magic code.(!pc) in
          stack.(!sp-1) <- repr (!pc + offset - 1);
          sp := !sp - 2
      | OpAPPLY -> (*instructs.(opAPPLY) <- instructs.(opAPPLY) + 1;*)
          (*          incr pc; let nargs = code.(!pc) in *)
          let nargs = code.(!pc + 1) in 
          for i = 1 to nargs do
            incr sp;
            extern_sp := !sp + i; (* update extern SP *)
            accu := (closure !accu) stack.(!sp + i - 1);
          done;
          sp := !sp + nargs + 2;
          pc := magic stack.(!sp); incr sp;
      | OpAPPLY1 -> (*instructs.(opAPPLY1) <- instructs.(opAPPLY1) + 1;*)
          let arg = stack.(!sp) in 
          incr sp;
          extern_sp := !sp;
          accu := (closure !accu) arg;
      | OpAPPLY2 -> (*instructs.(opAPPLY2) <- instructs.(opAPPLY2) + 1;*)
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          sp := !sp + 2;
          extern_sp := !sp;
          accu := (closure !accu) arg1 arg2;
      | OpAPPLY3 -> (*instructs.(opAPPLY3) <- instructs.(opAPPLY3) + 1;*)
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          let arg3 = stack.(!sp + 2) in
          sp := !sp + 3;
          extern_sp := !sp;
          accu := (closure !accu) arg1 arg2 arg3;
      | OpAPPTERM -> (*instructs.(opAPPTERM) <- instructs.(opAPPTERM) + 1;*)
          let nargs = code.(!pc+1) in
          let slotsize = code.(!pc+2) in
          let newsp = !sp + slotsize - nargs in
          for i = nargs - 1 downto 0 do
            stack.(newsp + i) <- stack.(!sp + i)
          done;
          sp := newsp;
          for i = 0 to nargs - 1 do
            extern_sp := !sp;
            accu := (closure !accu) stack.(newsp + i);
          done;
          raise ExitInterp
      | OpAPPTERM1 -> (*instructs.(opAPPTERM1) <- instructs.(opAPPTERM1) + 1;*)
          let arg = stack.(!sp) in
          incr pc; let slotsize = code.(!pc) in
          sp := !sp + slotsize;
          let trapsp = !sp in
          
          extern_sp := !sp;
          accu := (closure !accu) arg;
          (* sp := trapsp; *)
          raise ExitInterp
      | OpAPPTERM2 -> (*instructs.(opAPPTERM2) <- instructs.(opAPPTERM2) + 1;*)
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          incr pc; let slotsize = code.(!pc) in
          sp := !sp + slotsize;
          let trapsp = !sp in
          
          extern_sp := !sp;
          accu := (closure !accu) arg1 arg2;
          (* sp := trapsp; *)
          raise ExitInterp
      | OpAPPTERM3 -> (*instructs.(opAPPTERM3) <- instructs.(opAPPTERM3) + 1;*)
          
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          let arg3 = stack.(!sp + 2) in
          incr pc; let slotsize = code.(!pc) in
          sp := !sp + slotsize;
          let trapsp = !sp in
          
          extern_sp := !sp;
          accu := (closure !accu) arg1 arg2 arg3;
          (* sp := trapsp; *)
          raise ExitInterp
      | OpRETURN -> (*instructs.(opRETURN) <- instructs.(opRETURN) + 1;*) 
          (* inutile 
          incr pc; let offset = code.(!pc) in
          sp := !sp + offset; *)
          raise ExitInterp
      | OpRESTART -> (*instructs.(opRESTART) <- instructs.(opRESTART) + 1;*) assert false
      | OpGRAB -> (*instructs.(opGRAB) <- instructs.(opGRAB) + 1;*)
          incr pc; let required = code.(!pc) in
          let goto = !pc + 1 in
          let arg1 = stack.(!sp) in incr sp;
          accu :=
          (match required with
              1 -> repr (closure1 closr goto arg1)
            | 2 -> repr (closure2 closr goto arg1)
            | 3 -> repr (closure3 closr goto arg1)
            | _ ->
                let args = Array.create required unit in
                args.(0) <- arg1;
                let rec f n arg =
                  if n = required then
                    let sp = extern_sp in
                    args.(n) <- arg; 
                    sp := !sp - required - 1;
                    for i = 0 to required do
                      stack.(!sp + i) <- args.(i)
                    done;                  
                    let v = magic (interp closr goto unit) in
                    v
                  else
                    (Obj.magic f : int -> t -> t) (n+1)
                in
                repr (f 1));
          raise ExitInterp
      | OpCLOSURE -> (*instructs.(opCLOSURE) <- instructs.(opCLOSURE) + 1;*)
          
          incr pc; let nvars = code.(!pc) in
          incr pc; let goto = !pc + magic code.(!pc) in
          if nvars > 0 then (decr sp; stack.(!sp) <- !accu);
          let clos = Array.create (1+nvars) unit in
          clos.(0) <- repr goto;
          
          for i = 0 to nvars - 1 do
            clos.(i+1) <- stack.(!sp + i)
          done;
          accu := repr (closure_byte closr clos goto);
          sp := !sp + nvars;
      
      | OpCLOSUREREC -> (*instructs.(opCLOSUREREC) <- instructs.(opCLOSUREREC) + 1;*) 
          incr pc; let nfuncs = code.(!pc) in
          incr pc; let nvars = code.(!pc) in
          if nvars > 0 then (decr sp; stack.(!sp) <- !accu);
          let size = nfuncs * 2 - 1 + nvars in
          let clos = Array.create size unit in
          for i = 0 to nvars - 1 do
            clos.(nfuncs * 2 - 1 + i) <- stack.(!sp + i);
          done;
          sp := !sp + nvars;
          let f i goto arg =
            let sp = extern_sp in
            let trapsp = !sp in
            decr sp; stack.(!sp) <- arg;
            let env = Array.sub clos (i*2) (size - i*2) in
            let clos = { closr with env = env; recenv = (i*2,clos) } in
            let v = interp clos goto unit in
            sp := trapsp;
            v
          in
          let oldpc = !pc + 1 in
          for i = 0 to nfuncs - 1 do
            incr pc; let goto = oldpc + magic code.(!pc) in
            let fi = repr (f i goto) in
            decr sp; stack.(!sp) <- fi;
            clos.(i*2) <- fi
          done
      | OpOFFSETCLOSUREM2 -> (*instructs.(opOFFSETCLOSUREM2) <- instructs.(opOFFSETCLOSUREM2) + 1;*)
          let (pos,env) = closr.recenv in
          accu := env.(pos-2)
      | OpOFFSETCLOSURE0 -> (*instructs.(opOFFSETCLOSURE0) <- instructs.(opOFFSETCLOSURE0) + 1;*)
          let (pos,env) = closr.recenv in
          accu := env.(pos)
      | OpOFFSETCLOSURE2 -> (*instructs.(opOFFSETCLOSURE2) <- instructs.(opOFFSETCLOSURE2) + 1;*)
          let (pos,env) = closr.recenv in
          accu := env.(pos+2)
      | OpOFFSETCLOSURE -> (*instructs.(opOFFSETCLOSURE) <- instructs.(opOFFSETCLOSURE) + 1;*)
          incr pc; let offset = magic code.(!pc) in
          let (pos,env) = closr.recenv in
          accu := env.(pos+offset)
      | OpPUSHOFFSETCLOSUREM2 -> (*instructs.(opPUSHOFFSETCLOSUREM2) <- instructs.(opPUSHOFFSETCLOSUREM2) + 1;*)
          decr sp; stack.(!sp) <- !accu;
          let (pos,env) = closr.recenv in
          accu := env.(pos-2)
      | OpPUSHOFFSETCLOSURE0 -> (*instructs.(opPUSHOFFSETCLOSURE0) <- instructs.(opPUSHOFFSETCLOSURE0) + 1;*)
          decr sp; stack.(!sp) <- !accu;
          let (pos,env) = closr.recenv in
          accu := env.(pos)
      | OpPUSHOFFSETCLOSURE2 -> (*instructs.(opPUSHOFFSETCLOSURE2) <- instructs.(opPUSHOFFSETCLOSURE2) + 1;*)
          decr sp; stack.(!sp) <- !accu;
          let (pos,env) = closr.recenv in
          accu := env.(pos+2)
      | OpPUSHOFFSETCLOSURE -> (*instructs.(opPUSHOFFSETCLOSURE) <- instructs.(opPUSHOFFSETCLOSURE) + 1;*)
          decr sp; stack.(!sp) <- !accu;
          incr pc; let offset = magic code.(!pc) in
          let (pos,env) = closr.recenv in
          accu := env.(pos+offset)
      | OpGETGLOBAL -> (*instructs.(opGETGLOBAL) <- instructs.(opGETGLOBAL) + 1;*)
          
          incr pc; let offset = code.(!pc) in
          accu := getglobal offset;
      
      | OpPUSHGETGLOBAL -> (*instructs.(opPUSHGETGLOBAL) <- instructs.(opPUSHGETGLOBAL) + 1;*)
          decr sp; stack.(!sp) <- !accu;
          incr pc; let offset = code.(!pc) in
          accu := getglobal offset
      | OpGETGLOBALFIELD -> (*instructs.(opGETGLOBALFIELD) <- instructs.(opGETGLOBALFIELD) + 1;*)
          incr pc; let offset = code.(!pc) in
          incr pc; let field = code.(!pc) in
          accu := (magic (getglobal offset)).(field)
      | OpPUSHGETGLOBALFIELD -> (*instructs.(opPUSHGETGLOBALFIELD) <- instructs.(opPUSHGETGLOBALFIELD) + 1;*)
          
          decr sp; stack.(!sp) <- !accu;
          incr pc; let offset = code.(!pc) in
          incr pc; let pos = code.(!pc) in
          let modul = getglobal offset in
          
          accu := field modul pos;
      
      | OpSETGLOBAL -> (*instructs.(opSETGLOBAL) <- instructs.(opSETGLOBAL) + 1;*) 
(* The structure of the module can not be used by other loaded modules 
   (for now) *)
          
          incr pc; let offset = code.(!pc) in
          setglobal offset !accu;
          accu := unit;
      
      
      | OpATOM0 -> (*instructs.(opATOM0) <- instructs.(opATOM0) + 1;*) 
          accu := repr (Obj.new_block 0 0)
      | OpATOM -> (*instructs.(opATOM) <- instructs.(opATOM) + 1;*) 
          incr pc; let atom = code.(!pc) in
          accu := repr (Obj.new_block atom 0)
      | OpPUSHATOM0 -> (*instructs.(opPUSHATOM0) <- instructs.(opPUSHATOM0) + 1;*) 
          decr sp; stack.(!sp) <- !accu;
          accu := repr (Obj.new_block 0 0)
      | OpPUSHATOM -> (*instructs.(opPUSHATOM) <- instructs.(opPUSHATOM) + 1;*)
          decr sp; stack.(!sp) <- !accu;
          incr pc; let atom = code.(!pc) in
          accu := repr (Obj.new_block atom 0)
      | OpMAKEBLOCK -> (*instructs.(opMAKEBLOCK) <- instructs.(opMAKEBLOCK) + 1;*)
          incr pc; let wosize = code.(!pc) in
          incr pc; let tag = code.(!pc) in
          let tab = Obj.magic (Obj.new_block tag wosize) in
          for i = 0 to wosize - 2 do
            tab.(i+1) <- stack.(!sp); incr sp
          done;
          tab.(0) <- !accu;
          accu := repr tab;
      | OpMAKEBLOCK1 -> (*instructs.(opMAKEBLOCK1) <- instructs.(opMAKEBLOCK1) + 1;*)
          incr pc; let tag = code.(!pc) in
          let tab = new_block tag 1 in
          set_field tab 0 !accu;
          accu := tab
      | OpMAKEBLOCK2 -> (*instructs.(opMAKEBLOCK2) <- instructs.(opMAKEBLOCK2) + 1;*)
          incr pc; let tag = code.(!pc) in
          let tab = new_block tag 2 in
          set_field tab 0 !accu;
          set_field tab 1 stack.(!sp); incr sp;
          accu := tab;
      | OpMAKEBLOCK3 -> (*instructs.(opMAKEBLOCK3) <- instructs.(opMAKEBLOCK3) + 1;*)
          incr pc; let tag = code.(!pc) in
          let tab = new_block tag 3 in
          set_field tab 0 !accu;
          set_field tab 1 stack.(!sp); incr sp;
          set_field tab 2 stack.(!sp); incr sp;
          accu := tab;
      | OpMAKEFLOATBLOCK -> (*instructs.(opMAKEFLOATBLOCK) <- instructs.(opMAKEFLOATBLOCK) + 1;*) 
          incr pc; let wosize = code.(!pc) in
          let tab = Array.create wosize 0.0 in
          for i = 0 to wosize - 2 do
            tab.(i+1) <- magic (field (repr stack) (!sp + i))
          done;
          tab.(0) <- ((magic !accu) : float);
          accu := repr tab;
          sp := !sp + wosize - 1
      
      | OpGETFIELD0 -> (*instructs.(opGETFIELD0) <- instructs.(opGETFIELD0) + 1;*) accu := field !accu 0
      | OpGETFIELD1 -> (*instructs.(opGETFIELD1) <- instructs.(opGETFIELD1) + 1;*) accu := field !accu 1
      | OpGETFIELD2 -> (*instructs.(opGETFIELD2) <- instructs.(opGETFIELD2) + 1;*) accu := field !accu 2
      | OpGETFIELD3 -> (*instructs.(opGETFIELD3) <- instructs.(opGETFIELD3) + 1;*) accu := field !accu 3
      | OpGETFIELD -> (*instructs.(opGETFIELD) <- instructs.(opGETFIELD) + 1;*) 
          incr pc; let pos = code.(!pc) in
          accu := field !accu pos
      | OpGETFLOATFIELD -> (*instructs.(opGETFLOATFIELD) <- instructs.(opGETFLOATFIELD) + 1;*) 
          incr pc; let pos = code.(!pc) in
          let tab = (magic !accu : float array) in
          accu := repr (tab.(pos))
      | OpSETFIELD0 -> (*instructs.(opSETFIELD0) <- instructs.(opSETFIELD0) + 1;*) 
          let arg = stack.(!sp) in incr sp;
          (Obj.magic !accu).(0) <- arg
      | OpSETFIELD1 -> (*instructs.(opSETFIELD1) <- instructs.(opSETFIELD1) + 1;*)
          let arg = stack.(!sp) in incr sp;
          (Obj.magic !accu).(1) <- arg
      | OpSETFIELD2 -> (*instructs.(opSETFIELD2) <- instructs.(opSETFIELD2) + 1;*)
          let arg = stack.(!sp) in incr sp;
          (Obj.magic !accu).(2) <- arg
      | OpSETFIELD3 -> (*instructs.(opSETFIELD3) <- instructs.(opSETFIELD3) + 1;*)
          let arg = stack.(!sp) in incr sp;
          (Obj.magic !accu).(3) <- arg
      | OpSETFIELD -> (*instructs.(opSETFIELD) <- instructs.(opSETFIELD) + 1;*)
          let arg = stack.(!sp) in incr sp;
          incr pc; let pos = code.(!pc) in
          set_field  !accu pos arg
      | OpSETFLOATFIELD -> (*instructs.(opSETFLOATFIELD) <- instructs.(opSETFLOATFIELD) + 1;*)
          let arg = stack.(!sp) in incr sp;
          incr pc; let pos = code.(!pc) in
          let tab = (magic !accu : float array) in
          tab.(pos) <- (magic arg: float);
      | OpVECTLENGTH -> (*instructs.(opVECTLENGTH) <- instructs.(opVECTLENGTH) + 1;*)
(* dangerous if float array *)
          accu := repr (Array.length (Obj.magic !accu))
      | OpGETVECTITEM -> (*instructs.(opGETVECTITEM) <- instructs.(opGETVECTITEM) + 1;*)
          accu := repr (magic !accu).(magic stack.(!sp)); incr sp
      | OpSETVECTITEM -> (*instructs.(opSETVECTITEM) <- instructs.(opSETVECTITEM) + 1;*)
          (magic !accu).(magic stack.(!sp)) <- stack.(!sp + 1); sp := !sp + 2
      | OpGETSTRINGCHAR -> (*instructs.(opGETSTRINGCHAR) <- instructs.(opGETSTRINGCHAR) + 1;*)
          accu := repr (magic !accu).[magic stack.(!sp)]; incr sp
      | OpSETSTRINGCHAR -> (*instructs.(opSETSTRINGCHAR) <- instructs.(opSETSTRINGCHAR) + 1;*)
          (magic !accu).[magic stack.(!sp)] <- 
            Char.chr ((magic stack.(!sp + 1)) land 0xff); 
          sp := !sp + 2
      | OpBRANCH -> (*instructs.(opBRANCH) <- instructs.(opBRANCH) + 1;*)
          let offset = magic code.(!pc+1) in
          pc := !pc + offset;
      
      
      | OpBRANCHIF -> (*instructs.(opBRANCHIF) <- instructs.(opBRANCHIF) + 1;*)
          let offset = magic code.(!pc + 1) in
          if !accu <> unit then pc := !pc + offset else incr pc
      | OpBRANCHIFNOT -> (*instructs.(opBRANCHIFNOT) <- instructs.(opBRANCHIFNOT) + 1;*)
          let offset = code.(!pc + 1) in
          if (magic !accu:int) == 0 then pc := !pc + offset else incr pc
      | OpSWITCH -> (*instructs.(opSWITCH) <- instructs.(opSWITCH) + 1;*) (* inputu + inputs *)
          
          incr pc; let sizes = code.(!pc) in
          
          let obj = repr !accu in
          if is_block obj then
            begin
              
              pc := !pc + code.(!pc + 1 + (sizes land 0xffff) + tag obj);
            
            end
          else
            begin
              let index = magic !accu in
              
              if index >= 0 && index  < (sizes land 0xffff) then
                pc := !pc + code.(!pc + 1 + index)
              else
                pc := !pc + (sizes land 0xffff) + (sizes lsr 16);
            
            end;
      
      | OpBOOLNOT -> (*instructs.(opBOOLNOT) <- instructs.(opBOOLNOT) + 1;*) accu := repr (1 - (magic !accu))
      | OpPUSHTRAP -> (*instructs.(opPUSHTRAP) <- instructs.(opPUSHTRAP) + 1;*) 
          incr pc; 
          let trap_link = !pc - 1 + magic code.(!pc) in
          let trapsp = !sp in
          
          let goto = !pc in
          sp := !sp - 4;
          begin
            try
              extern_sp := !sp;
              accu := interp closr (!pc+1) !accu;
            with
              PopTrap (newpc,newacc) -> 
                sp := trapsp; (*!sp + 4;*) 
                pc := newpc; 
                accu := newacc; 
            | e ->
                sp := trapsp;
                accu := repr e;
                pc := trap_link
          end
      | OpPOPTRAP -> (*instructs.(opPOPTRAP) <- instructs.(opPOPTRAP) + 1;*)
          raise (PopTrap (!pc,!accu))
      | OpRAISE -> (*instructs.(opRAISE) <- instructs.(opRAISE) + 1;*)
          raise (Obj.magic !accu);
      | OpCHECK_SIGNALS -> (*instructs.(opCHECK_SIGNALS) <- instructs.(opCHECK_SIGNALS) + 1;*) ()
      | OpC_CALL1 -> (*instructs.(opC_CALL1) <- instructs.(opC_CALL1) + 1;*) 
          incr pc; let prim = code.(!pc) in
          extern_sp := !sp;
          accu := (Obj.magic caml_prims.(prim)) !accu
      | OpC_CALL2 -> (*instructs.(opC_CALL2) <- instructs.(opC_CALL2) + 1;*)
          let arg = stack.(!sp) in
          incr sp;
          incr pc; let prim = code.(!pc) in
          extern_sp := !sp;
          accu := (Obj.magic caml_prims.(prim)) !accu arg
      | OpC_CALL3 -> (*instructs.(opC_CALL3) <- instructs.(opC_CALL3) + 1;*)
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp+1) in
          sp := !sp + 2;
          incr pc; let prim = code.(!pc) in
          extern_sp := !sp;
          accu := (Obj.magic caml_prims.(prim)) !accu arg1 arg2;
      
      | OpC_CALL4 -> (*instructs.(opC_CALL4) <- instructs.(opC_CALL4) + 1;*)
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp+1) in
          let arg3 = stack.(!sp+2) in
          sp := !sp + 3;
          incr pc; let prim = code.(!pc) in
          extern_sp := !sp;
          accu := (Obj.magic caml_prims.(prim)) !accu arg1 arg2 arg3
      | OpC_CALL5 -> (*instructs.(opC_CALL5) <- instructs.(opC_CALL5) + 1;*)
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp+1) in
          let arg3 = stack.(!sp+2) in
          let arg4 = stack.(!sp+3) in
          sp := !sp + 4;
          incr pc; let prim = code.(!pc) in
          extern_sp := !sp;
          accu := (Obj.magic caml_prims.(prim)) !accu arg1 arg2 arg3 arg4
      | OpC_CALLN -> (*instructs.(opC_CALLN) <- instructs.(opC_CALLN) + 1;*)
          incr pc; let nargs = code.(!pc) in
          decr sp; stack.(!sp) <- !accu;
          let args = Array.create nargs (repr 0) in
          for i = nargs-1 downto 0 do 
            args.(i) <- stack.(!sp + i)
          done;
          sp := !sp + nargs;
          incr pc; let prim = code.(!pc) in
          extern_sp := !sp;
          accu := (Obj.magic caml_prims.(prim)) args  nargs
      | OpCONST0 -> (*instructs.(opCONST0) <- instructs.(opCONST0) + 1;*) accu := repr 0
      | OpCONST1 -> (*instructs.(opCONST1) <- instructs.(opCONST1) + 1;*) accu := repr 1
      | OpCONST2 -> (*instructs.(opCONST2) <- instructs.(opCONST2) + 1;*) accu := repr 2
      | OpCONST3 -> (*instructs.(opCONST3) <- instructs.(opCONST3) + 1;*) accu := repr 3
      | OpCONSTINT -> (*instructs.(opCONSTINT) <- instructs.(opCONSTINT) + 1;*)
          incr pc; let const = magic code.(!pc) in
          accu := repr const
      | OpPUSHCONST0 -> (*instructs.(opPUSHCONST0) <- instructs.(opPUSHCONST0) + 1;*)
          decr sp; stack.(!sp) <- !accu; accu := repr 0
      | OpPUSHCONST1 -> (*instructs.(opPUSHCONST1) <- instructs.(opPUSHCONST1) + 1;*)
          decr sp; stack.(!sp) <- !accu; accu := repr 1
      | OpPUSHCONST2 -> (*instructs.(opPUSHCONST2) <- instructs.(opPUSHCONST2) + 1;*)
          decr sp; stack.(!sp) <- !accu; accu := repr 2
      | OpPUSHCONST3 -> (*instructs.(opPUSHCONST3) <- instructs.(opPUSHCONST3) + 1;*)
          decr sp; stack.(!sp) <- !accu; accu := repr 3
      | OpPUSHCONSTINT -> (*instructs.(opPUSHCONSTINT) <- instructs.(opPUSHCONSTINT) + 1;*)
          decr sp; stack.(!sp) <- !accu;
          incr pc; let const = magic code.(!pc) in
          accu := repr const
      | OpNEGINT -> (*instructs.(opNEGINT) <- instructs.(opNEGINT) + 1;*) accu := repr (- (magic !accu))
      | OpADDINT -> (*instructs.(opADDINT) <- instructs.(opADDINT) + 1;*) 
          accu := repr ((magic !accu) + magic stack.(!sp));
          incr sp
      | OpSUBINT -> (*instructs.(opSUBINT) <- instructs.(opSUBINT) + 1;*)
          accu := repr ((magic !accu) - magic stack.(!sp)); 
          incr sp
      | OpMULINT -> (*instructs.(opMULINT) <- instructs.(opMULINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) * arg)
      | OpDIVINT -> (*instructs.(opDIVINT) <- instructs.(opDIVINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          if arg = 0 then raise Division_by_zero;
          accu := repr ((magic !accu) / arg)
      | OpMODINT -> (*instructs.(opMODINT) <- instructs.(opMODINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          if arg = 0 then raise Division_by_zero;
          accu := repr ((magic !accu) mod arg)
      | OpANDINT -> (*instructs.(opANDINT) <- instructs.(opANDINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) land arg)
      | OpORINT -> (*instructs.(opORINT) <- instructs.(opORINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) lor arg)
      | OpXORINT -> (*instructs.(opXORINT) <- instructs.(opXORINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) lxor arg)
      | OpLSLINT -> (*instructs.(opLSLINT) <- instructs.(opLSLINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) lsl arg)
      | OpLSRINT -> (*instructs.(opLSRINT) <- instructs.(opLSRINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) lsr arg)
      | OpASRINT -> (*instructs.(opASRINT) <- instructs.(opASRINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) asr arg)
      | OpEQ -> (*instructs.(opEQ) <- instructs.(opEQ) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) == arg)
      | OpNEQ -> (*instructs.(opNEQ) <- instructs.(opNEQ) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr (not ((magic !accu) == arg))
      | OpLTINT -> (*instructs.(opLTINT) <- instructs.(opLTINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) < arg)
      | OpLEINT -> (*instructs.(opLEINT) <- instructs.(opLEINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) <= arg)
      | OpGTINT -> (*instructs.(opGTINT) <- instructs.(opGTINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) > arg)
      | OpGEINT -> (*instructs.(opGEINT) <- instructs.(opGEINT) + 1;*)
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) >= arg)
      | OpOFFSETINT -> (*instructs.(opOFFSETINT) <- instructs.(opOFFSETINT) + 1;*)
          incr pc; let offset = magic code.(!pc) in
          accu := repr ((magic !accu) + offset)
      | OpOFFSETREF -> (*instructs.(opOFFSETREF) <- instructs.(opOFFSETREF) + 1;*) 
          incr pc; let offset = magic code.(!pc) in
          (Obj.magic (magic !accu)).(0) <- (Obj.magic (magic !accu)).(0) + offset;
          accu := repr 0;
      | OpGETMETHOD -> (*instructs.(opGETMETHOD) <- instructs.(opGETMETHOD) + 1;*)
          let label = 2 * (magic !accu: int) in
          accu := (field (field (field (stack.(!sp)) 0)
              ((label lsr 16) / value_size))
            ((label / value_size) land 0xff))
      | OpSTOP -> (*instructs.(opSTOP) <- instructs.(opSTOP) + 1;*) raise ExitInterp
      | OpEVENT -> (*instructs.(opEVENT) <- instructs.(opEVENT) + 1;*) 
          assert false; (* Not implemented *)          
      | OpBREAK -> (*instructs.(opBREAK) <- instructs.(opBREAK) + 1;*)
          assert false; (* Not implemented *)          
    done;
    unit
  with
    ExitInterp ->
      extern_sp := !sp;
      !accu

and closure_byte closr clos goto arg =
  let sp = extern_sp in
  let trapsp = !extern_sp in
  decr extern_sp; stack.(!sp) <- arg;
  let clos = { closr with env = clos; recenv = (0,clos) } in
  let v = interp clos goto unit in
  sp := trapsp;
  v

and closure1 closr goto arg1 arg2 =
  let sp = extern_sp in
  sp := !sp - 2;
  stack.(!sp+1) <- arg2;
  stack.(!sp) <- arg1;
  let v = magic (interp closr goto unit) in
  v

and closure2 closr goto arg1 arg2 arg3 =
  let sp = extern_sp in
  sp := !sp - 3;
  stack.(!sp+2) <- arg3;
  stack.(!sp+1) <- arg2;
  stack.(!sp) <- arg1;
  let v = magic (interp closr goto unit) in
  v
and closure3 closr goto arg1 arg2 arg3 arg4 =
  let sp = extern_sp in
  sp := !sp - 4;
  stack.(!sp+3) <- arg4;
  stack.(!sp+2) <- arg3;
  stack.(!sp+1) <- arg2;
  stack.(!sp) <- arg1;
  let v = magic (interp closr goto unit) in
  v


let caml_globals = (magic caml_globals: Obj.t array)
  