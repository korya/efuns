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
open Printf

module Obj = struct  
    type t = A of int | B of float | C of string
    
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
  end

open Obj
  
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
let sp = ref 10000

let value_size = Sys.word_size / 8
let step = ref 0
  
(*
  Les closures Bytecode sont des fonctions native prenant ses arguments un par
un. Lorsque tous les arguments d'une closure Bytecode sont disponibles,
la fonction native les empile et lance le Bytecode.
*)
exception ExitInterp
exception PopTrap of int * t

let instruct (opcode : int) = (magic opcode : Instruct.instruction)
let unsigned (v : 'a) = (magic v : int)
let unit = repr 0
let closure (accu : t) = (magic accu : t -> 'a)

let rec interp stack (code: int array) pc accu env recenv =
(*  printf "interp"; print_newline ();  *)
  let pc = ref (pc - 1) in
  let accu = ref (accu : t) in 
  let env = (env : t array) in
  try
    while true do
      incr pc;
(*
      incr step;
      printf "%d: PC(%d) %s SP(%d)" !step !pc Instruct.opnames.(unsigned code.(!pc)) !sp;
      print_newline ();
      *)
      
      match magic code.(!pc) with
      | OpACC0 -> accu := stack.(!sp) 
      | OpACC1 -> accu := stack.(!sp + 1)
      | OpACC2 -> accu := stack.(!sp + 2)
      | OpACC3 -> accu := stack.(!sp + 3)
      | OpACC4 -> accu := stack.(!sp + 4)
      | OpACC5 -> accu := stack.(!sp + 5)
      | OpACC6 -> accu := stack.(!sp + 6)
      | OpACC7 -> accu := stack.(!sp + 7)
      | OpACC -> 
          incr pc; let offset = unsigned code.(!pc) in
          accu := stack.(!sp + offset)
      | OpPUSH ->
          decr sp; stack.(!sp) <- !accu
      | OpPUSHACC0 ->
          decr sp; stack.(!sp) <- !accu
      | OpPUSHACC1 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 1)
      | OpPUSHACC2 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 2)
      | OpPUSHACC3 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 3)
      | OpPUSHACC4 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 4)
      | OpPUSHACC5 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 5)
      | OpPUSHACC6 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 6)
      | OpPUSHACC7 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 7)
      | OpPUSHACC -> decr sp; stack.(!sp) <- !accu; 
          incr pc; let offset = unsigned code.(!pc) in
          accu := stack.(!sp + offset)
      | OpPOP -> 
          incr pc; let offset = unsigned code.(!pc) in
          sp := !sp + offset
      | OpASSIGN ->
          incr pc; let offset = unsigned code.(!pc) in
          stack.(!sp + offset) <- !accu;
          accu := unit
      | OpENVACC1 -> accu := env.(1)
      | OpENVACC2 -> accu := env.(2)
      | OpENVACC3 -> accu := env.(3)
      | OpENVACC4 -> accu := env.(4)
      | OpENVACC ->
          incr pc; let offset = unsigned code.(!pc) in
          accu := env.(offset)
      | OpPUSHENVACC1  ->
          decr sp; stack.(!sp) <- !accu; accu := env.(1)
      | OpPUSHENVACC2  ->
          decr sp; stack.(!sp) <- !accu; accu := env.(2)
      | OpPUSHENVACC3  ->
          decr sp; stack.(!sp) <- !accu; accu := env.(3)
      | OpPUSHENVACC4  ->
          decr sp; stack.(!sp) <- !accu; accu := env.(4)
      | OpPUSHENVACC  ->
          incr pc; let offset = unsigned code.(!pc) in
          decr sp; stack.(!sp) <- !accu; accu := env.(offset)
      | OpPUSH_RETADDR  ->
          incr pc; let offset = magic code.(!pc) in
          decr sp; stack.(!sp) <- repr (!pc + offset - 1);
          decr sp; decr sp
      | OpAPPLY  ->
          let nargs = unsigned code.(!pc+1) in
          for i = 0 to nargs - 1 do
            incr sp;
            accu := (closure !accu) stack.(!sp - 1);
          done;
          sp := !sp + 2;
          pc := magic stack.(!sp); incr sp;
      | OpAPPLY1  ->
          let arg = stack.(!sp) in incr sp;
          accu := (closure !accu) arg;
      | OpAPPLY2  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          sp := !sp + 2;
          accu := (closure !accu) arg1 arg2;
      | OpAPPLY3  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          let arg3 = stack.(!sp + 2) in
          sp := !sp + 3;
          accu := (closure !accu) arg1 arg2 arg3;
      | OpAPPTERM  ->
          incr pc; let nargs = unsigned code.(!pc) in
          incr pc; let slotsize = unsigned code.(!pc) in
          let trapsp = !sp + slotsize in
          let newsp = trapsp - nargs in
          for i = nargs - 1 downto 0 do
            stack.(newsp + i) <- stack.(!sp + i)
          done;
          sp := newsp;
          for i = 0 to nargs - 1 do
            accu := (closure !accu) stack.(newsp + i);
          done;
          (* sp := trapsp; *)
          raise ExitInterp
      | OpAPPTERM1  ->
          let arg = stack.(!sp) in
          let slotsize = unsigned code.(!pc+1) in
          sp := !sp + slotsize;
          accu := (closure !accu) arg;
          (* sp := trapsp; *)
          raise ExitInterp
      | OpAPPTERM2  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          let slotsize = unsigned code.(!pc+1) in
          sp := !sp + slotsize;
          accu := (closure !accu) arg1 arg2;
          (* sp := trapsp; *)
          raise ExitInterp
      | OpAPPTERM3  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          let arg3 = stack.(!sp + 2) in
          let slotsize = unsigned code.(!pc+1) in
          sp := !sp + slotsize;
          accu := (closure !accu) arg1 arg2 arg3;
          (* sp := trapsp; *)
          raise ExitInterp
      | OpRETURN  -> 
          (* inutile 
          incr pc; let offset = unsigned code.(!pc) in
          sp := !sp + offset; *)
          raise ExitInterp
      | OpRESTART  -> assert false
      | OpGRAB  ->
          incr pc; let required = unsigned code.(!pc) in
          let goto = !pc + 1 in
          let arg1 = stack.(!sp) in incr sp;
          accu :=
          (match required with
              1 -> repr
                  (fun arg2 ->
                    let trapsp = !sp in
                    sp := !sp - 2;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp stack code goto unit env recenv) in
                    sp := trapsp;
                    v
                )
            | 2 -> repr
                  (fun arg2 arg3 ->
                    let trapsp = !sp in
                    sp := !sp - 3;
                    stack.(!sp+2) <- arg3;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp stack  code goto unit env recenv) in
                    sp := trapsp;
                    v
                )
            | 3 -> repr
                  (fun arg2 arg3 arg4 ->
                    let trapsp = !sp in
                    sp := !sp - 4;
                    stack.(!sp+3) <- arg4;
                    stack.(!sp+2) <- arg3;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp stack code goto unit env recenv) in
                    sp := trapsp;
                    v
                )
            | _ ->
                let rec f args arg =
                  if (List.length args) = required then
                    let trapsp = !sp in
                    let argss = Array.of_list (List.rev (arg::args)) in
                    sp := !sp - required - 1;
                    for i = 0 to required do
                      stack.(!sp + i) <- argss.(i)
                    done;                  
                    let v = magic (interp stack code goto unit env recenv) in
                    sp := trapsp;
                    v
                  else
                    (Obj.magic f : t list -> t -> t) (arg :: args)
                in
                repr (f [arg1]));
          raise ExitInterp
      | OpCLOSURE  ->
          
          let nvars = unsigned code.(!pc+1) in
          pc := !pc+2;
          let goto = !pc + magic code.(!pc) in
          if nvars > 0 then (decr sp; stack.(!sp) <- !accu);
          let clos = Array.create (1+nvars) unit in
          clos.(0) <- repr goto;
          for i = 0 to nvars - 1 do
            clos.(i+1) <- stack.(!sp + i)
          done;
          sp := !sp + nvars;
          
          (* We can optimize the *most* current case: 
          code.(goto)  contains a GRAB instruction, which will create
          a new closure. We can create this closure now and we do.
          *)
          accu :=
          if code.(goto) = opGRAB then
            let required = code.(goto+1) in
            let goto = goto + 2 in
            let env = clos in
            let recenv = (0,clos) in
            match required with
              1 -> repr
                  (fun arg1 arg2 ->
                    let trapsp = !sp in
                    sp := !sp - 2;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp stack code goto unit env recenv) in
                    sp := trapsp;
                    v
                )
            | 2 -> repr
                  (fun arg1 arg2 arg3 ->
                    let trapsp = !sp in
                    sp := !sp - 3;
                    stack.(!sp+2) <- arg3;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp stack code goto unit env recenv) in
                    sp := trapsp;
                    v
                )
            | 3 -> repr
                  (fun arg1 arg2 arg3 arg4 ->
                    let trapsp = !sp in
                    sp := !sp - 4;
                    stack.(!sp+3) <- arg4;
                    stack.(!sp+2) <- arg3;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp stack code goto unit env recenv) in
                    sp := trapsp;
                    v
                )
            | 4 -> repr
                  (fun arg1 arg2 arg3 arg4 arg5 ->
                    let trapsp = !sp in
                    sp := !sp - 5;
                    stack.(!sp+4) <- arg5;
                    stack.(!sp+3) <- arg4;
                    stack.(!sp+2) <- arg3;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp stack code goto unit env recenv) in
                    sp := trapsp;
                    v
                )
            | _ ->
                let rec f args arg =
                  if (List.length args) = required then
                    let trapsp = !sp in
                    let argss = Array.of_list (List.rev (arg::args)) in
                    sp := !sp - required - 1;
                    for i = 0 to required do
                      stack.(!sp + i) <- argss.(i)
                    done;                  
                    let v = magic (interp stack code goto unit env recenv) in
                    sp := trapsp;
                    v
                  else
                    (Obj.magic f : t list -> t -> t) (arg :: args)
                in
                repr (f [])
          else
            repr (fun arg -> 
                let trapsp = !sp in
                decr sp; stack.(!sp) <- arg;
                let v = interp stack code goto unit clos (0,clos) in
                sp := trapsp;
                v);
      
      | OpCLOSUREREC  -> 
          incr pc; let nfuncs = unsigned code.(!pc) in
          incr pc; let nvars = unsigned code.(!pc) in
          if nvars > 0 then (decr sp; stack.(!sp) <- !accu);
          let size = nfuncs * 2 - 1 + nvars in
          let clos = Array.create size unit in
          for i = 0 to nvars - 1 do
            clos.(nfuncs * 2 - 1 + i) <- stack.(!sp + i);
          done;
          sp := !sp + nvars;
          let f i goto arg =
            let trapsp = !sp in
            decr sp; stack.(!sp) <- arg;
            let env = Array.sub clos (i*2) (size - i*2) in
            
            let v = interp stack code goto unit env (i*2,clos)  in
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
      | OpOFFSETCLOSUREM2  ->
          let (pos,env) = recenv in
          accu := env.(pos-2)
      | OpOFFSETCLOSURE0  ->
          let (pos,env) = recenv in
          accu := env.(pos)
      | OpOFFSETCLOSURE2  ->
          let (pos,env) = recenv in
          accu := env.(pos+2)
      | OpOFFSETCLOSURE  ->
          incr pc; let offset = magic code.(!pc) in
          let (pos,env) = recenv in
          accu := env.(pos+offset)
      | OpPUSHOFFSETCLOSUREM2  ->
          decr sp; stack.(!sp) <- !accu;
          let (pos,env) = recenv in
          accu := env.(pos-2)
      | OpPUSHOFFSETCLOSURE0  ->
          decr sp; stack.(!sp) <- !accu;
          let (pos,env) = recenv in
          accu := env.(pos)
      | OpPUSHOFFSETCLOSURE2  ->
          decr sp; stack.(!sp) <- !accu;
          let (pos,env) = recenv in
          accu := env.(pos+2)
      | OpPUSHOFFSETCLOSURE  ->
          decr sp; stack.(!sp) <- !accu;
          incr pc; let offset = magic code.(!pc) in
          let (pos,env) = recenv in
          accu := env.(pos+offset)
      | OpGETGLOBAL  ->
          
          incr pc; let offset = unsigned code.(!pc) in
          accu := getglobal offset;
      
      | OpPUSHGETGLOBAL  ->
          decr sp; stack.(!sp) <- !accu;
          incr pc; let offset = unsigned code.(!pc) in
          accu := getglobal offset
      | OpGETGLOBALFIELD  ->
          incr pc; let offset = unsigned code.(!pc) in
          incr pc; let field = unsigned code.(!pc) in
          accu := (magic (getglobal offset)).(field)
      | OpPUSHGETGLOBALFIELD  ->
          
          decr sp; stack.(!sp) <- !accu;
          incr pc; let offset = unsigned code.(!pc) in
          incr pc; let pos = unsigned code.(!pc) in
          let modul = getglobal offset in
          
          accu := field modul pos;
      
      | OpSETGLOBAL  -> 
(* The structure of the module can not be used by other loaded modules 
   (for now) *)
          
          incr pc; let offset = unsigned code.(!pc) in
          setglobal offset !accu;
          accu := unit;
      
      
      | OpATOM0  -> 
          accu := repr (Obj.new_block 0 0)
      | OpATOM  -> 
          incr pc; let atom = unsigned code.(!pc) in
          accu := repr (Obj.new_block atom 0)
      | OpPUSHATOM0  -> 
          decr sp; stack.(!sp) <- !accu;
          accu := repr (Obj.new_block 0 0)
      | OpPUSHATOM  ->
          decr sp; stack.(!sp) <- !accu;
          incr pc; let atom = unsigned code.(!pc) in
          accu := repr (Obj.new_block atom 0)
      | OpMAKEBLOCK  ->
          incr pc; let wosize = unsigned code.(!pc) in
          incr pc; let tag = unsigned code.(!pc) in
          let tab = Obj.magic (Obj.new_block tag wosize) in
          for i = 0 to wosize - 2 do
            tab.(i+1) <- stack.(!sp); incr sp
          done;
          tab.(0) <- !accu;
          accu := repr tab;
      | OpMAKEBLOCK1  ->
          incr pc; let tag = unsigned code.(!pc) in
          if tag =0 then accu := repr (ref !accu)
          else
          let tab = new_block tag 1 in
          set_field tab 0 !accu;
          accu := tab
      | OpMAKEBLOCK2  ->
          incr pc; let tag = unsigned code.(!pc) in
          incr sp;
          if tag = 0 then accu := repr (!accu, stack.(!sp-1)) else
          let tab = new_block tag 2 in
          set_field tab 0 !accu;
          set_field tab 1 stack.(!sp-1); 
          accu := tab;
      | OpMAKEBLOCK3  ->
          incr pc; let tag = unsigned code.(!pc) in
          if tag = 0 then
          let tab = new_block tag 3 in
          set_field tab 0 !accu;
          set_field tab 1 stack.(!sp); incr sp;
          set_field tab 2 stack.(!sp); incr sp;
          accu := tab;
      | OpMAKEFLOATBLOCK  -> 
          incr pc; let wosize = unsigned code.(!pc) in
          let tab = Array.create wosize 0.0 in
          for i = 0 to wosize - 2 do
            tab.(i+1) <- magic (field (repr stack) (!sp + i))
          done;
          tab.(0) <- ((magic !accu) : float);
          accu := repr tab;
          sp := !sp + wosize - 1
      
      | OpGETFIELD0  -> accu := field !accu 0
      | OpGETFIELD1  -> accu := field !accu 1
      | OpGETFIELD2  -> accu := field !accu 2
      | OpGETFIELD3  -> accu := field !accu 3
      | OpGETFIELD  -> 
          incr pc; let pos = unsigned code.(!pc) in
          accu := field !accu pos
      | OpGETFLOATFIELD  -> 
          incr pc; let pos = unsigned code.(!pc) in
          let tab = (magic !accu : float array) in
          accu := repr (tab.(pos))
      | OpSETFIELD0  -> 
          let arg = stack.(!sp) in incr sp;
          (Obj.magic !accu).(0) <- arg
      | OpSETFIELD1  ->
          let arg = stack.(!sp) in incr sp;
          (Obj.magic !accu).(1) <- arg
      | OpSETFIELD2  ->
          let arg = stack.(!sp) in incr sp;
          (Obj.magic !accu).(2) <- arg
      | OpSETFIELD3  ->
          let arg = stack.(!sp) in incr sp;
          (Obj.magic !accu).(3) <- arg
      | OpSETFIELD ->
          let arg = stack.(!sp) in incr sp;
          incr pc; let pos = unsigned code.(!pc) in
          set_field  !accu pos arg
      | OpSETFLOATFIELD  ->
          let arg = stack.(!sp) in incr sp;
          incr pc; let pos = unsigned code.(!pc) in
          let tab = (magic !accu : float array) in
          tab.(pos) <- (magic arg: float);
      | OpVECTLENGTH  ->
(* dangerous if float array *)
          accu := repr (Array.length (Obj.magic !accu))
      | OpGETVECTITEM  ->
          accu := repr (magic !accu).(magic stack.(!sp)); incr sp
      | OpSETVECTITEM  ->
          (magic !accu).(magic stack.(!sp)) <- stack.(!sp + 1); sp := !sp + 2
      | OpGETSTRINGCHAR  ->
          accu := repr (magic !accu).[magic stack.(!sp)]; incr sp
      | OpSETSTRINGCHAR  ->
          (magic !accu).[magic stack.(!sp)] <- 
            Char.chr ((magic stack.(!sp + 1)) land 0xff); 
          sp := !sp + 2
      | OpBRANCH  ->
          let offset = magic code.(!pc+1) in
          pc := !pc + offset;
      
      
      | OpBRANCHIF  ->
          let offset = magic code.(!pc + 1) in
          if !accu <> unit then pc := !pc + offset else incr pc
      | OpBRANCHIFNOT  ->
          let offset = magic code.(!pc + 1) in
          if !accu == unit then pc := !pc + offset else incr pc
      | OpSWITCH  -> (* inputu + inputs *)
          
          incr pc; let sizes = unsigned code.(!pc) in
          
          let obj = repr !accu in
          if is_block obj then
            begin
              
              pc := !pc + unsigned code.(!pc + 1 + (sizes land 0xffff) + tag obj);
            
            end
          else
            begin
              let index = magic !accu in
              
              if index >= 0 && index  < (sizes land 0xffff) then
                pc := !pc + unsigned code.(!pc + 1 + index)
              else
                pc := !pc + (sizes land 0xffff) + (sizes lsr 16);
            
            end;
      
      | OpBOOLNOT  -> accu := repr (1 - (magic !accu))
      | OpPUSHTRAP -> 
          incr pc; 
          let trap_link = !pc - 1 + magic code.(!pc) in
          let trapsp = !sp in
          
          let goto = !pc in
          sp := !sp - 4;
          begin
            try
              
              accu := interp stack code (!pc+1) !accu env recenv;
            
            with
              PopTrap (newpc,newacc) -> 
                pc := newpc; 
                
                accu := newacc; 
                sp := !sp + 4
            | e ->
                
                sp := trapsp;
                
                accu := repr e;
                pc := trap_link
          end
      | OpPOPTRAP ->
          raise (PopTrap (!pc,!accu))
      | OpRAISE ->
          
          raise (Obj.magic !accu);
      | OpCHECK_SIGNALS  -> ()
      | OpC_CALL1  -> 
          incr pc; let prim = unsigned code.(!pc) in
          
          accu := (Obj.magic caml_prims.(prim)) !accu
      | OpC_CALL2  ->
          let arg = stack.(!sp) in
          incr sp;
          incr pc; let prim = unsigned code.(!pc) in
          accu := (Obj.magic caml_prims.(prim)) !accu arg
      | OpC_CALL3  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp+1) in
          sp := !sp + 2;
          incr pc; let prim = unsigned code.(!pc) in
          accu := (Obj.magic caml_prims.(prim)) !accu arg1 arg2;
      
      | OpC_CALL4  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp+1) in
          let arg3 = stack.(!sp+2) in
          sp := !sp + 3;
          incr pc; let prim = unsigned code.(!pc) in
          accu := (Obj.magic caml_prims.(prim)) !accu arg1 arg2 arg3
      | OpC_CALL5  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp+1) in
          let arg3 = stack.(!sp+2) in
          let arg4 = stack.(!sp+3) in
          sp := !sp + 4;
          incr pc; let prim = unsigned code.(!pc) in
          accu := (Obj.magic caml_prims.(prim)) !accu arg1 arg2 arg3 arg4
      | OpC_CALLN  ->
          incr pc; let nargs = unsigned code.(!pc) in
          decr sp; stack.(!sp) <- !accu;
          let args = Array.init nargs (fun i -> stack.(!sp + i)) in
          sp := !sp + nargs;
          incr pc; let prim = unsigned code.(!pc) in
          accu := (Obj.magic caml_prims.(prim)) args  nargs
      | OpCONST0  -> accu := repr 0
      | OpCONST1  -> accu := repr 1
      | OpCONST2  -> accu := repr 2
      | OpCONST3  -> accu := repr 3
      | OpCONSTINT  ->
          incr pc; let const = magic code.(!pc) in
          accu := repr const
      | OpPUSHCONST0  ->
          decr sp; stack.(!sp) <- !accu; accu := repr 0
      | OpPUSHCONST1  ->
          decr sp; stack.(!sp) <- !accu; accu := repr 1
      | OpPUSHCONST2  ->
          decr sp; stack.(!sp) <- !accu; accu := repr 2
      | OpPUSHCONST3  ->
          decr sp; stack.(!sp) <- !accu; accu := repr 3
      | OpPUSHCONSTINT  ->
          decr sp; stack.(!sp) <- !accu;
          incr pc; let const = magic code.(!pc) in
          accu := repr const
      | OpNEGINT  -> accu := repr (- (magic !accu))
      | OpADDINT  -> 
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) + arg)
      | OpSUBINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) - arg)
      | OpMULINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) * arg)
      | OpDIVINT  ->
          let arg = magic stack.(!sp) in incr sp;
          if arg = 0 then raise Division_by_zero;
          accu := repr ((magic !accu) / arg)
      | OpMODINT  ->
          let arg = magic stack.(!sp) in incr sp;
          if arg = 0 then raise Division_by_zero;
          accu := repr ((magic !accu) mod arg)
      | OpANDINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) land arg)
      | OpORINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) lor arg)
      | OpXORINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) lxor arg)
      | OpLSLINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) lsl arg)
      | OpLSRINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) lsr arg)
      | OpASRINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) asr arg)
      | OpEQ  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) == arg)
      | OpNEQ  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr (not ((magic !accu) == arg))
      | OpLTINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) < arg)
      | OpLEINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) <= arg)
      | OpGTINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) > arg)
      | OpGEINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) >= arg)
      | OpOFFSETINT  ->
          incr pc; let offset = magic code.(!pc) in
          accu := repr ((magic !accu) + offset)
      | OpOFFSETREF  -> 
          incr pc; let offset = magic code.(!pc) in
          (Obj.magic (magic !accu)).(0) <- (Obj.magic (magic !accu)).(0) + offset;
          accu := repr 0;
      | OpGETMETHOD  ->
          let label = 2 * unsigned !accu in
          accu := (field (field (field (stack.(!sp)) 0)
              ((label lsr 16) / value_size))
            ((label / value_size) land 0xff))
      | OpSTOP  -> raise ExitInterp
      | OpEVENT  -> 
          assert false; (* Not implemented *)          
      | OpBREAK  ->
          assert false; (* Not implemented *)          
    done;
    unit
  with
    ExitInterp ->       
      !accu
      
      