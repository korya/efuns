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

(*
  Les closures Bytecode sont des fonctions native prenant ses arguments un par
un. Lorsque tous les arguments d'une closure Bytecode sont disponibles,
la fonction native les empile et lance le Bytecode.
*)
exception ExitInterp
exception PopTrap of int * t

let d = 0x3fffffff
let signed (v : t) =  (* Only for 64 bits architectures *)
  let v = magic v in
  let sign = if v > d then v - (1 lsl 32) else v in
  sign
let unit = repr 0

let closure (v:t) = (magic v : t -> 'a)
let int (v:t) = (magic v : int)
let instruct (v:t) = (magic v : Instruct.instruction)
external int : t -> int = "%identity"

let rec interp name code (pc : int) (accu : t) (env : t array) recenv =
  let rec exec pc sp accu =
(*
    Printf.printf "Pc(%d):" pc;
    Printf.printf " %s" (Instruct.opnames.((magic code.(pc))));
    print_newline ();
*)
    match instruct code.(pc) with
    | OpACC0 -> exec (pc+1) sp stack.(sp) 
    | OpACC1 -> exec (pc+1) sp stack.(sp + 1)
    | OpACC2 -> exec (pc+1) sp stack.(sp + 2)
    | OpACC3 -> exec (pc+1) sp stack.(sp + 3)
    | OpACC4 -> exec (pc+1) sp stack.(sp + 4)
    | OpACC5 -> exec (pc+1) sp stack.(sp + 5)
    | OpACC6 -> exec (pc+1) sp stack.(sp + 6)
    | OpACC7 -> exec (pc+1) sp stack.(sp + 7)
    | OpACC -> 
        let offset = int code.(pc+1) in
        exec (pc+2) sp stack.(sp + offset)

    | OpPUSH 
    | OpPUSHACC0 -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) accu
    | OpPUSHACC1 -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) stack.(sp) 
    | OpPUSHACC2 -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) stack.(sp+1) 
    | OpPUSHACC3 -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) stack.(sp+2) 
    | OpPUSHACC4 -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) stack.(sp+3) 
    | OpPUSHACC5 -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) stack.(sp+4) 
    | OpPUSHACC6 -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) stack.(sp+5) 
    | OpPUSHACC7 -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) stack.(sp+6) 
    | OpPUSHACC  -> stack.(sp - 1) <- accu;
        let offset = int code.(pc+1) in
        exec (pc+2) (sp-1) stack.(sp + offset - 1)

    | OpPOP -> 
        let offset = int code.(pc+1) in exec (pc+2) (sp+offset) accu
    | OpASSIGN ->
        let offset = int code.(pc+1) in
        stack.(sp + offset) <- accu;
        exec (pc+2) sp unit

    | OpENVACC1 -> exec (pc+1) sp env.(1)
    | OpENVACC2 -> exec (pc+1) sp env.(2)
    | OpENVACC3 -> exec (pc+1) sp env.(3)
    | OpENVACC4 -> exec (pc+1) sp env.(4)
    | OpENVACC ->
        let offset = int code.(pc+1) in
        exec (pc+2) sp env.(offset)

    | OpPUSHENVACC1  -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) env.(1)
    | OpPUSHENVACC2  -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) env.(2)
    | OpPUSHENVACC3  -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) env.(3)
    | OpPUSHENVACC4  -> stack.(sp - 1) <- accu;exec (pc+1) (sp-1) env.(4)
    | OpPUSHENVACC   -> stack.(sp - 1) <- accu;
        let offset = int code.(pc+1) in
        exec (pc+2) (sp-1) env.(offset)

    | OpPUSH_RETADDR  ->
        let offset = int code.(pc+1) in
        stack.(sp-1) <- repr (pc + 1 + offset);
        exec (pc+2) (sp-3) accu
    | OpAPPLY  ->
        let nargs = int code.(pc+1) in
        let rec apply1 n sp accu =
          extern_sp := sp+1;
          let res = (closure accu) stack.(sp) in
          if n = 1 then
            (* we could maybe use pc+2 instead of the stack ... *)
            exec (int stack.(sp+3)) (sp+4) res
          else
            apply1 (n-1) (sp+1) res
        in
        apply1 nargs sp accu

      | OpAPPLY1  ->
          extern_sp := sp+1;
          exec (pc+1) (sp+1) ((closure accu) stack.(sp))
      | OpAPPLY2  ->
          extern_sp := sp+2;
          exec (pc+1) (sp+2) ((closure accu) stack.(sp) stack.(sp+1))
      | OpAPPLY3  ->
          extern_sp := sp+3;
          exec (pc+1) (sp+3) ((closure accu) 
                                stack.(sp) stack.(sp+1) stack.(sp+2))
      
      | OpAPPTERM  ->
          let nargs = int code.(pc+1) in
          let slotsize = int code.(pc+2) in
          let trapsp = sp + slotsize in
          let newsp = trapsp - nargs in
          for i = nargs - 1 downto 0 do
            stack.(newsp + i) <- stack.(sp + i)
          done;
          let sp = newsp in
          let rec apply1 n sp accu =
            extern_sp := sp+1;
            if n = 1 then
              (closure accu) stack.(sp) (* RETURN *)
            else
              apply1 (n-1) (sp+1) ((closure accu) stack.(sp))
          in
          apply1 nargs sp accu

      | OpAPPTERM1  ->
          extern_sp := sp+1;
          (closure accu) stack.(sp)
      | OpAPPTERM2  ->
          extern_sp := sp+2;
          (closure accu) stack.(sp) stack.(sp+1)
      | OpAPPTERM3  ->
          extern_sp := sp+3;
          (closure accu) stack.(sp) stack.(sp+1) stack.(sp+2)

      | OpRETURN  -> accu

      | OpRESTART  -> assert false
      | OpGRAB  ->
          let required = int code.(pc+1) in
          let goto = pc + 2 in
          let arg1 = stack.(sp) in
          (match required with
            1 -> repr
                (fun arg2 -> 
(*
                  Printf.printf "GRAB(1) %d %d" (int arg1) (int arg2);
                  print_newline ();
*)
                  let trapsp = !extern_sp in
                  extern_sp := trapsp - 2;
                  stack.(trapsp-1) <- arg2;
                  stack.(trapsp-2) <- arg1;
                  interp name code goto unit env recenv)
             | 2 -> repr
                 (fun arg2 arg3 ->
(*
                   Printf.printf "GRAB(2) %d %d %d" (int arg1) (int arg2) (int arg3);
                   print_newline ();
*)
                   let trapsp = !extern_sp in
                   extern_sp := trapsp - 3;
                   stack.(trapsp-1) <- arg3;
                   stack.(trapsp-2) <- arg2;
                   stack.(trapsp-3) <- arg1;
                   interp name code goto unit env recenv)
             | 3 -> repr
                 (fun arg2 arg3 arg4 ->
                   let trapsp = !extern_sp in
                   extern_sp := trapsp - 4;
                   stack.(trapsp-1) <- arg4;
                   stack.(trapsp-2) <- arg3;
                   stack.(trapsp-3) <- arg2;
                   stack.(trapsp-4) <- arg1;
                   interp name code goto unit env recenv)
             | _ ->
                 let rec f args arg =
                   if (List.length args) = required then
                     let trapsp = !extern_sp in
                     let argss = Array.of_list (List.rev (arg::args)) in
                     extern_sp := trapsp - required - 1;
                     for i = 0 to required do
                       stack.(!extern_sp + i) <- argss.(i)
                     done;                  
                     magic (interp name code goto unit env recenv)
                   else
                     (Obj.magic f : t list -> t -> t) (arg :: args)
                 in
                 repr (f [arg1]))

      | OpCLOSURE  ->
          
          let nvars = int code.(pc+1) in
          let goto = pc + 2 + int code.(pc+2) in
          let clos =
            if nvars = 0 then
              Array.create 1 (repr goto)
            else
              let clos = Array.create (1+nvars) accu in
              clos.(0) <- repr goto;
              for i = 2 to nvars do
                clos.(i) <- stack.(sp + i - 2)
              done;
              clos
          in
          exec (pc+3) (if nvars = 0 then sp else sp + nvars - 1)  
            (repr (fun arg -> 
(*              print_string "Closure strated"; print_newline (); *)
              let trapsp = !extern_sp in
              stack.(trapsp - 1) <- arg;
              extern_sp := trapsp - 1;
              interp name code goto unit clos (0,clos)
                ))
          
      | OpCLOSUREREC  -> 
          let nfuncs = int code.(pc+1) in
          let nvars = int code.(pc+2) in
          let size = nfuncs * 2 - 1 + nvars in
          let clos = Array.create size accu in
          for i = 0 to nvars - 2 do
            clos.(nfuncs * 2 + i) <- stack.(sp + i);
          done;
          let f i goto arg =
            let trapsp = !extern_sp in
            stack.(trapsp - 1) <- arg;
            extern_sp := trapsp - 1;
            let env = Array.sub clos (i*2) (size - i*2) in
            interp name code goto unit env (i*2,clos)
          in
          let newsp = 
            if nvars = 0 then sp else
            sp + nvars - 1
          in
          for i = 0 to nfuncs - 1 do
            let goto = pc + 3 + int code.(pc+3+i) in
(*
            Printf.printf "CLOSUREREC goto %d" goto; print_newline ();
*)
            let fi = repr (f i goto) in
            stack.(newsp - i - 1) <- fi;
            clos.(i*2) <- fi
          done;
          exec (pc+3+nfuncs) (newsp - nfuncs) (repr clos)

      | OpOFFSETCLOSUREM2  ->
          let (pos,env) = recenv in exec (pc+1) sp env.(pos-2)
      | OpOFFSETCLOSURE0  ->
          let (pos,env) = recenv in exec (pc+1) sp env.(pos)
      | OpOFFSETCLOSURE2  ->
          let (pos,env) = recenv in exec (pc+1) sp env.(pos+2)
      | OpOFFSETCLOSURE  ->
          let offset = int code.(pc+1) in
          let (pos,env) = recenv in exec (pc+2) sp env.(pos+offset)

      | OpPUSHOFFSETCLOSUREM2  ->
          stack.(sp-1) <- accu;
          let (pos,env) = recenv in exec (pc+1) (sp-1) env.(pos-2)
      | OpPUSHOFFSETCLOSURE0  ->
          stack.(sp-1) <- accu;
          let (pos,env) = recenv in exec (pc+1) (sp-1) env.(pos)
      | OpPUSHOFFSETCLOSURE2  ->
          stack.(sp-1) <- accu;
          let (pos,env) = recenv in exec (pc+1) (sp-1) env.(pos+2)
      | OpPUSHOFFSETCLOSURE  ->
          stack.(sp-1) <- accu;
          let offset = int code.(pc+1) in
          let (pos,env) = recenv in exec (pc+2) (sp-1) env.(pos+offset)

      | OpGETGLOBAL  ->
          let offset = int code.(pc+1) in
          exec (pc+2) sp (getglobal offset);
      | OpPUSHGETGLOBAL  ->
          stack.(sp-1) <- accu;
          let offset = int code.(pc+1) in
          exec (pc+2) (sp-1) (getglobal offset);
      | OpGETGLOBALFIELD  ->
          let offset = int code.(pc+1) in
          let pos = int code.(pc+2) in
          exec (pc+3) sp (field (getglobal offset) pos)
      | OpPUSHGETGLOBALFIELD  ->
          stack.(sp-1) <- accu;
          let offset = int code.(pc+1) in
          let pos = int code.(pc+2) in
          exec (pc+3) (sp-1) (field (getglobal offset) pos);
      | OpSETGLOBAL  -> 
          let offset = int code.(pc+1) in
          setglobal offset accu;
          exec (pc+2) sp unit

      | OpATOM0  -> 
          exec (pc+1) sp (new_block 0 0)
      | OpATOM  -> 
          let atom = int code.(pc+1) in
          exec (pc+2) sp (new_block atom 0)
      | OpPUSHATOM0  -> 
          stack.(sp-1) <- accu;
          exec (pc+1) (sp-1) (new_block 0 0)
      | OpPUSHATOM  ->
          stack.(sp-1) <- accu;
          let atom = int code.(pc+1) in
          exec (pc+2) (sp-1) (new_block atom 0)

      | OpMAKEBLOCK  ->
          let wosize = int code.(pc+1) in
          let tag = int code.(pc+2) in
          let tab = new_block tag wosize in
          for i = 0 to wosize - 2 do
            set_field tab (i+1) stack.(sp+i)
          done;
          set_field tab 0 accu;
          exec (pc+3) (sp+ wosize - 1) tab
      | OpMAKEBLOCK1  ->
          let tag = int code.(pc+1) in
          let tab = new_block tag 1 in
          set_field tab 0 accu;
          exec (pc+2) sp tab
      | OpMAKEBLOCK2  ->
          let tag = int code.(pc+1) in
          let tab = new_block tag 2 in
          set_field tab 0 accu;
          set_field tab 1 stack.(sp);
          exec (pc+2) (sp+1) tab
      | OpMAKEBLOCK3  ->
          let tag = int code.(pc+1) in
          let tab = new_block tag 3 in
          set_field tab 0 accu;
          set_field tab 1 stack.(sp);
          set_field tab 2 stack.(sp+1);
          exec (pc+2) (sp+2) tab

      | OpGETFIELD0  -> exec (pc+1) sp (field accu 0)
      | OpGETFIELD1  -> exec (pc+1) sp (field accu 1)
      | OpGETFIELD2  -> exec (pc+1) sp (field accu 2)
      | OpGETFIELD3  -> exec (pc+1) sp (field accu 3)
      | OpGETFIELD  -> 
          let pos = int code.(pc+1) in
          exec (pc+2) sp (field accu pos)

      | OpSETFIELD0  -> 
          set_field accu 0 stack.(sp);
          exec (pc+1) (sp+1) accu
      | OpSETFIELD1  ->
          set_field accu 1 stack.(sp);
          exec (pc+1) (sp+1) accu
      | OpSETFIELD2  ->
          set_field accu 2 stack.(sp);
          exec (pc+1) (sp+1) accu
      | OpSETFIELD3  ->
          set_field accu 3 stack.(sp);
          exec (pc+1) (sp+1) accu
      | OpSETFIELD ->
          let pos = int code.(pc+1) in
          set_field accu pos stack.(sp);
          exec (pc+2) (sp+1) accu


      | OpMAKEFLOATBLOCK  -> 
          assert false (* Not implemented *) 
(*
  incr pc; let wosize = code.(pc+1) in
  let tab = Array.create wosize 0.0 in
  for i = 0 to wosize - 2 do
  tab.(i+1) <- (stack.(!sp + i) : float)
  done;
  tab.(0) <- (!accu : float);
  accu := tab;
  sp := !sp + wosize - 1
*)
      | OpSETFLOATFIELD  ->
          assert false (* Not implemented *) 
      | OpGETFLOATFIELD  -> 
          assert false (* Not implemented *) 


(* completely polymorph ? *)
      | OpVECTLENGTH  ->
          exec (pc+1) sp (repr (Array.length (magic accu)))
      | OpGETVECTITEM  ->
          exec (pc+1) (sp+1) (repr (magic accu).(int stack.(sp)))
      | OpSETVECTITEM  ->
          (magic accu).(magic stack.(sp)) <- magic stack.(sp + 1);
          exec (pc+1) (sp+2) accu


      | OpGETSTRINGCHAR  ->
          exec (pc+1) (sp+1) (repr (magic accu).[int stack.(sp)])
      | OpSETSTRINGCHAR  ->
          (magic accu).[int stack.(sp)] <- 
             Char.chr ((int stack.(sp + 1)) land 0xff); 
          exec (pc+1) (sp+2) accu

      | OpBRANCH  ->
          exec (pc + 1 + int code.(pc+1)) sp accu
      | OpBRANCHIF  ->
          exec
            (if int accu == 0 then
              pc+2
            else
              pc + 1 + int code.(pc+1)) sp accu
      | OpBRANCHIFNOT  ->
          exec
            (if int accu == 0 then
              pc + 1 + int code.(pc+1)
            else
              pc+2) sp accu
      | OpSWITCH  ->
          let sizes = int code.(pc+1) in
          exec 
            (if is_block accu then
              pc + 2 + int code.(pc+ 2 + (sizes land 0xffff) + tag accu)
            else
              let index = int accu in
              if index >= 0 && index  < (sizes land 0xffff) then
                pc + 2 + int code.(pc+ 2 + index)
              else
                pc + 2 + (sizes land 0xffff) + (sizes lsr 16)
                  )
            sp accu
          
      | OpBOOLNOT  -> 
          exec (pc+1) sp (repr (1 - (int accu)))
      | OpPUSHTRAP -> 
          let trap_link = pc + int code.(pc+1) in
          extern_sp := sp - 4;
          (try
            interp name code (pc+2) accu env recenv
          with
            PopTrap (newpc,newacc) -> 
              exec newpc sp newacc
          | e ->
              exec trap_link sp (repr e))
      | OpPOPTRAP ->
          raise (PopTrap (pc+1,accu))
      | OpRAISE ->
          raise (magic accu);

      | OpCHECK_SIGNALS  -> exec (pc+1) sp accu
      | OpC_CALL1  -> 
          let prim = int code.(pc+1) in
          extern_sp := sp;
          exec (pc+2) sp ((magic caml_prims.(prim)) accu)
      | OpC_CALL2  ->
          let prim = int code.(pc+1) in
          extern_sp := sp+1;
          exec (pc+2) (sp+1) ((magic caml_prims.(prim)) accu stack.(sp))
      | OpC_CALL3  ->
          let prim = int code.(pc+1) in
          extern_sp := sp+2;
          exec (pc+2) (sp+2)
            ((magic caml_prims.(prim)) accu stack.(sp) stack.(sp+1))
      | OpC_CALL4  ->
          let prim = int code.(pc+1) in
          extern_sp := sp+3;
          exec (pc+2) (sp+3)
            ((magic caml_prims.(prim)) 
               accu stack.(sp) stack.(sp+1) stack.(sp+2))
      | OpC_CALL5  ->
          let prim = int code.(pc+1) in
          extern_sp := sp+4;
          exec (pc+2) (sp+4)
            ((magic caml_prims.(prim)) 
               accu stack.(sp) stack.(sp+1) stack.(sp+2) stack.(sp+3))
      | OpC_CALLN  ->
          let nargs = int code.(pc+1) in
          let prim = int code.(pc+2) in
          let args = Array.create nargs accu in
          for i = 1 to nargs - 1 do
            args.(i) <- stack.(sp + i - 1)
          done;
          extern_sp := sp + nargs - 1;
          exec (pc+3) (sp + nargs - 1)
            ((magic caml_prims.(prim)) args  nargs)

      | OpCONST0  -> exec (pc+1) sp (repr 0)
      | OpCONST1  -> exec (pc+1) sp (repr 1)
      | OpCONST2  -> exec (pc+1) sp (repr 2)
      | OpCONST3  -> exec (pc+1) sp (repr 3)
      | OpCONSTINT  -> exec (pc+2) sp code.(pc+1)

      | OpPUSHCONST0  ->
          stack.(sp-1) <- accu; exec (pc+1) (sp-1) (repr 0)
      | OpPUSHCONST1  ->
          stack.(sp-1) <- accu; exec (pc+1) (sp-1) (repr 1)
      | OpPUSHCONST2  ->
          stack.(sp-1) <- accu; exec (pc+1) (sp-1) (repr 2)
      | OpPUSHCONST3  ->
          stack.(sp-1) <- accu; exec (pc+1) (sp-1) (repr 3)
      | OpPUSHCONSTINT  ->
          stack.(sp-1) <- accu; exec (pc+2) (sp-1) (code.(pc+1))

      | OpNEGINT  -> exec (pc+1) sp (repr (- (int accu)))
      | OpADDINT  -> 
          exec (pc+1) (sp+1) (repr  ((int accu) + (int stack.(sp))))
      | OpSUBINT  ->
          exec (pc+1) (sp+1) (repr  ((int accu) - (int stack.(sp))))
      | OpMULINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) * (int stack.(sp))))
      | OpDIVINT  ->
          let arg = int stack.(sp) in
          if arg == 0 then raise Division_by_zero;
          exec (pc+1) (sp+1) (repr  ((int accu) / arg))
      | OpMODINT  ->
          if int stack.(sp) = 0 then raise Division_by_zero;
          exec (pc+1) (sp+1) (repr  ((int accu) mod (int stack.(sp))))
      | OpANDINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) land (int stack.(sp))))
      | OpORINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) lor (int stack.(sp))))
      | OpXORINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) lxor (int stack.(sp))))
      | OpLSLINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) lsl (int stack.(sp))))
      | OpLSRINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) lsr (int stack.(sp))))
      | OpASRINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) asr (int stack.(sp))))
      | OpEQ  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) == (int stack.(sp))))
      | OpNEQ  ->
          
          exec (pc+1) (sp+1) (repr  (not ((int accu) == (int stack.(sp)))))
      | OpLTINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) < (int stack.(sp))))
      | OpLEINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) <= (int stack.(sp))))
      | OpGTINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) > (int stack.(sp))))
      | OpGEINT  ->
          
          exec (pc+1) (sp+1) (repr  ((int accu) >= (int stack.(sp))))
      | OpOFFSETINT  ->
          exec (pc+2) sp (repr ((int accu) + int code.(pc+1)))
      | OpOFFSETREF  -> 
          set_field accu 0 (repr (int (field accu 0) + (int code.(pc+1))));
          exec (pc+2) sp unit;
      | OpGETMETHOD  ->
          let label = int stack.(sp) in
          exec (pc+1) sp (field (field (field accu 0)
                                   ((label lsr 16) / Sys.word_size))
                            ((label / Sys.word_size) land 0xff))

      | OpSTOP  -> accu
      | OpEVENT  -> 
assert false; (* Not implemented *)          
      | OpBREAK  ->
assert false; (* Not implemented *)          

  in
  exec pc (!extern_sp) accu

