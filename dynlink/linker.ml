(***********************************************************************)
(*                                                                     *)
(*                           Dynlink                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Compat_run
open Interp
  
(* Own Symtable *)
module Symtable = struct
    open Emitcode
    open My_Emitcode
    open Interp
    
    type error =
      Undefined_global of string
    | Unavailable_primitive of string
    
    exception Error of error
    
    let slot_for_literal sc = 
      assert(!next_free_global < Array.length caml_globals);
      caml_globals.(!next_free_global) <- My_Emitcode.transl_const sc;
      incr next_free_global;
      !next_free_global - 1
    
    let slot_for_getglobal id = 
      let name = My_Ident.name id in
      try
        let i = Hashtbl.find globals name in
        Interp.install_module i;
        i
      with
        Not_found ->
          raise (Error (Undefined_global name))
    
    let slot_for_setglobal id = 
      let mod_name = My_Ident.name id in
      assert(!next_free_global < Array.length caml_globals);
      Hashtbl.add globals mod_name !next_free_global;
      incr next_free_global;
      !next_free_global - 1
    
    let num_of_prim name = 
      try
        Hashtbl.find prims name
      with
        Not_found ->
          raise (Error (Unavailable_primitive name))
    
    
    let patch_int buff pos n =
      String.unsafe_set buff pos (Char.unsafe_chr n);
      String.unsafe_set buff (pos + 1) (Char.unsafe_chr (n asr 8));
      String.unsafe_set buff (pos + 2) (Char.unsafe_chr (n asr 16));
      String.unsafe_set buff (pos + 3) (Char.unsafe_chr (n asr 24))
    
    
    let patch_object buff patchlist = 
      List.iter
        (function
          (Reloc_literal sc, pos) ->
            patch_int buff pos (slot_for_literal sc)
        | (Reloc_getglobal id, pos) ->
            patch_int buff pos (slot_for_getglobal id)
        | (Reloc_setglobal id, pos) ->
            patch_int buff pos (slot_for_setglobal id)
        | (Reloc_primitive name, pos) ->
            patch_int buff pos (num_of_prim name))
      patchlist
    
    let current_state () = ()
    let update_global_table () = ()
    let restore_state old_state = ()
    let hide_additions () = ()
    
    let predef_exn (exn : exn) = My_Obj.field (My_Obj.repr exn) 0
    let _ =
      caml_globals.(0) <- predef_exn Out_of_memory;
      caml_globals.(1) <- predef_exn (Sys_error "");
      caml_globals.(2) <- predef_exn (Failure "");
      caml_globals.(3) <- predef_exn (Invalid_argument "");
      caml_globals.(4) <- predef_exn End_of_file;
      caml_globals.(5) <- predef_exn Division_by_zero;
      caml_globals.(6) <- predef_exn Not_found;
      caml_globals.(7) <- predef_exn (Match_failure ("",0,0));
      caml_globals.(8) <- predef_exn Stack_overflow;
      Hashtbl.add globals "Out_of_memory" 0;
      Hashtbl.add globals "Sys_error" 1;
      Hashtbl.add globals "Failure" 2;
      Hashtbl.add globals "Invalid_argument" 3;
      Hashtbl.add globals "End_of_file" 4;
      Hashtbl.add globals "Division_by_zero" 5;
      Hashtbl.add globals "Not_found" 6;
      Hashtbl.add globals "Match_failure" 7;
      Hashtbl.add globals "Stack_overflow" 8
    
    let init_toplevel () = ()
  end

module Meta = struct
    
    open My_Obj
    open Instructs
    
    
    let unit = repr 0
    
    let inputu str i =
      let pos = i * 4 in
      let b1 = Char.code str.[pos] in
      let b2 = Char.code str.[pos+1] in
      let b3 = Char.code str.[pos+2] in
      let b4 = Char.code str.[pos+3] in
      (b4 lsl 24) + (b3 lsl 16) + (b2 lsl 8) + b1
    
    let inputs str i =
      let pos = i * 4 in
      let b1 = Char.code str.[pos] in
      let b2 = Char.code str.[pos+1] in
      let b3 = Char.code str.[pos+2] in
      let b4 = Char.code str.[pos+3] in
      let b4' = if b4 >= 128 then b4-256 else b4 in
      (b4' lsl 24) + (b3 lsl 16) + (b2 lsl 8) + b1
    
    let d = 0x3fffffff
    let signed v =  (* Only for 64 bits architectures *)
      let sign = if v > d then v - (1 lsl 32) else v in
      sign
    
    let fix_code str str_size =
      let code_size = str_size / 4 in
      let code = Array.create code_size 0 in
      let rec fix i =
        if i < code_size then 
          let op = inputu str i in
          let op = Compat_run.translate op in
          code.(i) <- op;
          match magic op with
(* one arg *)
          | OpPUSHACC | OpACC | OpPOP | OpASSIGN | OpPUSHENVACC | OpENVACC
          | OpAPPLY | OpAPPTERM1 | OpAPPTERM2 | OpAPPTERM3 | OpRETURN | OpGRAB
          | OpPUSHGETGLOBAL | OpGETGLOBAL | OpSETGLOBAL | OpPUSHATOM | OpATOM
          | OpMAKEBLOCK1 | OpMAKEBLOCK2 | OpMAKEBLOCK3 | OpMAKEFLOATBLOCK
          | OpGETFIELD
          | OpGETFLOATFIELD | OpSETFIELD | OpSETFLOATFIELD | OpC_CALL1 | OpC_CALL2 | OpC_CALL3
          | OpC_CALL4
          | OpC_CALL5
            -> 
              let arg = inputu str (i+1) in
              code.(i+1) <- arg;
              fix (i+2)

(* signed arg *)
          | OpPUSH_RETADDR | OpOFFSETCLOSURE | OpPUSHOFFSETCLOSURE | OpBRANCH
          | OpBRANCHIF | OpPUSHTRAP | OpCONSTINT | OpPUSHCONSTINT
          | OpBRANCHIFNOT | OpOFFSETINT | OpOFFSETREF
            ->
              let arg = inputu str (i+1) in
              code.(i+1) <- (signed arg);
              fix (i+2)



(* two args *) 
          | OpAPPTERM | OpPUSHGETGLOBALFIELD | OpGETGLOBALFIELD
          | OpMAKEBLOCK
          | OpC_CALLN
            -> 
              let arg1 = inputu str (i+1) in              
              let arg2 = inputu str (i+2) in
              code.(i+1) <- arg1;
              code.(i+2) <- arg2;
              fix (i+3)

(* two args, second signed *)
          | OpCLOSURE
            -> 
              let arg1 = inputu str (i+1) in              
              let arg2 = inputu str (i+2) in
              code.(i+1) <- arg1;
              code.(i+2) <- (signed arg2);
              fix (i+3)
          
          | OpSWITCH ->
              let sizes = inputu str (i+1) in
              code.(i+1) <- sizes;
              let const_size = sizes land 0xFFFF in
              let block_size = sizes lsr 16 in
              let offset = 2 + const_size + block_size in
              for j = 2 to offset - 1 do
                code.(i + j) <- (inputu str (i + j))
              done;
              fix (i + offset)
          
          |  OpCLOSUREREC ->
              let nfuncs = inputu str (i+1) in
              let nvars = inputu str (i+2) in
              code.(i+1) <- nfuncs;
              code.(i+2) <- nvars;
              let offset = 3 + nfuncs in
              for j = 3 to offset - 1 do
                code.(i + j) <- (signed (inputu str (i + j)))
              done;
              fix (i + offset)
             

(* no args *) 
          | _ -> 
              fix (i+1) 
      
      in 
      fix 0;
      code
    
    open Interp
      
    let reify_bytecode buff code_size name =
      let code = fix_code buff code_size in
      let pc = 0 in
      let accu = repr 0 in
      let env = [||] in
      (*
      let clos = { stack = stack;code = code; env=env; recenv = (0,env) } in 
      let v = interp clos pc accu in
      *)
      let v = interp code pc accu env (0,env) in
  
(*      for i = 0 to opBREAK-1 do
        Printf.printf "%s : %d \n" opnames.(i) instructs.(i)
      done;
      print_newline ();
  *)
      v
  end

module Prims = struct
    open Interp
    
    open My_Obj
      
    let insert_one name f =
      caml_prims.(!next_free_prim) <- My_Obj.magic f;
      prims_call.(!next_free_prim) <- name;
      Hashtbl.add prims name !next_free_prim;
      incr next_free_prim
    
    
    let insert_all () =
      List.iter (fun (name, value) ->
          insert_one name value
      ) Compat_run.Externals.caml_primitives
  
  end
  
