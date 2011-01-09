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

open Asm
open Comb_types
open Comb_alloc

  
let rec compute v dst reg =
  let dest = Register dst in
  match v with
(** OffsetBaseIndexScale **)        
  | C_op (Add, C_const c, C_op (Add, C_reg r1, 
        C_op(Mul, v, C_const (Const_int s)))) ->
      let _ = compute v dst reg in
      let src = OffsetBaseIndexScale(c, physreg r1, dst, s) in
      add_instr reg (mkinstr Leal [src; dest] [])
  | C_op (Add, C_reg r1, C_op(Mul, v, C_const (Const_int s))) ->
      let _ = compute v dst reg in
      let src = OffsetBaseIndexScale(Const_int 0, physreg r1, 
          dst, s) in
      add_instr reg (mkinstr Leal [src;dest] [])
  | C_op (Add, C_const c, C_op (Add, v, 
        C_op(Mul, C_reg r2, C_const (Const_int s)))) ->
      let _ = compute v dst reg in
      let src = OffsetBaseIndexScale(c, dst, physreg r2, s) in
      add_instr reg (mkinstr Leal [src;dest] [])
  | C_op (Add, v, C_op(Mul, C_reg r2, C_const (Const_int s))) ->
      let _ = compute v dst reg in
      let src = OffsetBaseIndexScale(Const_int 0, dst, 
          physreg r2, s) in
      add_instr reg (mkinstr Leal [src;dest] [])
(** OffsetBaseIndex **)          
  | C_op (Add, C_const c, C_op (Add, C_reg r1, v)) ->
      let _ = compute v dst reg in
      let src = OffsetBaseIndex(c, physreg r1, dst) in
      add_instr reg (mkinstr Leal [src;dest] []);
  | C_op (Add, C_const c, C_op (Add, v, C_reg r1)) ->
      let _ = compute v dst reg in
      let src = OffsetBaseIndex(c, physreg r1, dst) in
      add_instr reg (mkinstr Leal [src;dest] []);
  | C_op (Add, C_reg r1, v) ->
      let _ = compute v dst reg in
      let src = OffsetBaseIndex(Const_int 0, 
          physreg r1, dst) in
      add_instr reg (mkinstr Leal [src;dest] []);
  | C_op (Add, v, C_reg r1) ->
      let _ = compute v dst reg in
      let src = OffsetBaseIndex(Const_int 0, 
          physreg r1, dst) in
      add_instr reg (mkinstr Leal [src;dest] []);
(** OffsetIndexScale **)
  | C_op (Add, C_const c, C_op (Mul, v, C_const (Const_int s))) ->
      let _ = compute v dst reg in
      let src = OffsetIndexScale (c, dst, s) in
      add_instr reg (mkinstr Leal [src;dest] []);
  | C_op (Mul, C_reg r1, C_const (Const_int s)) ->
      let c = Const_int 0 in
      let _ = compute v dst reg in
      let src = OffsetIndexScale (c, dst, s) in
      add_instr reg (mkinstr Leal [src;dest] []);
(** OffsetBase **)            
  | C_op (Add, C_const c, v) ->
      let _ = compute v dst reg in
      let src = OffsetBase (c, dst) in
      add_instr reg (mkinstr Leal [src;dest] []);
(** Other operations **)
  | C_op (opcode, v1, v2) ->
      let src = argument_of_value v2 reg in
      add_instr reg (
        mkinstr Movl [src; dest] []);
      let src = argument_of_value v1 reg in
      add_instr reg (mkinstr (opcode_of_operation opcode) [src; dest] []);
  | _ -> assert false

let compute_arg v =
  match v with
  | C_load (C_Float, addr) -> compute_addr addr
  | C_load (C_Long, addr) -> compute_addr addr
  | C_reg r -> Register (physreg r)
  | C_const c -> Const c
  | _ -> assert false

      
let address_of_value map v =
  match v with
    C_op (Add, C_const c, C_reg r) ->
      OffsetBase (c, physreg r)
  | C_op (Add, C_const c, C_op (Add, C_reg r1, C_reg r2)) ->
      OffsetBaseIndex (c, physreg r1, physreg r2)
  | C_op (Add, C_const c, C_op (Add, C_reg r1,
        C_op (Mul, C_reg r2, C_const (Const_int s)))) ->
      OffsetBaseIndexScale (c, physreg r1, physreg r2,s)
  | C_op (Add, C_const c, 
      C_op (Mul, C_reg r1, C_const (Const_int s))) ->
      OffsetIndexScale (c, physreg r1,s)
  | C_reg r -> OffsetBase (Const_int 0, physreg r)
  | C_const c -> ConstantBase c
  | C_stack n -> OffsetBase (Const_int n, Esp)
  | _ -> 
      Printf.printf "Address of value %s" (string_of_value v);
      print_newline ();
      assert false

  
let translate_regs state pseudo_regs =
    
  try
    
    allocs := [];
    new_instrs := [];
    for i = 0 to 6 do death.(i) <- 0; done;
    
    list_iter (fun reg ->
        let i = reg.reg_num in
        if i<7 then begin
            reg.map <- i;
            allocs := (i, reg) :: !allocs;
            death.(i) <- reg.used_by_value;
            if i = 6 then begin
                (*** If some of the registers are not in their final
                location, and the final location is free, try to
                move them immediatly. *)
                for i = 0 to 6 do
                  let r = init_regs.(i) in
                  if r.used_by_value = 1000 then
                    match r.prefered_reg with
                      Some j when i<>r.map && death.(j) = 0 ->
                        add_instr_end (mkinstr Movl [
                            Register regs.(r.map); Register regs.(j)] []);
                        death.(r.map) <- 0;
                        r.map <- j;
                        death.(j) <- r.used_by_value;
                        allocs := (j,r) :: !allocs;
                    | _ -> ()
                done
              end
          end else
        if reg.used_by_value = 0 then () else 
        let _ = () in
        printf "TRANSLATE r%d = %s" i (string_of_value reg.reg_value);
        print_newline ();      
        match reg.reg_value with
          C_store (part, src, dst) ->
            let dst = address_of_value map dst in
            let src = argument_of_value src reg in
            add_instr reg (mkinstr (match part with 
                | C_LowByte -> Movb
                | C_LowWord -> Movw
                | C_Long ->    Movl
                | _ -> assert false) [src;dst] []);
        | C_const c -> 
            let dst = alloc_reg reg in
            add_instr reg (mkinstr Movl [Const c; Register dst] []);
        | C_load (part, v) ->
            let dst = alloc_reg reg in
            let src = address_of_value map v in
            add_instr reg (mkinstr (match part with 
                  C_Long -> Movl
                | C_SignBL -> Movsbl
                | C_SignWL -> Movswl
                | C_ZeroWL -> Movzwl
                | C_ZeroBL -> Movzbl
                | _ -> assert false) [src; Register dst] []);
        | C_cast (cast, C_reg r) -> 
            let dst = alloc_reg reg in
            let src = regs.(r.map) in
            add_instr reg (mkinstr Movzbl [
                Register 
                  (match cast with
                  | C_LowByte -> LowByte src
                  | C_LowWord -> LowWord src
                  | C_HighByte -> HighByte src
                  | _ -> assert false
                  );
                Register dst] [])
        | C_stack n ->
            let dst = alloc_reg reg in
            add_instr reg (mkinstr Movl [OffsetBase(Const_int n, Esp); Register dst] []);
        | C_reg r ->
            if death_at r = i then begin
                reg.map <- r.map;
                death.(r.map) <- reg.used_by_value;
                allocs := (r.map, reg) :: !allocs;
              end else
            let dst = alloc_reg reg in
            add_instr reg (mkinstr Movl [Register (physreg r); Register dst] []);
        | C_op (Add, C_const (Const_int 1), C_reg r) when  death_at r = i -> 
            let dst = reuse_for r reg in
            add_instr reg (mkinstr Incl [dst] []);
        | C_op (Sub, C_const (Const_int 1), C_reg r) when  death_at r = i -> 
            let dst = reuse_for r reg in
            add_instr reg (mkinstr Decl [dst] []);
        | C_op (op, v1, C_reg r) when death_at r = i -> 
            force_reg_map reg true;
            let dst = reuse_for r reg in
            let src = argument_of_value v1 reg in
            (************
            In some cases, we may have to force a register in a
            given location. It can be done before the op, if the register
            is needed for the op, or after the op, if the register
            concerned is the result...
            *******)
            let instr = mkinstr (opcode_of_operation op) [src;dst] [] in
            add_instr reg instr;
            force_reg_map reg false;
        | C_op (Add, C_const c, C_reg r1) ->
            let dst = alloc_reg reg in
            let src = OffsetBase(c, physreg r1) in
            add_instr reg (mkinstr Leal [src; Register dst] []);
        | C_op (Add, C_const c, C_op (Add, C_reg r1, C_reg r2)) ->
            let dst = alloc_reg reg in
            let src = OffsetBaseIndex(c, physreg r1, physreg r2) in
            add_instr reg (mkinstr Leal [src;Register dst] []);
        | C_op (Add, C_const c, C_op (Add, C_reg r1, 
              C_op (Mul, C_reg r2, C_const (Const_int s)))) ->
            let dst = alloc_reg reg in
            let src = OffsetBaseIndexScale (c, physreg r1, physreg r2, s)
            in
            add_instr reg (mkinstr Leal [src;Register dst] []);
        | C_op (Add, C_const c, C_op (Mul, C_reg r1, C_const (Const_int s))) ->
            let dst = alloc_reg reg in
            let src = OffsetIndexScale (c, physreg r1, s) in
            add_instr reg (mkinstr Leal [src;Register dst] []);
        | C_op (Add, C_reg r1, C_reg r2) ->
            let c = Const_int 0 in
            let dst = alloc_reg reg in
            let src = OffsetBaseIndex(c, physreg r1, physreg r2) in
            add_instr reg (mkinstr Leal [src;Register dst] []);
        | C_op (Add, C_reg r1, C_op (Mul, C_reg r2, C_const (Const_int s))) ->
            let c = Const_int 0 in
            let dst = alloc_reg reg in
            let src = OffsetBaseIndexScale (c, physreg r1, physreg r2, s)
            in
            add_instr reg (mkinstr Leal [src;Register dst] []);
        | C_op (Mul, C_reg r1, C_const (Const_int s)) ->
            let c = Const_int 0 in
            let dst = alloc_reg reg in
            let src = OffsetIndexScale (c, physreg r1, s) in
            add_instr reg (mkinstr Leal [src;Register dst] []);      
        | (C_op (opcode, v1, C_reg r)) as v-> 
            let dst = alloc_reg reg in
            compute v dst reg
        | (C_op (opcode, v1, v2)) as v-> 
            assert false;
          (*** we cannot compute this expression in a single step. 
          Try to find the fastest way to compute it, by using an 
          intermediate register.
        ***)
            let dst = alloc_reg_off reg in
            compute v dst reg
        | C_unknown (opcode, args, []) ->
            add_instr reg (mkinstr opcode args [])
        | C_unknown (opcode, args, values) ->
            add_instr reg (mkinstr opcode 
                (args@(List.map compute_arg values)) [])
        | _ -> 
            assert false) pseudo_regs;

    (*** Don't forget to move registers to their good location at the end ***)
    
    let liveregs = Array.init 7 (fun i ->
          if state.new_liveness.(i) <> [] then 
            state.regs.(i).map
          else -1) in
    
    let rec iter n =
      let lives = ref [] in
      let sources = Array.create 7 0 in
      for i = 0 to 6 do
        if liveregs.(i) <> -1 then
          sources.(liveregs.(i)) <- sources.(liveregs.(i)) + 1
      done;
      
      for i = 0 to 6 do
        let src = liveregs.(i) in
        if src = i then begin
            printf "%s is OK" (string_of_register regs.(i));
            print_newline ();
          end else 
        if src = -1 then begin
            printf "%s is dead" (string_of_register regs.(i));
            print_newline ();            
          end else
        if sources.(i) = 0 then begin
            printf "%s is moved" (string_of_register regs.(i)); 
            print_newline ();
            add_instr_end
              (mkinstr Movl [Register regs.(src); Register regs.(i)] []);
            liveregs.(i) <- i;
            sources.(src) <- sources.(src) - 1;
          end else
          lives := i :: !lives
      done;
      
      match !lives with
        [] -> ()
      | i :: _ -> 
          if sources.(i) = 0 then iter () else
          if sources.(i) = 1 then begin
              for j = 0 to 6 do
                if liveregs.(j) <> -1 && liveregs.(j) <> j
                then begin
                    printf "%s is stored in %s"
                      (string_of_register regs.(j))
                    (string_of_register regs.(liveregs.(j)));
                    print_newline ();
                  end;
              done;
              try
                (* try to find a register which is neither a source
              nor a destination. *)
                for dst = 0 to 6 do
                  if liveregs.(dst) = -1 && sources.(dst) = 0 then begin
                      add_instr_end
                        (mkinstr Movl [
                          Register regs.(liveregs.(i)); 
                          Register regs.(dst)] []);
                      sources.(liveregs.(i)) <- sources.(liveregs.(i)) - 1;
                      liveregs.(i) <- dst;
                      sources.(dst) <- 1;
                      raise Exit
                    end;
                done;
                (** We can use xchg for this, but it must be implemented
                first ... *)
                
                failwith (Printf.sprintf "Bad stored for %s in %s!"
                    (string_of_register regs.(i))
                  (string_of_register regs.(liveregs.(i)));
                )
              with Exit -> iter ()              
            end else begin
              printf "Continue normally"; print_newline ();
              iter ()
            end
    in
    iter ();
    
    let instrs = List.rev !new_instrs in
    instrs
    
  with
    e ->
      printf "Exception %s" (Printexc.to_string e);
      print_newline ();
      List.rev state.old_instrs


