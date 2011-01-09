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

(***

When it works, we must simplify it, by using something similar to what
Fraser and al. used in po, ie multiple passes:
- in the first pass, simplify every thing that can be by combining simple
operations.
- in the second pass, apply strengh reduction.
- in the third pass, new simplification (if some strenght reduction was
  possible only).
- in the fourth pass, compute liveness.
- in the fifth pass, combine operations as close as possible to the
instruction set possibilities.
- in the sixth pass, translate to machine code.

  ***)

(****

Using a good structure is important: Until now, we have used an array
of pseudo-registers that cannot be extended by insertion. This is clearly
a bad approach, since the "distribute" pass creates values which are
not computable in a single step. Thus, the combine pass is unable to
improve these values.

***)

open Asm
open Comb_types

let combine state = 
  printf "Combine executed on %d ins" state.ninstrs; print_newline ();
  
  let pseudo_regs = List.rev state.pseudo_regs in
  let first_instr = 
    let rec iter = function [i] -> i
      | [] -> assert false
      | i :: tail -> iter tail
    in iter state.old_instrs
  in
  let enter_state = first_instr.enter_instr in
    
  (* correct the mapping of init_regs ... *)
  for i = 0 to 6 do init_regs.(i).map <- i; done;
  
  List.iter (fun r ->
      r.used_by_value <- 0;
      r.used_by_addr <- 0) pseudo_regs;    
  
  print_rtl "BEFORE OPT" pseudo_regs;
  
  Comb_opt.propagate_loads pseudo_regs;
  
  Comb_opt.init_first_registers enter_state pseudo_regs;
  
  Comb_opt.simplify_regs pseudo_regs;
  
  print_rtl "AFTER SIMPLIFICATION" pseudo_regs;

  (**** distribute ****)
  
  Comb_opt.distribute_regs pseudo_regs;
  
  print_rtl "AFTER DISTRIBUTION" pseudo_regs;

  (**** strengh reduction ****)
  let pseudo_regs = Comb_opt.strenght_reduction  pseudo_regs in
  
  print_rtl "AFTER STRENGHT REDUCTION" pseudo_regs;
  
  (*****  liveness ****)
  
  Comb_alloc.liveness_regs state pseudo_regs;
  
  print_rtl "AFTER LIVENESS" pseudo_regs;
  
  Comb_alloc.combine_regs pseudo_regs;
  
  print_rtl "AFTER COMBINE" pseudo_regs;
  
  let instrs = Comb_transl.translate_regs state pseudo_regs in
  
  let len1 = List.length state.old_instrs in
  let len2 = List.length instrs in
  begin
    printf "COMBINE IN %s INSTRUCTIONS: %d/%d" 
      (if len1 < len2 then "MORE" else
      if len1 = len2 then "SAME" else
        "LESS") len2 len1;
    print_newline (); 
    printf "BEFORE:";
    print_newline ();
    let instrs1 = Array.of_list (List.rev state.old_instrs) in
    for i = 0 to Array.length instrs1 - 1 do
      print_instr instrs1.(i);
    done;
    printf "AFTER:";
    print_newline ();
    let instrs2 = Array.of_list instrs in
    for i = 0 to Array.length instrs2 - 1 do
      print_instr instrs2.(i);
    done;
    for i = 0 to 6 do
      if state.new_liveness.(i) <> [] then
        (printf "%s live at end" (string_of_register regs.(i));
          print_newline ());
    done;
  
  
  end;
  
  printf "Combine done"; print_newline ();
    (* instrs *)
  instrs
  
  
let register state r = 
  let v = C_reg state.regs.(regindex_all r) in
  match r with
    LowByte _ -> C_cast (C_LowByte, v)
  | LowWord _ -> C_cast (C_LowWord, v)
  | HighByte _ -> C_cast (C_HighByte, v)
  | _ -> v

let new_pseudo_reg state = 
  let num = state.next_reg in
  let rec reg = {
      reg_num = num; 
      reg_value = C_reg reg; 
      reg_used = [];
      used_by_addr = 0; 
      used_by_value = 0; 
      prefered_reg = None;
      force_reg = [];
      map = -1;
      nused = 0;
      tail = [];
    } in
  let new_state = { state with
      pseudo_regs = reg :: state.pseudo_regs;
      next_reg = state.next_reg + 1 } in
  reg, new_state
  
let new_reg state r =
  let num = state.next_reg in
  let r = regindex r in
  let rec reg = {
      reg_num = num; 
      reg_value = C_reg reg; 
      reg_used = [];
      used_by_addr = 0; 
      used_by_value = 0; 
      prefered_reg = None;
      force_reg = [];
      map = -1;
      nused = 0;
      tail = [];
    } in
  let new_state = { state with
      pseudo_regs = reg :: state.pseudo_regs;
      regs = Array.copy state.regs;
      next_reg = state.next_reg + 1 } in
  new_state.regs.(r) <- reg;
  reg, new_state
  
let set_pseudo_reg reg v = reg.reg_value <- v
  
let source src size state =
  match src with
    Register r -> register state r, state
  | Const c -> C_const c, state
  | ConstantBase c -> C_load (size, C_const c), state
  | OffsetBase (Const_int n, Esp) -> 
      C_load (size, C_stack n), state
  | OffsetBase (c, r) ->
      let r1, state = new_pseudo_reg state in
      set_pseudo_reg r1 (C_op (Add, C_const c, register state r));
      C_load (size, C_reg r1), state
  | OffsetBaseIndex (c,r1,r2) ->
      let ps1, state = new_pseudo_reg state in
      set_pseudo_reg ps1 (C_op (Add, C_const c, 
          C_op (Add, register state r1, register state r2)));
      C_load (size, C_reg ps1), state
  | OffsetBaseIndexScale (c,r1,r2,s) ->
      let ps1, state = new_pseudo_reg state in
      set_pseudo_reg ps1 (C_op (Add, C_const c, 
          C_op (Add, register state r1, 
            (C_op (Mul, register state r2, C_const (Const_int s))))));
      C_load (size, C_reg ps1), state
  | OffsetIndexScale (c, r1, s) ->
      let ps1, state = new_pseudo_reg state in
      set_pseudo_reg ps1 (C_op (Add, C_const c, 
          (C_op (Mul, register state r1, C_const (Const_int s)))));
      C_load (size, C_reg ps1), state
  | _ -> assert false
  
let source_rec src size state =
  let v, new_state = source src size state in
  match v with
    C_load _ ->
      let r, new_state = new_pseudo_reg new_state in
      set_pseudo_reg r v;
      C_reg r, new_state
  | _ -> v, new_state

let fsource src state =
  let addr =
    match src with
    | ConstantBase c -> C_const c
    | OffsetBase (Const_int n, Esp) -> C_stack n
    | OffsetBase (c, r) -> C_op (Add, C_const c, register state r)
    | OffsetBaseIndex (c,r1,r2) ->
        C_op (Add, C_const c, 
          C_op (Add, register state r1, register state r2))
    | OffsetBaseIndexScale (c,r1,r2,s) ->
        C_op (Add, C_const c, 
          C_op (Add, register state r1, 
            (C_op (Mul, register state r2, C_const (Const_int s)))))
  | OffsetIndexScale (c, r1, s) ->
        C_op (Add, C_const c, 
          (C_op (Mul, register state r1, C_const (Const_int s))))
  | _ -> assert false
  in
  C_load (C_Float, addr)
  
let rec combine_instrs instrs state =
  if not (state == init_state) then begin
      printf "Reg map: ";
      for i = 0 to 6 do
        printf "%s=r%d " (string_of_register regs.(i)) 
        state.regs.(i).reg_num
      done;
      print_newline ();      
    end;    
  match instrs with
    [] ->           
      if state == init_state then [] else 
      if state.ninstrs < min_combine_instrs then
        List.rev state.old_instrs 
      else combine state
  | instr :: instrs -> 
      printf "Combine_instr:";
      print_instr instr;
      if instr.leave_instr == noapprox then
        (printf "napprox"; print_newline ())
      else
      if instr.leave_instr.liveness = [||] then
        (printf "noliveness"; print_newline ());
      let new_state = { state with 
          old_instrs = instr :: state.old_instrs;
          ninstrs = state.ninstrs + 1;
          new_liveness = instr.leave_instr.liveness } in      
      match instr with
      | { opcode = Movl; args = [Register src; Register dst] } ->
          let new_state = { new_state with
              regs = Array.copy new_state.regs } in
          new_state.regs.(regindex dst) <- new_state.regs.(regindex src);
          combine_instrs instrs new_state
      | { opcode = Movl; args = [src; Register r] } ->
          let v, new_state = source src C_Long new_state in
          let ps, new_state = new_reg new_state r in
          set_pseudo_reg ps v; 
          combine_instrs instrs new_state
      | { opcode = (Leal|Lea); args = [src; Register r] } ->
          let v, new_state = source src C_Long new_state in
          let ps, new_state = new_reg new_state r in
          set_pseudo_reg ps (match v with
              C_load (_,v) -> v | _ -> assert false); 
          combine_instrs instrs new_state
      | { opcode = Movzbl; args = [src; Register r] } ->
          let v, new_state = source src C_ZeroBL new_state in
          let ps, new_state = new_reg new_state r in
          set_pseudo_reg ps v; 
          combine_instrs instrs new_state
      | { opcode = Movzwl; args = [src; Register r] } ->
          let v, new_state = source src C_ZeroWL new_state in
          let ps, new_state = new_reg new_state r in
          set_pseudo_reg ps v; 
          combine_instrs instrs new_state
      | { opcode = Movsbl; args = [src; Register r] } ->
          let v, new_state = source src C_SignBL new_state in
          let ps, new_state = new_reg new_state r in
          set_pseudo_reg ps v;
          combine_instrs instrs new_state
      | { opcode = Movswl; args = [src; Register r] } ->
          let v, new_state = source src C_SignWL new_state in
          let ps, new_state = new_reg new_state r in
          set_pseudo_reg ps v; 
          combine_instrs instrs new_state
      | { opcode = (Movw| Movb); args = [src;Register _ ] } ->
          cannot_combine instr instrs state
      | { opcode = (Movl| Movw | Movb); args = [src;dst] } ->
          let part = match instr.opcode with
              Movl -> C_Long
            | Movw -> C_LowWord
            | Movb -> C_LowByte
            | _ -> assert false
          in
          let src, new_state = source src part new_state in          
          let dst, new_state = source dst part new_state in
          let ps, new_state = new_pseudo_reg new_state in
          set_pseudo_reg ps (match dst with
              C_load (_, dst) -> C_store (part, src, dst)
            | _ -> 
                print_instr instr; print_newline ();
                assert false);
          combine_instrs instrs new_state
      | { opcode =  (
            Shrl | Addl | Andl | Xorl | Orl | Subl |
            Sarl | Sall | Imull | Idivl
          );
          args = [src; (Register r) as dst] } when r <> Esp ->
          let v1, new_state = source_rec src C_Long new_state in
          let v2, new_state = source dst C_Long new_state in
          let ps, new_state = new_reg new_state r in
          let operation = match instr.opcode with
              Shrl -> Shr
            | Addl -> Add
            | Subl -> Sub
            | Andl -> And
            | Xorl -> Xor
            | Orl -> Or
            | Sarl -> Sar
            | Sall -> Sal
            | Imull -> Mul
            | Idivl -> Div
            | _ -> assert false
          in
          set_pseudo_reg ps (C_op (operation, v1, v2));
          combine_instrs instrs new_state
      | { opcode = Incl; args = [(Register r) as dst] } ->
          let v, new_state = source dst C_Long new_state in
          let ps, new_state = new_reg new_state r in
          set_pseudo_reg ps (C_op (Add, C_const (Const_int 1), v));
          combine_instrs instrs new_state
      | { opcode = Decl; args = [(Register r) as dst] } ->
          let v, new_state = source dst C_Long  new_state in
          let ps, new_state = new_reg new_state r in
          set_pseudo_reg ps (C_op (Sub, C_const (Const_int 1), v));
          combine_instrs instrs new_state
      | { opcode = Cmpl; args = [src1;src2] } ->
          let v1, new_state = source_rec src1 C_Long new_state in
          let v2, new_state = source_rec src2 C_Long new_state in
          let ps, new_state = new_pseudo_reg new_state in
          set_pseudo_reg ps (C_unknown (Cmpl, [], [v1;v2]));
          combine_instrs instrs new_state
      | { opcode = CheckBound comp } ->
          let ps, new_state = new_pseudo_reg new_state in
          set_pseudo_reg ps (C_unknown (instr.opcode, [], []));
          combine_instrs instrs new_state

          (*** Floating point operations ***)
      | { opcode = (Fldl|Fstpl); args = [addr] } ->
          let ps, new_state = new_pseudo_reg new_state in
          set_pseudo_reg ps (C_unknown (instr.opcode, [], 
              [fsource addr new_state]));
          combine_instrs instrs new_state
      | { opcode = Fldz; args = [] } -> 
          let ps, new_state = new_pseudo_reg new_state in
          set_pseudo_reg ps (C_unknown (instr.opcode, [], []));
          combine_instrs instrs new_state
      | { opcode = (Faddp| Fsubp); 
          args = [Register (St _); Register (St _)] } ->
          let ps, new_state = new_pseudo_reg new_state in
          set_pseudo_reg ps (C_unknown (instr.opcode, instr.args, []));
          combine_instrs instrs new_state
          
          (*
      | { opcode = Cltd } ->
          combine_instrs instrs new_state
      | { opcode = Idivl; args = [(Register r) as src] } ->
          (*** r/eax -> edx : eax ***)
          let divisor, new_state = source src C_Long new_state in
          let dividend = C_reg new_state.regs.(eax) in
          let ps1, new_state = new_reg new_state Eax in
          let ps2, new_state = new_reg new_state Edx in
          set_pseudo_reg ps1 (C_op (Div, dividend, divisor));
          set_pseudo_reg ps2 (C_op (Mod, dividend, divisor));
          combine_instrs instrs new_state
          *)
      | _ -> cannot_combine instr instrs state 
          
and cannot_combine instr instrs state =
  printf "CANNOT COMBINE"; print_newline ();
  if state == init_state then
    instr :: (combine_instrs instrs state)
  else 
  if state.ninstrs < min_combine_instrs then
    (List.rev state.old_instrs) @ (instr :: (
        combine_instrs instrs init_state))
  else begin
      printf "Combine on %d ins ended with %s"
        state.ninstrs (SimplePrint.string_of_instr instr);
      print_newline ();
      let new_instrs = combine state in
      new_instrs @ (instr :: (
          combine_instrs instrs init_state))
    end
    
let combine_node node =
  try
    printf "COMBINE NODE %d" node.node_ident.label;
    print_newline ();
    let instrs1 = node.instrs in
    let len1 = Array.length instrs1 in
    if len1 < min_combine_instrs then () else
    let instrs2 = Array.of_list (
        combine_instrs (Array.to_list instrs1) init_state) in
    let len2 = Array.length instrs2 in
    if len1 < len2 then begin
        printf "node %d: MORE INSTRUCTIONS: %d/%d" node.node_ident.label
          len2 len1;
        print_newline ();
        printf "BEFORE:";
        print_newline ();
        for i = 0 to Array.length instrs1 - 1 do
          print_instr instrs1.(i);
        done;
        printf "AFTER:";
        print_newline ();
        for i = 0 to Array.length instrs2 - 1 do
          print_instr instrs2.(i);
        done;
      end else
    if len2 < len1 then begin
        printf "node %d: FEWER INSTRUCTIONS: %d/%d" node.node_ident.label
        len2 len1;
        print_newline ();        
        printf "BEFORE:";
        print_newline ();
        for i = 0 to Array.length instrs1 - 1 do
          print_instr instrs1.(i);
        done;
        printf "AFTER:";
        print_newline ();
        for i = 0 to Array.length instrs2 - 1 do
          print_instr instrs2.(i);
        done;
        node.instrs <- instrs2;
        stat_combine_ins := !stat_combine_ins + len1 - len2;
      end;
    ()
  with e ->
      printf "In combine node %d: %s" 
        node.node_ident.label (Printexc.to_string e);
      print_newline ()
      
let combine_func func =
  (*
  printf "Code before combine:";
  print_newline ();
  print_func func;
*)
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes combine_node;

  (*
  printf "Code after combine:";
  print_newline ();
  print_func func;
*)

  (*
              with e ->
                printf "In combine %s: %s" (
                  SimplePrint.string_of_instr instr)
                (Printexc.to_string e);
                print_newline ();
                raise e;
          in
  *)
  