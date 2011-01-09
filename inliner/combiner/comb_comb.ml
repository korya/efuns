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
  
  print_rtl pseudo_regs;

  (*** propagate stack loads ***)
  let rec iter list = match list with
      [] -> ()
    | r1 :: tail ->
        iter tail;
        match r1.reg_value with
          C_store (C_Long, C_reg r2, C_stack n) ->
            subst_all tail (C_load (C_Long, C_stack n)) r2
        | C_load (C_Long, C_stack n) ->
            subst_all tail (C_load (C_Long, C_stack n)) r1
        | C_load (cast, addr) ->
            subst_no_alias tail (C_load (cast, addr)) r1
        | C_store (cast, C_reg r2, addr) ->
            subst_no_alias tail (C_load (cast, addr)) r2
        | _ -> ()
  in
  iter pseudo_regs;
  
  let values = Hashtbl.create 31 in

  (* tout le probleme est que l'on ne peut pas prendre des valeurs dans
  des registres si ces registres sont morts, car d'autres passes peuvent
  avoir reutilise ces registres. *)
  
  let len = Array.length enter_state.liveness in
  for i = 0 to enter_state.stack_size - 1 do
    if enter_state.liveness.(len-1-i) <> [] then
      Hashtbl.add values enter_state.stack.(i) 
      (C_load (C_Long, C_stack (i*4)));
  done;
  (* Is one of the initial registers a copy of a stack location ? If yes,
  use this copy instead of a load.
  *)
  for i = 0 to 6 do
    if enter_state.liveness.(i) <> [] then
      try
        let v = Hashtbl.find values enter_state.registers.(i) in
        subst_all pseudo_regs v init_regs.(i)
      with Not_found -> ()
  done;
  
  Hashtbl.clear values;
  (* try to express init register values as operations on other registers *)
  for i = 0 to 6 do
    if enter_state.liveness.(i) <> [] then
      try
        let r = Hashtbl.find values enter_state.registers.(i) in
        init_regs.(i).reg_value <- r
      with Not_found ->
          init_regs.(i).reg_value <- C_param i;
          Hashtbl.add values enter_state.registers.(i) (C_reg init_regs.(i))
  done;
  
  for i = 0 to 6 do
    if enter_state.liveness.(i) <> [] then
      let v = enter_state.registers.(i) in
      Hashtbl.remove values v;
      (try
          init_regs.(i).reg_value <- 
            convert_approx values enter_state.registers.(i)
        with _ -> ());
      Hashtbl.add values v (C_reg init_regs.(i))
  done;
  
  
  (*** propagate heap loads/stores ***)
  let rec iter list = match list with
      [] -> 
        list_iter (fun r -> r.reg_value <- simplify r.reg_value) pseudo_regs
    | r1 :: tail ->
        (match r1.reg_value with
          | C_unknown (Cmpl, [], args) ->
              remove_checkbound tail args
          | C_load (cast, addr) ->
              subst_no_alias tail (C_load (cast, addr)) r1
          | C_store (cast, C_reg r2, addr) ->
              subst_no_alias tail (C_load (cast, addr)) r2
          | C_load _
          | C_store _
          | C_unknown _
          | C_reg _
          | C_const _
            -> ()
          | v -> subst_all tail v r1
        );
        iter tail;
  in
  subst_done := true;
  list_iter (fun r -> r.reg_value <- simplify r.reg_value) pseudo_regs;
  while !subst_done do
    subst_done := false;
    printf "BEFORE SUBST"; print_newline ();
    print_rtl pseudo_regs;
    iter pseudo_regs; (* do it twice, just because the block is small *)
  done;

  (**** simplify ****)
  
  printf "AFTER SIMPLIFICATION";
  print_newline ();
  print_rtl pseudo_regs;

  (**** distribute ****)
  list_iter (fun r -> 
      r.reg_value <- 
        simplify (distribute r.reg_value)) pseudo_regs;
  
  printf "AFTER DISTRIBUTION";
  print_newline ();
  print_rtl pseudo_regs;

  (**** strengh reduction ****)
  
  let rec iter list =
    match list with
      [] -> ()
    | r :: tail ->
        match r.reg_value with
          C_op (Add, C_const c1, v1) ->
            list_iter (fun r2 ->
                match r2.reg_value with
                  C_op (Add, C_const c2, v2) when v1 = v2 ->
                    r2.reg_value <- C_op (
                      Add, C_const (Const_sub (c2, c1)), C_reg r)
                | v2 -> if v2 = v1 then
                      r2.reg_value <- C_op (
                        Add, C_const (Const_sub (Const_int 0, c1)), C_reg r)
            ) tail;
        | C_op _ as v1 ->
            list_iter (fun r2 ->
                match r2.reg_value with
                  C_op (Add, C_const c2, v2) when v1 = v2 ->
                    r2.reg_value <- C_op (
                      Add, C_const c2, C_reg r)
                | v2 -> if v2 = v1 then r2.reg_value <- C_reg r
            ) tail;
            iter tail
        | _ -> iter tail
  in
  iter pseudo_regs;
  
  
  let pseudo_regs = List.fold_right (fun r pseudo_regs ->
        cut r pseudo_regs
    ) pseudo_regs [] in

  (**** new simplification ****)  
  list_iter (fun r -> 
      r.reg_value <- simplify r.reg_value) pseudo_regs;
  
  printf "AFTER STRENGHT REDUCTION";
  print_newline ();  
  print_rtl pseudo_regs;
  
  (*****  liveness ****)
  
  let num = ref 0 in
  list_iter (fun r ->
      let i = !num in
      incr num;
      r.reg_num <- i;
      r.initial_map <- [];
  ) pseudo_regs;
  
  let rec force_alloc i r =
    match r.initial_map with
      _ :: _ -> 
        () 
        (* Can't force a register to go into another one *)
    | _ -> 
        printf "Force alloc r%d in %s" r.reg_num 
          (string_of_register regs.(i));
        print_newline ();
        r.initial_map <- [i];
        match r.reg_value with
          C_op (_, C_const _, C_reg r) ->
            force_alloc i r
        | _ -> ()
  in
  
  (try
      list_iter (fun r ->
          let i = r.reg_num in
          if i = 7 then 
            r.initial_map <- [i]
          else raise Exit) pseudo_regs
    with _ -> ());
  
  (try
      list_iter (fun r ->
          let i = r.reg_num in
          if i = 7 then begin
              if state.new_liveness.(i) <> [] then begin
                  let rec original r =
                    match r.reg_value with
                      C_reg r -> original r
                    | _ -> r
                  in
                  let r = original state.regs.(i) in
                  state.regs.(i) <- r;
                  r.used_by_value <- 10000;
                  printf "%s live at exit in r%d"
                    (string_of_register regs.(i)) r.reg_num;
              (*** We MUST force the register to be used for this value as
                soon as it is computed ... *)
                  force_alloc i r;                
                  print_newline ();
                end
            end else begin
              (* We may also have to force some registers for some
            operations. *)
              match r.reg_value with
                C_op (Sal, C_cast (_, C_reg r1) , _ ) ->
                (* r1 must be in ecx *)
                  force_alloc 2 (* %ecx *) r1
              | _ -> ()
            end
      ) pseudo_regs;
    with _ -> ());
  
  list_riter (fun r ->
      let i = r.reg_num in
      if r.used_by_value > 0 then
        iter_used_by_value r.reg_value i
      else if r.used_by_addr > 0 then
        match r.reg_value with
          C_op (Add, C_const _, C_reg _) -> 
            iter_used_by_addr r.reg_value i
        | C_op (Add, C_const _, C_op (Add, C_reg _, C_reg _)) -> 
            iter_used_by_value r.reg_value i
        | C_op (Add, C_const _, C_op (Add, C_reg _, 
              C_op (Mul, C_reg _, C_const _))) -> 
            iter_used_by_value r.reg_value i
        | C_op (Add, C_const _, C_op (Mul, C_reg _, C_const _)) -> 
            iter_used_by_value r.reg_value i
        | C_op (Add, C_reg _, C_reg _) -> 
            iter_used_by_value r.reg_value i
        | C_op (Add, C_reg _, C_op (Mul, C_reg _, C_const _)) -> 
            iter_used_by_value r.reg_value i
        | C_op (Mul, C_reg _, C_const _) -> 
            iter_used_by_value r.reg_value i
        | _ -> 
            r.used_by_value <- r.used_by_addr;
            iter_used_by_value r.reg_value i
      else
      match r.reg_value with
        C_store(part, src, dst) ->
          iter_used_by_value src i;
          iter_used_by_addr dst i;
          r.used_by_value <- i;        
      | C_unknown (opcode, args, values) ->
          r.used_by_value <- i;
          List.iter (fun v ->
              match v with
                C_load (C_Float, addr) -> iter_used_by_addr addr i
              | _ -> iter_used_by_value v i
          ) values
      | _ -> ()
  ) pseudo_regs;
  
  printf "AFTER LIVENESS";
  print_newline ();  
  print_rtl pseudo_regs;
  
  list_iter (fun r ->
      let i = r.reg_num in
      if r.used_by_value + r.used_by_addr> 0 then
        match r.reg_value with
          C_store (part, src, C_reg addr) ->
            r.reg_value <- 
              C_store (part, src, find_addr addr (Const_int 0) i);
        | C_load (part, C_reg addr) ->
            r.reg_value <- 
              C_load (part, find_addr addr (Const_int 0) i)
        | _ -> ()
  ) pseudo_regs;
  
  let add_used v =
    match v with
      C_reg r -> r.nused <- r.nused + 1
    | _ -> ()
  in
  list_iter_tail (fun r ->
      r.nused <- 0;
      if r.used_by_value > 0 then begin
          value_iter add_used r.reg_value;
          match r.reg_value with
            C_op (opcode, 
              C_reg ({ reg_value = C_load (C_Long,C_stack n) } as r1), 
              C_reg r2) ->
              if 
                r2.used_by_value = r.reg_num &&
                r1.used_by_value = r.reg_num &&
                r1.nused = 1 then
                begin
                  try
                    list_iter (fun rr -> 
                        if r==rr then raise Not_found else
                        match rr.reg_value with
                          C_store (_,_, C_stack nn) when n=nn -> raise Exit
                        | _ -> ()) r1.tail;
                    assert false
                  with
                  | Not_found ->
                      r1.used_by_value <- 0;
                      r.reg_value <- 
                        C_op (opcode, C_load (C_Long, C_stack n), C_reg r2)
                  | Exit -> ()
                end
                (******** We can't do that, unless we are sure there is no
                store between r1 and now ... *)
          | C_op (opcode, 
              C_reg ({ reg_value = (C_load _) as v } as r1), 
              C_reg r2) when no_store r1 r ->
              if 
                r2.used_by_value = r.reg_num &&
                r1.used_by_value = r.reg_num &&
                r1.nused = 1 then
                begin
                  r1.used_by_value <- 0;
                  r.reg_value <- 
                    C_op (opcode, v, C_reg r2)
                end
          | C_unknown (Cmpl, [], [
                C_reg ({ 
                    reg_value = (C_load _) as v } as r1);
                C_reg r2]) when no_store r1 r ->
              if 
                r2.used_by_value = r.reg_num &&
                r1.used_by_value = r.reg_num &&
                r1.nused = 1 then
                begin
                  r1.used_by_value <- 0;
                  r.reg_value <- 
                    C_unknown (Cmpl, [], [v; C_reg r2])
                end
          | C_unknown (Cmpl, [], [
                C_reg r2;
                C_reg ({ 
                    reg_value = (C_load _) as v } as r1);
                ]) when no_store r1 r ->
              if 
                r2.used_by_value = r.reg_num &&
                r1.used_by_value = r.reg_num &&
                r1.nused = 1 then
                begin
                  r1.used_by_value <- 0;
                  r.reg_value <- 
                    C_unknown (Cmpl, [], [C_reg r2;v])
                end
          |  _ -> ()
        end;
  ) pseudo_regs;
  
  printf "AFTER COMBINE";
  print_newline ();  
  print_rtl pseudo_regs;
  
  try
    
    allocs := [];
    new_instrs := [];
    for i = 0 to 6 do death.(i) <- 0; done;
    
    list_iter (fun reg ->
        let i = reg.reg_num in
        if i<7 then begin
            allocs := (i, reg) :: !allocs;
            death.(i) <- reg.used_by_value
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
            add_instr reg (mkinstr Movl [Const c; dst] []);
        | C_load (part, v) ->
            let dst = alloc_reg reg in
            let src = address_of_value map v in
            add_instr reg (mkinstr (match part with 
                  C_Long -> Movl
                | C_SignBL -> Movsbl
                | C_SignWL -> Movswl
                | C_ZeroWL -> Movzwl
                | C_ZeroBL -> Movzbl
                | _ -> assert false) [src; dst] []);
        | C_cast _ -> assert false 
        | C_stack n ->
            let dst = alloc_reg reg in
            add_instr reg (mkinstr Movl [OffsetBase(Const_int n, Esp); dst] []);
        | C_reg r ->
            if death_at r = i then begin
                reg.map <- r.map;
                death.(r.map) <- reg.used_by_value;
                allocs := (r.map, reg) :: !allocs;
              end else
            let dst = alloc_reg reg in
            add_instr reg (mkinstr Movl [Register (physreg r); dst] []);
        | C_op (Add, C_const (Const_int 1), C_reg r) when  death_at r = i -> 
            let dst = reuse_for r reg in
            add_instr reg (mkinstr Incl [dst] []);
        | C_op (Sub, C_const (Const_int 1), C_reg r) when  death_at r = i -> 
            let dst = reuse_for r reg in
            add_instr reg (mkinstr Decl [dst] []);
        | C_op (op, v1, C_reg r) when death_at r = i -> 
            let dst = reuse_for r reg in
            let src = argument_of_value v1 reg in
            let instr = mkinstr (opcode_of_operation op) [src;dst] [] in
            add_instr reg instr;
        | C_op (Add, C_const c, C_reg r1) ->
            let dst = alloc_reg_off reg in
            let src = OffsetBase(c, physreg r1) in
            add_instr reg (mkinstr Leal [src; Register dst] []);
        | C_op (Add, C_const c, C_op (Add, C_reg r1, C_reg r2)) ->
            let dst = alloc_reg_off reg in
            let src = OffsetBaseIndex(c, physreg r1, physreg r2) in
            add_instr reg (mkinstr Leal [src;Register dst] []);
        | C_op (Add, C_const c, C_op (Add, C_reg r1, 
              C_op (Mul, C_reg r2, C_const (Const_int s)))) ->
            let dst = alloc_reg_off reg in
            let src = OffsetBaseIndexScale (c, physreg r1, physreg r2, s)
            in
            add_instr reg (mkinstr Leal [src;Register dst] []);
        | C_op (Add, C_const c, C_op (Mul, C_reg r1, C_const (Const_int s))) ->
            let dst = alloc_reg_off reg in
            let src = OffsetIndexScale (c, physreg r1, s) in
            add_instr reg (mkinstr Leal [src;Register dst] []);
        | C_op (Add, C_reg r1, C_reg r2) ->
            let c = Const_int 0 in
            let dst = alloc_reg_off reg in
            let src = OffsetBaseIndex(c, physreg r1, physreg r2) in
            add_instr reg (mkinstr Leal [src;Register dst] []);
        | C_op (Add, C_reg r1, C_op (Mul, C_reg r2, C_const (Const_int s))) ->
            let c = Const_int 0 in
            let dst = alloc_reg_off reg in
            let src = OffsetBaseIndexScale (c, physreg r1, physreg r2, s)
            in
            add_instr reg (mkinstr Leal [src;Register dst] []);
        | C_op (Mul, C_reg r1, C_const (Const_int s)) ->
            let c = Const_int 0 in
            let dst = alloc_reg_off reg in
            let src = OffsetIndexScale (c, physreg r1, s) in
            add_instr reg (mkinstr Leal [src;Register dst] []);      
        | (C_op (opcode, v1, v2)) as v-> 
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
  with
    e ->
      printf "Exception %s" (Printexc.to_string e);
      print_newline ();
      List.rev state.old_instrs
