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

let symetric opcode = 
  match opcode with
    Or -> true
  | And -> true
  | _ -> false
  
let physreg r = 
  assert (r.used_by_value>0);
  regs.(r.map)

      
let death = Array.create 7 0
let allocs = ref []
let new_instrs = ref []  


let death_at r = 
  assert (r.used_by_value > 0);
  death.(r.map)

let rec reuse_for r r2 =
  assert (death_at r = r2.reg_num);
  r2.map <- r.map;
  death.(r.map) <- r2.used_by_value;
  allocs := (r.map, r2) :: !allocs;
  Register regs.(r.map)

and alloc_reg reg =
  let j = reg.reg_num in
  match reg.prefered_reg with
    
    Some r when death.(r) <= j ->
      printf "For r%d, preferred is free" j; print_newline ();
      allocs := (r, reg) :: !allocs;
      death.(r) <- reg.used_by_value;
      reg.map <- r;
      regs.(r)
  
  | Some rr ->
      printf "For r%d, preferred is used" j; print_newline ();
      begin
        let dst = ref 0 in
        let sreg = try List.assoc rr !allocs with _ -> assert false in
        try
          match sreg.prefered_reg with
            Some r when r<>rr && death.(r) < j ->
              add_instr_end (
                mkinstr Movl [
                  Register regs.(rr); Register regs.(r)] []);
              sreg.map <- r;
              death.(r) <- sreg.used_by_value;
              reg.map <- rr;
              death.(rr) <- reg.used_by_value;
              allocs := (r, sreg) :: (rr, reg) :: !allocs;
              regs.(rr)
          
          | _ -> 
              for r = 0 to 6 do
                if death.(r) < j then
                  ( dst := r;
                    raise Exit)
              done;
              failwith "No register found"        
        with Exit -> 
            let r = !dst in
                (* we must force this register to go in its final. *)
              assert (sreg.map = rr);
              assert (sreg.used_by_value > j);
              printf "GOOD ALLOC !"; print_newline ();
              add_instr_end (
                mkinstr Movl [
                  Register regs.(rr); Register regs.(r)] []);
              sreg.map <- r;
              death.(r) <- sreg.used_by_value;
              reg.map <- rr;
              death.(rr) <- reg.used_by_value;
              allocs := (r, sreg) :: (rr, reg) :: !allocs;
              regs.(rr)
            (*
            with 
              Not_found ->
                assert false;
                allocs := (r, reg) :: !allocs;
                reg.map <- r;
                death.(r) <-  reg.used_by_value;
                regs.(r); *)
      end                
  | _ ->
      printf "For r%d, no preferred" j; print_newline ();
      (** first register **)
      let dst = ref 0 in
      try
        for r = 0 to 6 do
          if death.(r) <= j then (dst := r; raise Exit)
        done;
        failwith "No register found"        
      with Exit -> 
          let r = !dst in
          reg.map <- r;
          allocs := (r, reg) :: !allocs;
          death.(r) <-  reg.used_by_value;
          regs.(r); 
          
and alloc_reg_off reg =
  let j = reg.reg_num in
  match reg.prefered_reg with
    
    Some r when death.(r) < j ->
      death.(r) <- reg.used_by_value;
      allocs := (r, reg) :: !allocs;
      reg.map <- r;
      regs.(r)
  | Some rr ->
      begin
        let dst = ref 0 in
        try
          for r = 0 to 6 do
            if death.(r) < j then
              ( dst := r;
                raise Exit)
          done;
          failwith "No register found"        
        with Exit -> 
            let r = !dst in
                (* we must force this register to go in its final. *)
            try
              let sreg = List.assoc rr !allocs in
              assert (sreg.map = rr);
              assert (sreg.used_by_value >= j);
              printf "GOOD ALLOC OFF !"; print_newline ();
              add_instr_end (
                mkinstr Movl [
                  Register regs.(rr); Register regs.(r)] []);
              sreg.map <- r;
              death.(r) <- sreg.used_by_value;
              reg.map <- rr;
              death.(rr) <- reg.used_by_value;
              allocs := (r, sreg) :: (rr, reg) :: !allocs;
              regs.(rr)
            with 
              Not_found ->
                assert false;
                allocs := (r, reg) :: !allocs;
                reg.map <- r;
                death.(r) <-  reg.used_by_value;
                regs.(r); 
      end                
  | _ ->
      (** first register **)
      let dst = ref 0 in
      try
        for r = 0 to 6 do
          if death.(r) < j then (dst := r; raise Exit)
        done;
        failwith "No register found"        
      with Exit -> 
          let r = !dst in
          reg.map <- r;
          allocs := (r, reg) :: !allocs;
          death.(r) <-  reg.used_by_value;
          regs.(r); 

and add_instr reg instr =
  printf "for r%d = %s" reg.reg_num (string_of_value reg.reg_value);
  print_newline ();
  printf "%s" (SimplePrint.string_of_instr instr);
  print_newline ();
  new_instrs := instr :: !new_instrs;

and add_instr_end instr =
  printf "for liveness: %s" (SimplePrint.string_of_instr instr);
  print_newline ();
  new_instrs := instr :: !new_instrs;

and alloc_byte_reg reg =
  let j = reg.reg_num in
  let dst = ref (Eax) in
  try
    for r = 0 to 3 do
      if death.(r) <= j then
        ( dst := regs.(r); 
          reg.map <- r;
          allocs := (r, reg) :: !allocs;
          death.(r) <- reg.used_by_value;
          raise Exit)
    done;
    failwith "Byte reg: No register found"        
  with Exit -> !dst

and argument_of_value v reg =
  match v with
  | C_reg r -> Register (physreg r)
  | C_const c -> Const c
  | C_cast (part, C_reg r) ->
      begin
        match part with
          C_LowByte -> 
            let pr = physreg r in
            if List.mem pr [Eax;Ebx;Ecx;Edx] then
              Register (LowByte pr)
            else
            let pr = alloc_byte_reg reg in
            Register (LowByte pr)
        
        | C_HighByte -> 
            let pr = physreg r in
            if List.mem pr [Eax;Ebx;Ecx;Edx] then
              Register (HighByte pr)
            else
            let pr = alloc_byte_reg reg in
            Register (HighByte pr)
        
        | C_LowWord -> Register (LowWord (physreg r))
        | _ -> assert false
      end
  | C_load (C_Long, C_stack n) -> OffsetBase (Const_int n, Esp)
  | C_load (C_Long, addr) -> compute_addr addr
  | _ -> assert false

and compute_addr v = 
  match v with
  | C_reg r -> OffsetBase(Const_int 0, physreg r)
  | C_op (Add, C_const c, C_reg r1) -> OffsetBase(c, physreg r1)
  | C_op (Add, C_const c, C_op (Add, C_reg r1, C_reg r2)) ->
      OffsetBaseIndex(c, physreg r1, physreg r2)
  | C_op (Add, C_const c, C_op (Add, C_reg r1, 
        C_op (Mul, C_reg r2, C_const (Const_int s)))) ->
      OffsetBaseIndexScale (c, physreg r1, physreg r2, s)
  | C_op (Add, C_const c, C_op (Mul, C_reg r1, C_const (Const_int s))) ->
      OffsetIndexScale (c, physreg r1, s)
  | C_op (Add, C_reg r1, C_reg r2) ->
      let c = Const_int 0 in
      OffsetBaseIndex(c, physreg r1, physreg r2)
  | C_op (Add, C_reg r1, C_op (Mul, C_reg r2, C_const (Const_int s))) ->
      let c = Const_int 0 in
      OffsetBaseIndexScale (c, physreg r1, physreg r2, s)
  | C_op (Mul, C_reg r1, C_const (Const_int s)) ->
      let c = Const_int 0 in
      OffsetIndexScale (c, physreg r1, s)
  | C_const c -> ConstantBase c
  | C_stack n -> OffsetBase(Const_int n, Esp)
  | _ -> assert false

let rec iter_used_by_addr v i =
  match v with
    C_reg r ->
      if r.used_by_addr < i then r.used_by_addr <- i
  | C_cast (cast, v) -> iter_used_by_addr v i
  | C_op (Add, v1, v2) -> 
      iter_used_by_addr v1 i;
      iter_used_by_addr v2 i
  | C_op (_, v1, v2) -> 
      iter_used_by_value v1 i;
      iter_used_by_value v2 i
  | C_load (_,v) -> iter_used_by_addr v i
  | _ -> ()

and iter_used_by_value v i =
  match v with
    C_reg r ->
      if r.used_by_value < i then r.used_by_value <- i
  | C_cast (cast, v) -> iter_used_by_value v i
  | C_op (_, v1, v2) -> 
      iter_used_by_value v1 i;
      iter_used_by_value v2 i
  | C_load (_,v) -> iter_used_by_addr v i
  | _ -> ()

and find_1reg reg offset i =
  if reg.used_by_value > 0 then
    begin
      if reg.used_by_value < i then 
        reg.used_by_value <- i;
      offset, reg
    end else
  match reg.reg_value with
    C_op (Add, C_const c, C_reg reg) ->
      find_1reg reg (Const_add (offset,c)) i
  | _ ->
      if reg.used_by_value < i then 
        reg.used_by_value <- i;
      offset, reg

and find_addr reg offset i =
  if reg.used_by_value > 0 then
    begin
      if reg.used_by_value < i then 
        reg.used_by_value <- i;
      C_op (Add, C_const (simplify_const offset), C_reg reg)
    end else
  match reg.reg_value with
    C_op (Add, C_const c, C_reg reg) ->
      find_addr reg (Const_add (offset,c)) i
  | C_op (Add, C_const c, C_op (Add, C_reg r1, C_reg r2)) ->
      let offset = Const_add (c, offset) in
      let (offset, reg1) = find_1reg r1 offset i in
      let (offset, reg2) = find_1reg r2 offset i in
      C_op (Add, C_const (simplify_const offset), 
        C_op (Add, C_reg reg1, C_reg reg2))
  | C_op (Add, C_reg r1, C_reg r2) ->
      let (offset, reg1) = find_1reg r1 offset i in
      let (offset, reg2) = find_1reg r2 offset i in
      C_op (Add, C_const (simplify_const offset), 
        C_op (Add, C_reg reg1, C_reg reg2))
  | C_op (Add, C_const c, C_op (Add, C_reg r1, 
        C_op (Mul, C_reg r2, C_const (Const_int s)))) ->
      let offset = Const_add (c, offset) in
      let offset, reg1 = find_1reg r1 offset i in 
      C_op (Add, C_const (simplify_const offset), 
        C_op (Add, C_reg reg1,
          C_op (Mul, C_reg r2, C_const (Const_int s))))
  | C_op (Add, C_reg r1, 
      C_op (Mul, C_reg r2, C_const (Const_int s))) ->
      let offset, reg1 = find_1reg r1 offset i in 
      C_op (Add, C_const (simplify_const offset), 
        C_op (Add, C_reg reg1,
          C_op (Mul, C_reg r2, C_const (Const_int s))))
  | _ ->
      if reg.used_by_value < i then 
        reg.used_by_value <- i;
      C_op (Add, C_const (simplify_const offset), C_reg reg)


let rec force_alloc i r =
  match r.prefered_reg with
    Some _ -> 
      () 
        (* Can't force a register to go into another one *)
  | _ -> 
      printf "Force alloc r%d in %s" r.reg_num 
        (string_of_register regs.(i));
      print_newline ();
      r.prefered_reg <- Some i;
      match r.reg_value with
        C_op (_, C_const _, C_reg r) ->
          force_alloc i r
      | _ -> ()
          
let liveness_regs state pseudo_regs =        
  let num = ref 0 in
  list_iter (fun r ->
      let i = !num in
      incr num;
      r.reg_num <- i;
      r.prefered_reg <- None;
      r.force_reg <- [];
  ) pseudo_regs;  
    
  (try
      list_iter (fun r ->
          let i = r.reg_num in
          if i = 7 then 
            r.prefered_reg <- Some i
          else raise Exit) pseudo_regs
    with _ -> ());
  
  (try
      list_iter (fun r ->
          let i = r.reg_num in
          if i < 7 then begin
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
                C_op ((Sal|Shr), C_cast (_, C_reg r1) , _ ) ->
                (* r1 must be in ecx *)
                  if r1.reg_num >= 7 then begin
                      force_alloc 2 (* %ecx *) r1; (* preference *)
                      r.force_reg <- (r1, Map_reg 2) :: r.force_reg;
                    end
              | C_store ((C_LowByte | C_HighByte), C_reg r, _) ->
                  r.force_reg <- (r, Map_reg8) :: r.force_reg;
              | _ -> ()
            end
      ) pseudo_regs;
    with _ -> ());
  
  (* iter_used_by_addr indicates that a given register's computation is
  not required, since the computation can be done at the last moment, by
  an addressing mode.
  
  iter_used_by_value indicates that the register is used in a computation
  where it needs to be stored in a single register, thus, forcing its 
  computation.
    *)
  
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
  ) pseudo_regs
  
let combine_regs pseudo_regs =    
  list_iter (fun r ->
      let i = r.reg_num in
      if r.used_by_value + r.used_by_addr> 0 then
        match r.reg_value with
          C_store (part, src, C_reg addr) ->
            r.reg_value <- 
              C_store (part, src, find_addr addr (Const_int 0) i);
        | C_load (part, C_reg addr) ->
            printf "find_addr at r%d" i; print_newline ();
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
      printf "COMBINER ON r%d" r.reg_num;
      print_newline ();
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
              printf "TRY COMBINE OP AND LOAD on r%d" r.reg_num;
              print_newline ();
              if 
                r2.used_by_value = r.reg_num &&
                r1.used_by_value = r.reg_num &&
                r1.nused = 1 then
                begin
                  r1.used_by_value <- 0;
                  r.reg_value <- 
                    C_op (opcode, v, C_reg r2)
                end 
          | C_op (opcode, C_reg r1, C_reg ({
                  reg_value = (C_load _) as v;
                } as r2)) 
            when symetric opcode && no_store r2 r &&
            r2.used_by_value = r.reg_num && r2.nused = 1 &&
            r1.used_by_value = r.reg_num && r1.nused = 1 ->
              printf "TRY SYMETRIC";
              print_newline ();
              r2.used_by_value <- 0;
              r.reg_value <- C_op (opcode, v, C_reg r1);
              (match r2.prefered_reg with
                  None -> ()
                | Some i ->
                    force_alloc i r1);
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
  
  (*** try to propagate register constraints backward so that registers
  are well allocated as soon as possible. *)
  
  let rec propagate_c list =
    match list with
      [] -> (* Here, we could examine liveness information to force
      register allocation. *)
        []
    | reg :: tail ->
        let cs = propagate_c tail in
        if reg.reg_num < 7 then [] else
        (* remove useless constraints *)
        let cs = remove_c reg cs in
        reg.force_reg <- cs;
        cs
  
  and remove_c reg cs =
    match cs with
      [] -> reg.force_reg
    | ((r, maps) as c) :: cs ->
        let cs = remove_c reg cs in
        (* remove c if there is an inconsistency with the current reg *)
        try
        (* verify that the constraint is OK *)
          verify_c reg r maps;
          if r = reg then (* a constraint on the current reg. can be
        translated in a constraint of the arguments *)
            match reg.reg_value with
            | C_op (_, C_const _, C_reg r) ->
                if r.used_by_value > reg.reg_num then 
                  c :: cs
                else
                  (r, maps) :: c :: cs
            | _ -> (* cannot force anything *)
                c :: cs
          else
            c :: cs
        with Exit -> cs
  
  and verify_c reg r maps =
    match maps with
      Map_reg i ->
        List.iter (fun (r2, maps2) ->
            if r <> r2 then
              if r.reg_num < r2.reg_num then raise Exit;
              match maps2 with
                Map_reg j -> if i = j then raise Exit
              | _ -> ()) r.force_reg
    | _ -> ()
  in
  let _ = propagate_c pseudo_regs in
  ()

  (* Le probleme vient du fait que nous n'autorisons pas une copie juste avant
  l'instruction dans le bon register. *)
  
let force_reg_map reg before =
  List.iter (fun (r, map) ->
      if before then
        if r.reg_num < reg.reg_num then
          match map with
            Map_reg i -> 
              if r.map = i then (printf "Map OK"; print_newline ())
              else
                begin
                  printf "Map not OK for r%d" r.reg_num; 
                  print_newline ();
                end
          | Map_reg8 ->
              if r.map < 4 then  (printf "Map OK"; print_newline ())
              else
                begin
                  printf "Map not OK for r%d" r.reg_num; 
                  print_newline ();
                end
        else
        if r = reg then
          (* ensure that the final location of r is OK.  *)
          match map with
            Map_reg i -> 
              (try
                  let r = List.assoc i !allocs in
                  printf "Old register in final reg: r%d"
                    r.reg_num; print_newline ();
                  if r.used_by_value > reg.reg_num then begin
                  (* try to find another register for this location *)
                      printf "Force final reg free"; 
                      print_newline ();
                    for i = 0 to 6 do
                      if death.(i) < reg.reg_num then begin
                          add_instr_end (mkinstr Movl [
                              Register regs.(r.map); Register regs.(i)] []);
                          death.(r.map) <- 0;
                          r.map <- i; 
                          death.(i) <- r.used_by_value;
                          allocs := (i, r) :: !allocs;
                          raise Not_found
                        end
                      done
                    end
                    (* nothing available *)
                  else
          (* normally OK *)
                    ()
                with Not_found -> ())
          | Map_reg8 -> ()
        else assert false
      else
      if not before && r = reg then
        match map with
          Map_reg i -> 
            if r.map = i then (printf "Map OK"; print_newline ())
            else
              begin
                printf "Map not OK for r%d" r.reg_num; 
                print_newline ();
                (try
                    let r = List.assoc i !allocs in
                    if r.used_by_value > reg.reg_num then
                  (* try to find another register for this location *)
                      for j = 0 to 6 do
                        if death.(j) < reg.reg_num then begin
                            add_instr_end (mkinstr Movl [
                                Register regs.(r.map); Register regs.(j)] []);
                            death.(r.map) <- 0;
                            r.map <- j; 
                            death.(j) <- r.used_by_value;
                            allocs := (j, r) :: !allocs;
                            raise Not_found
                          end
                      done
                      (* nothing available *)
                    else raise Not_found                  
                  with Not_found -> 
                      begin
                        add_instr_end (mkinstr Movl [
                            Register regs.(r.map); Register regs.(i)] []);
                        death.(r.map) <- 0;
                        r.map <- i; 
                        death.(i) <- r.used_by_value;
                        allocs := (i, r) :: !allocs;
                      end)
              end
        | Map_reg8 ->
            if r.map < 4 then  (printf "Map OK"; print_newline ())
            else
              begin
                printf "Map not OK for r%d" r.reg_num; 
                print_newline ();
              end
  ) reg.force_reg
  