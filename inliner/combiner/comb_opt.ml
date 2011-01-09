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

let subst_done = ref false
    
let rec subst v pattern repl =
  if v = pattern then
    (subst_done := true; repl) else 
  match v with
  | C_cast (cast, v) -> 
      if v = pattern then repl else
        C_cast (cast, subst v pattern repl)
  | C_op (op, v1, v2) ->
      if v = pattern then repl else
      let v1 = subst v1 pattern repl in
      let v2 = subst v2 pattern repl in
      C_op (op, v1,v2)
  | C_load (size,v) -> C_load (size,subst v pattern repl)
  | C_store (size, src, dst) ->
      C_store (size, subst src pattern repl, subst dst pattern repl)
  | C_unknown (opcode, args, values) ->
      C_unknown (opcode, args, 
        List.map (fun src -> subst src pattern repl) values)
  | _ -> v
  
      (******************************************************)
      
let subst_all tail v r2 =
  list_iter (fun r ->
      let v1 = r.reg_value in
      let v2 = subst v1 v (C_reg r2) in
      if v1 <> v2 then
          r.reg_value <- v2) tail
  
let subst_no_alias tail v r2 =
  let rec iter list = match list with
    | [] -> ()
    | r :: tail ->
        let v1 = r.reg_value in
        match v1 with
          C_store (_, _, C_stack _ ) -> 
          (* no problem with Stack stores *)
            iter tail
        | C_store _ -> () (* stop on store *)
        | C_reg _ -> iter tail
        | _ ->
            let v2 = subst v1 v (C_reg r2) in
            if v1 <> v2 then r.reg_value <- v2;
            iter tail
  in
  iter tail
  
let remove_checkbound tail args =
  let rec iter list = match list with
    | [] -> ()
    | ({ reg_value = C_unknown (Cmpl, [], args2) } as r1) ::
      ({ reg_value = C_unknown (CheckBound _, _, _) } as r2) :: tail 
      when args = args2 ->
        r1.reg_value <- C_const (Const_int 0);
        r2.reg_value <- C_const (Const_int 0);
        subst_done := true;
        iter tail
    | r :: tail ->
        iter tail
  in
  iter tail
  
let rec simplify_rec v =  
  match v with
  | C_const c -> C_const (simplify_const c)
  | C_reg { reg_value = C_reg r } -> simplify_rec (C_reg r)
  | C_op (Add, C_const (Const_int 0), v) -> simplify_rec v
  | C_op (Add, v, C_const (Const_int 0)) -> simplify_rec v
  | C_op (Add, C_const c1, C_const c2) -> C_const (Const_add (c1,c2))
  | C_op (Add, C_const c1, 
      C_op (Add, C_const c2, v)) -> 
      simplify_rec (C_op (Add, C_const (Const_add (c1,c2)), v))
  | C_op (And, C_const (Const_int 0), v) -> C_const (Const_int 0)
  | C_op (Or, C_const (Const_int 0), v) -> v
  | C_cast (C_LowByte, 
      C_reg { reg_value = C_op (And, C_const (Const_int n), C_reg r )}) 
    when n land 255 = 255 -> 
      C_cast (C_LowByte, C_reg r)
  | C_cast (C_LowWord, 
      C_reg { reg_value = C_op (And, C_const (Const_int n), C_reg r) }) 
    when n land 0xffff = 0xffff -> 
      C_cast (C_LowWord, C_reg r)
  | C_op (Sar, C_const c1, 
      C_reg { reg_value = C_op (Sar, C_const c2, C_reg r)}) ->
      C_op (Sar, C_const (Const_add (c1,c2)), C_reg r)
  | C_op (Shr, C_const c1, 
      C_reg { reg_value = C_op (Shr, C_const c2, C_reg r)}) ->
      C_op (Shr, C_const (Const_add (c1,c2)), C_reg r)
  | C_op (Sar, C_const c1, 
      C_reg { reg_value = C_op (Shr, C_const c2, C_reg r)}) ->
      C_op (Shr, C_const (Const_add (c1,c2)), C_reg r)
  | _ -> v

let simplify v = map simplify_rec v

let rec distribute_rec v =
  match v with
  | C_op (Sar, C_const (Const_int n), C_reg r) ->
      simplify_rec (distribute_sar n r)
  | C_op (Shr, C_const (Const_int n), C_reg r) ->
      simplify_rec (distribute_shr n r v)
  | C_op (Add, C_reg r1, C_reg r2) ->
      simplify_rec (distribute_add r1 r2 v)
  | _ -> v

and distribute_sar n r =
  match r.reg_value with
  | C_op (Add, C_const (Const_int nn), C_reg rr) when 
    nn land lnot (0xffffffff lsl n) = 0 ->
      C_op (Add, C_const (Const_int (nn asr n)), 
        C_op (Sar, C_const (Const_int n), C_reg rr))
  | C_op (Add, C_reg rr, C_const (Const_int nn)) when 
    nn land lnot (0xffffffff lsl n) = 0 ->
      C_op (Add, C_const (Const_int (nn asr n)), 
        C_op (Sar, C_const (Const_int n), C_reg rr))
  | C_op (And, C_const (Const_int nn), C_reg r) ->
      C_op (And, C_const (Const_int (nn asr n)), 
        distribute_sar n r)
  | C_op (Or, C_const (Const_int nn), C_reg r) ->
      C_op (Or, C_const (Const_int (nn asr n)), distribute_sar n r)
  | _ -> C_op (Sar, C_const (Const_int n), C_reg r)

and distribute_shr n r v =
  match r.reg_value with
  | C_op (Add, C_const (Const_int nn), C_reg rr) when 
    nn land lnot (0xffffffff lsl n) = 0 ->
      C_op (Add, C_const (Const_int (nn lsr n)), 
        C_op (Shr, C_const (Const_int n), C_reg rr))
  | C_op (Add, C_reg rr, C_const (Const_int nn)) when 
    nn land lnot (0xffffffff lsl n) = 0 ->
      C_op (Add, C_const (Const_int (nn lsr n)), 
        C_op (Shr, C_const (Const_int n), C_reg rr))
  | _ -> v

and distribute_add r1 r2 v =
  match r2.reg_value with
    C_op (Add, C_const c, (C_op (Shr, C_const _, C_reg _) as v)) ->
      C_op (Add, C_const c, C_op (Add, C_reg r1, v))
  | C_op (Add, C_const c, (C_op (Sar, C_const _, C_reg _) as v)) ->
      C_op (Add, C_const c, C_op (Add, C_reg r1, v))
  | C_op (Shr, C_const _, C_reg _) as v -> C_op (Add, C_reg r1, v)
  | C_op (Sar, C_const _, C_reg _) as v -> C_op (Add, C_reg r1, v)
  | _ -> v
      
let distribute v = map distribute_rec v

let rec convert_approx values v = 
  try
    Hashtbl.find values v
  with Not_found ->
      match v with
      | Xconst c -> C_const c
      | Xop (op, [arg1;arg2]) ->
          C_op (op, convert_approx values arg1, convert_approx values arg2)
          (* Xvar, Xparam, Xloadconst  *)
      | _ -> raise Not_found

          
let rec propagate_loads list = match list with
    [] -> ()
  | r1 :: tail ->
      propagate_loads tail;
      match r1.reg_value with
        C_store (C_Long, C_reg r2, C_stack n) ->
          subst_all tail (C_load (C_Long, C_stack n)) r2
      | C_load (C_Long, C_stack n) ->
          subst_all tail (C_load (C_Long, C_stack n)) r1
      | _ -> ()
          
let init_first_registers enter_state pseudo_regs =
  
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
  done  
  
let simplify_regs pseudo_regs =
  
  (*** propagate heap loads/stores ***)
  let rec iter list = match list with
      [] -> ()
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
    print_rtl "BEFORE SUBST" pseudo_regs;
    iter pseudo_regs; (* do it twice, just because the block is small *)
    list_iter (fun r -> r.reg_value <- simplify r.reg_value) pseudo_regs    
  done


let create_pseudo_reg v = {
    reg_num = 0; 
    reg_value = v; 
    reg_used = [];
    used_by_addr = 0; 
    used_by_value = 0; 
    prefered_reg = None;
    force_reg = [];
    map = -1;
    nused = 0;
    tail = [];
  }
    
let rec cut r pseudo_regs =
  match r.reg_value with
  | C_op (Add, C_const c, C_op (Add, v2, v3)) -> r :: pseudo_regs
  | C_op (Add, v1, C_op (Mul, v2, v3)) -> r :: pseudo_regs
  | C_op (op1, v1, C_op (op2, v2, v3)) ->
      let ps = create_pseudo_reg (C_op (op2, v2, v3)) in
      r.reg_value <- C_op (op1, v1, C_reg ps);
      cut ps (r:: pseudo_regs)
  | _ -> r :: pseudo_regs
  
let strenght_reduction pseudo_regs = 
  
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
        if r.reg_num < 7 then r :: pseudo_regs else
          cut r pseudo_regs
    ) pseudo_regs [] in
  
  list_iter (fun r -> r.reg_value <- simplify r.reg_value) pseudo_regs;
  pseudo_regs

let distribute_regs pseudo_regs =
  list_iter (fun r -> 
      r.reg_value <- 
        simplify (distribute r.reg_value)) pseudo_regs;
  
  