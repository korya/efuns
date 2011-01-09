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

open Args
open Asm
open Approx
  
let const_add c1 c2 = match c1, c2 with
    Const_int n1, Const_int n2 -> Const_int(n1+n2)
  | _ -> Const_add (c1, c2)

let const_mul c1 c2 = match c1, c2 with
    Const_int n1, Const_int n2 -> Const_int(n1*n2)
  | _ -> Const_mul (c1, c2)
      
let better_register enter_state register = 
  try
    let r = bits32 register in
    let value = enter_state.registers.(regindex r) in
    let checks = match register with
        LowByte _ | HighByte _  -> [3;2;1;0]
      | LowWord _ -> [6;5;4;3;2;1;0]
      | St _ -> raise Exit
      | _ -> [6;5;4;3;2;1;0]
    in
    let rec iter list =
      match list with
        [] -> register
      | r :: tail ->
          if enter_state.registers.(r) = value then regs.(r)
          else iter tail
    in
    let new_reg = iter checks in
    let new_reg = match register with
        LowWord _ -> LowWord new_reg
      | LowByte _ -> LowByte new_reg
      | HighByte _ -> HighByte new_reg
      | _ -> new_reg        
    in 
    if register <> new_reg && !debug_opt1 then
      (Printf.printf "Better register"; print_newline ());
    new_reg
  with _ -> register
  
let better_address enter_state arg =
  match arg with
  | OffsetBase (Const_int n, Esp) -> arg
  | OffsetBase (c,r) -> 
      begin
        match enter_state.registers.(regindex r) with
          Xconst n -> 
            if !debug_opt1 then
              (Printf.printf "Better address"; print_newline ());
            incr stat_constant_prop;
            ConstantBase(const_add c n)
        | _ -> OffsetBase (c, better_register enter_state r)
      end
      
  | OffsetBaseIndex (c,r1,r2) ->
      let r1p = regindex r1 in
      let r2p = regindex r2 in
      begin
        match enter_state.registers.(r1p), enter_state.registers.(r2p) with
          (Xconst c1, Xconst c2) -> 
            if !debug_opt1 then
              (Printf.printf "Better address"; print_newline ());
            incr stat_constant_prop;
            ConstantBase(const_add c (const_add c1 c2))
        | _, Xconst c1 -> 
            if !debug_opt1 then
              (Printf.printf "Better address"; print_newline ());
            incr stat_constant_prop;
            OffsetBase(const_add c c1, better_register enter_state r1)
        | Xconst c1, _ ->             
            if !debug_opt1 then 
              (Printf.printf "Better address"; print_newline ());
            incr stat_constant_prop;
            OffsetBase(const_add c c1, better_register enter_state r2)
        | _ -> 
            OffsetBaseIndex (c, better_register enter_state r1,
              better_register enter_state r2)
      end
      
  | OffsetBaseIndexScale (c,r1,r2,s) -> 
      let r1p = regindex r1 in
      let r2p = regindex r2 in
      begin
        match enter_state.registers.(r1p), enter_state.registers.(r2p) with
          (Xconst c1, Xconst c2) -> 
            incr stat_constant_prop;
            if !debug_opt1 then
              (Printf.printf "Better address"; print_newline ());
            ConstantBase(const_add c
                (const_add c1
                  (const_mul c2 (Const_int s))))
        | _, Xconst c1 -> 
            if !debug_opt1 then
              (Printf.printf "Better address"; print_newline ());
            incr stat_constant_prop;
            OffsetBase(const_add c (const_mul c1 (Const_int s)), 
              better_register enter_state r1)
        | Xconst c1, _ -> 
            incr stat_constant_prop;            
            if !debug_opt1 then
              (Printf.printf "Better address"; print_newline ());
            OffsetIndexScale(const_add c c1, 
              better_register enter_state r2,s)
        | _ -> 
            OffsetBaseIndexScale (c, better_register enter_state r1,
              better_register enter_state r2,s)
      end        
      
  | OffsetIndexScale (c, r, s) ->
      let rp = regindex r in
      begin
        match enter_state.registers.(rp) with
          Xconst c1 -> 
            incr stat_constant_prop;
            if !debug_opt1 then
              (Printf.printf "Better address"; print_newline ());
            ConstantBase(const_add c (const_mul c1 (Const_int s)))
        | _ ->             
            OffsetIndexScale (c, better_register enter_state r,s)
      end
  | _ -> arg
      
  (* We should clearly distinguish between source and destination registers *)
  
let source enter_state src =
  match src with
  | Register r ->
      begin
        match r with
          St _ -> raise Exit
        | LowByte r -> partial_reg enter_state (regindex r) 0 8
        | HighByte r -> partial_reg enter_state (regindex r) 1 8
        | LowWord r -> partial_reg enter_state (regindex r) 0 16
        | r -> enter_state.registers.(regindex r)
      end
  | OffsetBase (Const_int n, Esp) -> enter_state.stack.(n/4)
  | OffsetBase (Const_int n, r) ->
      begin
        let v = enter_state.registers.(regindex r) in
        try
          let block = List.assoc v enter_state.store in
          block.((n+4)/4)
        with _ -> raise Exit
      end
  | _ -> raise Exit
      
let better_source enter_state arg =
  try    
    match simplify (source enter_state arg) with
      Xconst c -> 
        if !debug_opt1 then 
          (Printf.printf "Better source"; print_newline ());
        Const c
    | v -> 
        (* Some other register may hold the same constant.
        Try to use it before *)
        let rec iter i =
          if i >= 0 then
            if enter_state.registers.(i) = v then 
              Register regs.(i)
            else
              iter (i-1)
          else arg
        in
        let new_arg = iter (nregs-1) in
        match new_arg with
          Register r when arg <> new_arg->
            if !debug_opt1 then
              (Printf.printf "Better location"; print_newline ());
            new_arg
        | _ -> better_address enter_state arg
  with
    _ -> better_address enter_state arg
      
let better_dest enter_state arg = 
  better_address enter_state arg
  
let optimize1_instr instr =
  begin
    let enter_state = instr.enter_instr in
    if not (enter_state == Asm.noapprox) then
      let leave_state = instr.leave_instr in
      if !debug_opt1 then
        (print_state enter_state;
          Printf.printf "Optimize %s"
            (SimplePrint.string_of_instr instr); print_newline ());  
      match instr with
        { opcode = (
            Movl | Movzbl | Movzbl | Movsbl | Movswl | Movb | Movw
          ); 
          args = [src;dst] } ->
          instr.args <-
            [better_source enter_state src; 
            better_dest enter_state dst]
      | { opcode = (    
            Shrl | Addl | Andl | Xorl | Orl
          | Subl | Sarl | Sall | Imull | Idivl
          ); 
          args = [src;dst] } ->
          begin
            try
              match simplify (source leave_state dst) with
                Xconst c ->
                  instr.opcode <- Movl;
                  if !debug_opt1 then
                    (Printf.printf "Better operation"; print_newline ());
                  instr.args <- [Const c; better_dest enter_state dst];
              | _ ->
                  instr.args <-
                    [better_source enter_state src; 
                    better_dest enter_state dst]          
            with _ ->
                instr.args <-
                  [better_source enter_state src; 
                  better_dest enter_state dst]
          end
      | { opcode = (Cmpl|Testl); args = [src1;src2] } ->
          let src1 = better_source enter_state src1 in
          let src2 = match src1 with
              Const _ -> (* better_source enter_state src2 *) src2
            | _ -> src2
          in
          instr.args <- [src1;src2]
            
          (* Note: the second source can only be simplified if the
            first one is a constant ... *)
      | { opcode = Call _; args = [Indirect (Register r)] } ->
          begin
            try
              match enter_state.registers.(regindex r) with
                Xconst (Const_symbol s) ->                  
                  Printf.printf "Solving indirect call";
                  print_newline ();
                  instr.args <- [ConstantBase(Const_symbol s)]
              | _ -> ()
            with _ -> ()              
          end
          
      | { opcode = (Lea|Leal); args = [src; Register dst] } ->
          let src = better_dest enter_state src in
          begin
            match src with
              OffsetBase (Const_int 0, r) -> 
                instr.opcode <- Movl;
                instr.args <- [Register r; Register dst]
            | OffsetBase (Const_int n, r) when r = dst ->
                instr.opcode <- Addl;
                instr.args <- [Const(Const_int n); Register r]
            | _ -> 
                instr.args <- [src;Register dst]
          end
      | _ -> 
          instr.args <- List.map (better_dest enter_state) instr.args
  end;
  if !debug_opt1 then
    (Printf.printf "After: %s"
        (SimplePrint.string_of_instr instr); print_newline ())

let rec remove_useless_computation instrs =
  match instrs with
    [] -> []
  | instr :: instrs ->
  (* Remove useless mov *)
      try
        match instr with
          { opcode = Movl; args = [src; dst] } ->
            begin
              match dst with
                Register r ->
                  let i = regindex r in
                  let before_instr = instr.enter_instr in
                  if before_instr.registers.(i) = 
                    instr.leave_instr.registers.(i) then begin
                      remove_useless_computation instrs
                    end else begin
                      let rec find_res comp instrs =
                        match instrs with
                          ({ opcode = (
                                Addl| Subl| Andl | Orl| Sarl| Sall| Shrl| Xorl
                              );
                              args = [src; Register rr]
                            } as instr) :: instrs when r = rr ->
                            find_res (instr::comp) instrs
                        | instr :: instrs when
                          instr.enter_instr.registers.(i) = before_instr.registers.(i)
                          -> 
                            if !debug_opt1 then begin
                                Printf.printf "REMOVE USELESS COMP";
                                print_newline ();
                                List.iter (fun i -> 
                                    Printf.printf "%s" (SimplePrint.string_of_instr i);
                                    print_newline ()) (List.rev comp);
                              end;
                            remove_useless_computation instrs
                        | (instr2 :: _ ) as instrs ->
                            let rec iter n =
                              if instr2.enter_instr.registers.(i) = 
                                before_instr.registers.(n) then begin
                                  instr.opcode <- Movl;
                                  instr.args <- [Register regs.(n); Register r];
                                  if !debug_opt1 then begin
                                      Printf.printf "REMOVE USELESS COMP";
                                      print_newline ();
                                      List.iter (fun i -> 
                                          Printf.printf "%s" (SimplePrint.string_of_instr i);
                                          print_newline ()) (List.rev comp);
                                    end;
                                  instr :: (remove_useless_computation instrs)
                                end else
                              if n = 0 then raise Not_found else iter (n-1)
                            in
                            iter 6
                        | _ -> raise Not_found
                      in
                      find_res [instr] instrs
                    end
              | OffsetBase(Const_int n, Esp) ->
                  let i = n/4 in
                  if instr.enter_instr.stack.(i) = 
                    instr.leave_instr.stack.(i) then begin
                      instrs
                    end else raise Not_found
              | _ -> raise Not_found
            end
        | _ -> raise Not_found
      with Not_found -> 
          instr :: (remove_useless_computation instrs)
    
let optimize1_node node =
(* HERE, WE HAVE ONLY THE VALUES COMPUTED BY THE APPROXIMATION PHASE *)
  let len = Array.length  node.instrs in
  if !debug_opt1 then
    (Printf.printf "NODE %d Len : %d" node.node_ident.label len; 
      print_newline ());  
  for i = 0 to len - 1 do
    optimize1_instr node.instrs.(i);
  done;
  node.instrs <- 
    Array.of_list (remove_useless_computation (Array.to_list node.instrs));
  
  optimize1_instr node.link

let func func = 
  if !debug_opt1 then
    (Printf.printf "****************************** Optimizing(1) %s" func.fun_name;
      print_newline ());
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes optimize1_node
  