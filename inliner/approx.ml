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

(* We use two main analysis:
- Forward analysis:
   - compute an approximation of each value 
       - in a register
       - in the store
       - in the stack

- Backward analysis:
   - compute the liveness analysis of each value
       - in a register
       - in the store
       - in the stack

These two optimizations must performed on a well formed text.
No modification of the text itself is allowed.

Interpretation des approx_states:

node.enter_node ->
  
  instrs : instr.enter_instr -> instr -> instr.leave_instr

-> node.leave_node ->

  node.link.enter_instr -> node.link -> node.link.leave_instr
  
  *)
    

open Args
open Asm

let restart_analysis = ref false
let stat_constant_prop = ref 0
let stat_better_reg = ref 0

let copy_state state =
  { 
    registers = Array.copy state.registers;
    stack_size = state.stack_size;
    stack = Array.copy state.stack;
    store = List.map (fun (v, r) -> v, Array.copy r) state.store;
    liveness = [||];
    state_num = next_state ();
  }
  
module MArray = struct
    let map2 f t1 t2 =
      assert (Array.length t1= Array.length t2);
      Array.mapi (fun u1 i -> f u1 t2.(i)) t1
  end

let state_modified = ref false

let rec simplify v = match v with
    Xop (op,args) ->
      begin
        let args = List.map simplify args in
        try
          match args with
(* Simplification on constants *)          
            [Xconst (Const_int n1);Xconst (Const_int n2)] ->
              Xconst (Const_int (match op with
                    Add -> n1 + n2
                  | Sub -> n1 - n2 
                  | And -> n1 land n2
                  | Xor -> n1 lxor n2
                  | Or -> n1 lor n2
                  | Mul -> n1 * n2
                  
                  | Shr -> n2 lsr n1
                  | Sar -> n2 asr n1
                  | Sal -> n2 lsl n1
                  | Div -> n2 / n1
                  
                  | Combine (0, 8) ->
                      (n1 land 0xffffff00) lor (n2 land 0xff)
                  | Combine (1, 8) ->
                      (n1 land 0xffff00ff) lor ((n2 land 0xff) lsl 8)
                  | Combine (0, 16) ->
                      (n1 land 0xffff0000) lor (n2 land 0xffff)
                  | _ -> assert false
                ))
          | [Xconst (Const_int n)] ->
              Xconst (Const_int (match op with
                  | Not -> lnot n
                  | Partial (0,8) -> n land 0xff
                  | Partial (1,8) -> (n lsr 8) land 0xff
                  | Partial (0,16) -> n land 0xffff
                  | _ -> assert false
                ))
          | _ -> Xop(op,args)
        with _ -> Xop (op, args)
            end
  | _ -> v
      
let rec remove_from_store leave_state addr =
  try
    let block = List.assoc addr leave_state.store in
    (* rmeove the block from the store only if it is not a closure *)
    begin
      if Array.length block >= 2 then
        match block.(1) with
          Xconst (Const_symbol s) -> 
            begin try 
                let _ = Hashtbl.find program.desc s in () (* a closure *)
              with Not_found -> try 
                    let _ = Hashtbl.find program.desc s in () (* a closure *)
                  with Not_found  ->                    
                      leave_state.store <- 
                        List.remove_assoc addr leave_state.store;
            end
        | _ -> 
            leave_state.store <- List.remove_assoc addr leave_state.store;
    end;
    Array.iter (remove_from_store leave_state) block;
  with Not_found -> ()
      
let killed_coming_addrs = ref []
let killed_orig_addrs = ref []
let tokill_coming_addrs = ref []
let tokill_orig_addrs = ref []
  
let add_killed_coming_addr addr =
  match addr with
    Xvar _ -> 
      if not (List.mem addr !killed_coming_addrs ||
          List.mem addr !tokill_coming_addrs)          
        then
        tokill_coming_addrs := addr :: !tokill_coming_addrs
  | _ -> ()

let add_killed_orig_addr addr =
  match addr with
    Xvar _ -> 
      if not (List.mem addr !killed_orig_addrs ||
          List.mem addr !tokill_orig_addrs) then
        tokill_orig_addrs := addr :: !tokill_orig_addrs
  | _ -> ()
  
let meet pos node v1 v2 =
  if v1 <> v2 then 
    let xvar = Xvar (pos, node) in
    add_killed_coming_addr v2;
    if v1 = xvar then v1 else begin
        state_modified := true;
        add_killed_orig_addr v1;
        xvar
      end else v1
    
let meet_arrays first node array1 array2 =
  Array.init (Array.length array1) (fun i ->
      meet (i+first) node array1.(i) array2.(i))

exception ArrayBoundError
  (* we are very lazy here. it's buggy. we should check that no variable
  can pass a meet if some of its pass is not correct.
  Xheader addr --> addr 
  
  We should pass through the state putting all defined values in
  a set, and then remove all the values which are depending on 
  undefined values ...
*)
  
let rec meet_state node orig_state coming_state = 
  if orig_state.stack_size = coming_state.stack_size then begin
      
      tokill_orig_addrs := [];
      tokill_coming_addrs := [];
      killed_coming_addrs := [];
      killed_orig_addrs := [];
      
      let new_state = 
        {
          registers = meet_arrays 0 node orig_state.registers coming_state.registers;
          stack = meet_arrays nregs node orig_state.stack coming_state.stack;
          stack_size = coming_state.stack_size;
        (* We are very careful with the store: if there is a meet, the store
        is simply erased. *)
          store = [];
          liveness = [||];
          state_num = next_state ();
        }
      in
      
        if orig_state.store <> [] && coming_state.store <> [] then begin
          (* We must take care of the addresses which might escape *)          
          let pos = ref (nregs + new_state.stack_size) in
          List.iter (fun (addr, block) ->
              try
                let block2 = List.assoc addr coming_state.store in
                new_state.store <- (addr,
                  Array.init (Array.length block) (fun i ->
                      if block.(i) <> block2.(i) then
                        let xvar  = Xvar (!pos, node) in
                        incr pos;
                        add_killed_coming_addr block2.(i);
                        if block.(i) <> xvar then begin
                            add_killed_orig_addr block.(i);
                            state_modified := true;
                          end;
                        xvar
                      else
                        block.(i))) :: new_state.store;
              with Not_found ->
                  add_killed_orig_addr addr
          ) orig_state.store;
          List.iter (fun (addr, _) ->
              if not(List.mem_assoc addr orig_state.store) then
                add_killed_coming_addr addr          
          ) coming_state.store;
          let rec kill_coming_addr () =            
            match !tokill_coming_addrs with 
              [] -> ()
            | addr :: tail ->
                tokill_coming_addrs := tail;
                killed_coming_addrs := addr :: !killed_coming_addrs;
                remove_from_store new_state addr;
                begin
                  try 
                    let block = List.assoc addr coming_state.store in
                    Array.iter add_killed_coming_addr block
                  with Not_found -> ()
                end;
                kill_coming_addr ()
          in
          kill_coming_addr ();
          let rec kill_orig_addr () =            
            match !tokill_orig_addrs with 
              [] -> ()
            | addr :: tail ->
                tokill_orig_addrs := tail;
                killed_orig_addrs := addr :: !killed_orig_addrs;
                remove_from_store new_state addr;
                begin
                  try 
                    let block = List.assoc addr orig_state.store in
                    Array.iter add_killed_orig_addr block
                  with Not_found -> ()
                end;
                kill_orig_addr ()
          in
          kill_orig_addr ();
      end;
      
      new_state
    end
  else 
    (* If the stacks are different, there is only two options:
    - My code is buggy
    - The node will not use the stack at all (array_bound_error for example).
  *)  
    begin
      if !debug_approx then
        (Printf.printf "Node with different stacks"; print_newline ());
      raise ArrayBoundError
    end
    
let combine_reg node enter_state leave_state num part size value =
  leave_state.registers.(num) <-
    simplify (let old_value = enter_state.registers.(num) in
    Xop (Combine (part,size), [old_value;value])
  )
      
let partial_reg enter_state reg byte bits =
  let c = enter_state.registers.(reg) in Xop (Partial (byte,bits), [c])
  
let destination node enter_state leave_state arg value =
  match arg with
    Register r ->
    (* We modify a register *)
      begin
        try
          match r with
          | LowByte r -> 
              combine_reg node enter_state leave_state (regindex r) 0 8 value
          | HighByte r -> 
              combine_reg node enter_state leave_state (regindex r) 1 8 value
          | LowWord r -> combine_reg node enter_state leave_state (regindex r) 0 16 value
          | r -> leave_state.registers.(regindex r) <- simplify value
        with Exit -> ()
      end
  | OffsetBase(Const_int n, Esp) ->
  (* We modify a stack register *)
      begin
        leave_state.stack.(n/4) <- simplify value
      end
  | OffsetBase (Const_int n, r) ->
      begin
        try
          let addr = enter_state.registers.(regindex r) in
          let block = List.assoc addr leave_state.store in
          let p = n/4 in
          if p * 4 = n && p+1 < Array.length block then  begin
              block.(p+1) <- simplify value
            end
          else remove_from_store leave_state addr
        with Not_found ->  ()
      end
  | OffsetBaseIndex (c, r1, r2) ->
      let addr = enter_state.registers.(regindex r1) in
      remove_from_store leave_state addr
  | OffsetBaseIndexScale (c,r1,r2,s) ->
      let addr = enter_state.registers.(regindex r1) in
      remove_from_store leave_state addr
  | _ -> ()
   
let destroy node enter_state leave_state arg =
  match arg with
    Register r ->
    (* We modify a register *)
      begin
        try
          leave_state.registers.(regindex_all r) <- mkvar node
        with Exit -> ()
      end
  | OffsetBase(Const_int n, Esp) ->
      (* We modify a stack register *)
      begin
        leave_state.stack.(n/4) <- mkvar node
      end
  | OffsetBase (Const_int n, r) ->
      let addr = enter_state.registers.(regindex r) in
      remove_from_store leave_state addr
  | OffsetBaseIndex (c, r1, r2) ->
      let addr = enter_state.registers.(regindex r1) in
      remove_from_store leave_state addr
  | OffsetBaseIndexScale (c,r1,r2,s) ->
      let addr = enter_state.registers.(regindex r1) in
      remove_from_store leave_state addr
  | _ -> ()
      
let source node enter_state leave_state src =
  match src with
    Const c -> Xconst c
  | ConstantBase c -> Xloadconst c
  | Register r ->
      begin
        try
          match r with
          | LowByte r -> partial_reg enter_state (regindex r) 0 8
          | LowWord r -> partial_reg enter_state (regindex r) 0 16
          | HighByte r -> partial_reg enter_state (regindex r) 1 8
          | r -> enter_state.registers.(regindex r)
        with Exit -> mkvar node
      end
  | OffsetBase (Const_int n, Esp) -> enter_state.stack.(n/4)
  | OffsetBase (Const_int (-4), r) ->
      let addr = enter_state.registers.(regindex r) in
      Xheader addr
  | OffsetBase (Const_int n, r) ->
      begin
        try
          let addr = enter_state.registers.(regindex r) in
          let block = List.assoc addr enter_state.store in
          let p = n/4 in
          if p * 4 = n && p+1 < Array.length block then block.(p+1)
          else
            ( remove_from_store leave_state addr;
              mkvar node)
        with Not_found -> mkvar node
      end
  | OffsetBaseIndex (c, r1, r2) ->
      let addr = enter_state.registers.(regindex r1) in
      remove_from_store leave_state addr;
      mkvar node
  | OffsetBaseIndexScale (c,r1,r2,s) ->
      let addr = enter_state.registers.(regindex r1) in
      remove_from_store leave_state addr;
      mkvar node
  | Indirect (Register r) ->
      begin try
          let addr = enter_state.registers.(regindex r) in
          let block = List.assoc addr enter_state.store in
          if block <> [||] then block.(1) else mkvar node
        with Not_found -> mkvar node
      end
  | _ -> mkvar node
      
let address node enter_state leave_state src =
  try
    match src with
    | OffsetBase (c, r) -> 
        Xop(Add, [Xconst c; enter_state.registers.(regindex r)])
    | OffsetBaseIndex (c, r1, r2) ->
        Xop(Add, [Xconst c; 
            Xop(Add, [enter_state.registers.(regindex r1);
                enter_state.registers.(regindex r2)])])
    | OffsetBaseIndexScale (c,r1,r2,s) ->
        Xop(Add, [Xconst c; 
            Xop(Add, [enter_state.registers.(regindex r1);
                Xop(Mul, [Xconst (Const_int s);
                    enter_state.registers.(regindex r2)])])])
    | _ -> mkvar node
  with
    _ -> Printf.printf "Exception in Approx.address"; print_newline ();
      mkvar node
      
let all_registers = 
  [Register Eax; Register Ebx; Register Ecx; Register Edx;
    Register Esi; Register Edi; Register Ebp]  
      
let destroyed_by opcode =
  match opcode with
  | Idivl -> [Register Eax; Register Edx]
      (* Here, we assume that a function will never destroy the values in
      the stack. We also assume that we never change the location in the stack
      used by an instruction. Otherwise, the location could be not protected
      against the GC.  *)
  | _ -> []
          
let operation node enter_state leave_state op arg1 arg2 =
  let s1 = source node enter_state leave_state arg1 in
  let s2 = source node enter_state leave_state arg2 in
  Xop (op, [s1;s2])

let destroy_non_live node enter_state leave_state directives =
  begin
    try
      List.iter (fun directive ->
          match directive with
            Dir_live lives ->
              (*
              Printf.printf "Lives :"; 
              List.iter (fun arg ->
                  Printf.printf "%s " (SimplePrint.string_of_arg arg);
              ) lives;
              print_newline ();
            *)
              List.iter (fun arg ->                  
                  if not (List.mem arg lives) then
                    destination node enter_state leave_state arg (mkvar node))
              all_registers;
              raise Exit              
          | Dir_saved lives ->
              (*
              Printf.printf "Saved :"; 
              List.iter (fun arg ->
                  Printf.printf "%s " (SimplePrint.string_of_arg arg);
              ) lives;
              print_newline ();
  *)
              List.iter (fun arg ->                  
                  if not (List.mem arg lives) then
                    destination node enter_state leave_state arg (mkvar node))
              all_registers;
              raise Exit              
          | _ -> ()
      ) directives;
      (* all registers should be destroyed *)
(*      Printf.printf "Destroy all physical registers"; print_newline (); *)
      List.iter (fun arg ->
          destination node enter_state leave_state arg (mkvar node))
      all_registers      
    with Exit -> ()
  end

  (* We should have a special treatment for "modify (arg1, arg2)", which
  is movl arg2, (arg1).
  *)

  
  
let escape_args node enter_state leave_state directives instr =
  try
    List.iter (fun directive ->
        match directive with
          Dir_args args ->
            (*
            Printf.printf "Escapes :"; 
              List.iter (fun arg ->
                  Printf.printf "%s " (SimplePrint.string_of_arg arg);
              ) args;
              print_newline ();
            *)
            List.iter (fun arg -> 
                let addr = source node enter_state leave_state arg in
                remove_from_store leave_state addr) args;
            raise Exit              
        | _ -> ()
    ) directives;
  with Exit -> ()  
  
let rec iter_instr node i enter_state =
  let len = Array.length node.instrs in
  if i < len then
    let enter_state = copy_state enter_state in
    let leave_state = copy_state enter_state in
    let instr = node.instrs.(i) in
    instr.enter_instr <- enter_state;
    instr.leave_instr <- leave_state;
    if !debug_approx then
      begin
        print_state enter_state;
        Printf.printf "[%s]" (SimplePrint.string_of_instr instr);
        print_newline ();
      end;
    begin
      match instr with
      (* MUST PAY LOT OF ATTENTION TO THE STACK ... *)
      | { opcode = Subl; args = [Const(Const_int n); Register Esp] } ->
          leave_state.stack_size <- leave_state.stack_size + (n/4);
          leave_state.stack <- Array.concat [
            Array.init (n/4) (fun _ -> mkvar node);
            leave_state.stack
          ]
      | { opcode = Addl; args = [Const(Const_int n); Register Esp] } ->
          leave_state.stack_size <- enter_state.stack_size - (n/4);
          leave_state.stack <- Array.sub leave_state.stack
            (n/4) leave_state.stack_size
      | { opcode = Pushl; args = [src] } ->
          leave_state.stack <- Array.concat [
            [| source node enter_state leave_state src |];
            leave_state.stack];
          leave_state.stack_size <- leave_state.stack_size + 1;
      | { opcode = Popl; args = [dst] } ->
          destination node enter_state leave_state dst 
            (source node enter_state leave_state (OffsetBase(Const_int 0, Esp)));
          leave_state.stack_size <- leave_state.stack_size - 1;
          leave_state.stack <- Array.sub leave_state.stack 1 leave_state.stack_size;
      | { opcode = Pushtrap } -> (* Equivalent to Pushl *)
          leave_state.stack <- Array.concat [
            [| mkvar node |];
            leave_state.stack
          ];
          leave_state.stack_size <- leave_state.stack_size + 1;
      | { opcode = Removetrap } -> (* Equivalent to Poptrap *)
          assert false
          (* THE INSTRUCTIONS WHICH ARE THE MOST COMMON *)
      | { opcode =  Movl; args = [src; dst] } ->
          destination node enter_state leave_state dst 
            (source node enter_state leave_state src)
      | { opcode = Lea | Leal; args = [src;dst] } ->
          begin
            try
              List.iter (fun directive ->
                  match directive with 
                    Dir_noalloc size ->
                      (* allocate the new block *) 
                      let addr = mkvar node in
                      if size < 30 then begin
                          let block = 
                            Array.init (size/4) (fun _ -> mkvar node) in
                          leave_state.store <- 
                          (addr, block) :: leave_state.store;
                        end;
                      destination node enter_state leave_state dst addr;
                      raise Exit
                  | _ -> ()
              ) instr.directives;
              destination node enter_state leave_state dst
                (address node enter_state leave_state src) 
            with Exit -> ()
          end                
      | { opcode = Xorl; args = [src;dst] } when src = dst ->
          destination node enter_state leave_state dst (Xconst (Const_int 0))
      | { opcode = (Cmpl | Testl | Cmpi _ ) } -> ()          
      | { opcode = (
            Shrl | Addl | Andl | Xorl | Orl | Subl |
            Sarl | Sall | Imull | Idivl
          ); args = [arg1;arg2] } ->
          destination node enter_state leave_state arg2
            (operation node enter_state leave_state (
              match instr.opcode with
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
              | _ -> assert false) arg1 arg2)
              (* These one have only one destination *)
      | { opcode = (
            Movzbl | Movzwl | Movsbl | Movswl | Movb | Movw
          ); args = [src;dst] } ->
          destination node enter_state leave_state dst (mkvar node)
      | { opcode = Alloc (size, _, frame, fast); 
          args = [dst];
          directives = directives } -> 
          (* destroy all non-live registers which could be destroyed by
          the GC. *)
          destroy_non_live node enter_state leave_state directives;
          (* %eax is always destroyed by the allocation *)
          destination node enter_state leave_state (Register Eax) (mkvar node);
          (* allocate the new block *) 
          let addr = mkvar node in
          if size < 30 then begin
              let block = Array.init (size/4) (fun _ -> mkvar node) in
              leave_state.store <- (addr, block) :: leave_state.store;
            end;
          destination node enter_state leave_state dst addr;
      | { opcode = Call _; 
          directives = directives } -> 
          (* destroy all non-live registers which could be destroyed by
          the GC. *)
          escape_args node enter_state leave_state directives instr;
          destroy_non_live node enter_state leave_state directives
      | _ -> 
        (* By default:
        All the arguments are supposed to be destination arguments. Since
        the value computed is unknown, we put an unknown value in these 
          destinations... Some instructions also destroy additionnal registers.
          *)
          List.iter (fun arg -> 
              destination node enter_state leave_state arg (mkvar node)
          ) ((destroyed_by instr.opcode)@instr.args);
    end;
    iter_instr node (i+1) leave_state
  else
    enter_state
    
let rec iter_node enter_state node =
  try
    state_modified := false;
    (* We want the vars to have the same names for every interation
    on the node *)
    if !debug_meet then
      begin
        Printf.printf "ENTER NODE: %d" node.node_ident.label; print_newline ();
        Printf.printf "OLD STATE"; print_newline ();
        print_state node.enter_node;
        Printf.printf "COMING STATE:"; print_newline ();
        print_state enter_state;
      end;
    let enter_state =
      if node.enter_node == noapprox then
        (state_modified := true; 
          if !debug_approx then Printf.printf "Noapprox";
          copy_state enter_state)
      else
      if node.leave_node == noapprox then raise Exit else
      match node.back_edges with 
        [_] ->
          if node.enter_node.state_num > enter_state.state_num then
            assert false;
          state_modified := true;
          copy_state enter_state
      | _ -> 
          meet_state node node.enter_node enter_state 
    in
    if !debug_meet then begin
        Printf.printf "MEET STATE:"; print_newline ();
        print_state enter_state;
      end;
    if not !state_modified then raise Exit;
    if !debug_approx then
      begin
        Printf.printf "NODE %d" node.node_ident.label;
        print_newline ();
      end;
    node.enter_node <- enter_state;
    let enter_state = iter_instr node 0 enter_state in
    let instr = node.link in
    instr.enter_instr <- copy_state enter_state;
    instr.leave_instr <- copy_state enter_state;
    node.leave_node <- copy_state enter_state;
    match instr with
      { opcode = Switch nodes } -> 
        List.iter (
          fun node -> 
            let enter_state = copy_state instr.leave_instr in
            iter_node enter_state node) nodes
    | { opcode = Setuptrap (body, next) } ->
        let body_state = copy_state instr.leave_instr in
        (* In the body: there is a call before ... *)
        let body_state = { body_state with
            stack_size = body_state.stack_size + 1;
            stack = Array.concat [
              [| Xvar (nregs, body) |]; 
              body_state.stack]} in 
        iter_node body_state body;
        (* In next: everything is unclear *)
        let next_state = copy_state instr.leave_instr in
        let next_state = { next_state with
            registers = Array.init nregs (fun i -> Xvar (i, next));
            stack = Array.init next_state.stack_size 
              (fun i -> Xvar (nregs+i, next));
            store = [];
          } in
        iter_node next_state next
    | { opcode = Jmp; 
        args = [ConstantBase (Const_label node)] } ->
        iter_node enter_state node;
    | { opcode = Jcond(comp, next); 
        args = [ConstantBase (Const_label node)] } ->
        iter_node enter_state node;
        let enter_state = copy_state instr.leave_instr in
        iter_node enter_state next
    | { opcode = Poptrap next } ->
        let next_state = copy_state enter_state in
        let next_state = { next_state with
            stack_size = next_state.stack_size - 2;
            stack = Array.sub next_state.stack 2 (next_state.stack_size - 2);
          } in
        instr.leave_instr <- next_state;
        iter_node next_state next
    | { opcode = Ret | Jmp | Raise } -> ()
    | i -> 
        Printf.printf ">>> %s <<<" (SimplePrint.string_of_instr i);
        print_newline ();
        assert false
  with
    
    Exit -> ()
  | ArrayBoundError ->
      if !debug_approx then begin
          Printf.printf "(1) found array bound error: %d" node.node_ident.label;
          print_newline ();
        end;
      node.leave_node <- noapprox
  
let compute func =
(* Start the computation *)  
  if !debug_approx then
    (Printf.printf "****************************** Approximating %s" 
      func.fun_name;
      print_newline ());

  let node = func.code in
  let on_stack = func.arity - nparams in
  let on_stack = if on_stack > 0 then on_stack else 0 in
  let enter_state = {
      registers = Array.init nregs (fun i -> Xvar (i,node));
      stack_size = on_stack + 1;
      stack = Array.init (on_stack+1) (fun i -> 
          if i = 0 then Xvar (nregs+i,node) else
            Xparam (i-1+nparams));
      store = [];
      liveness = [||];
      state_num = next_state ();
    } in
  for i = 0 to func.arity - on_stack - 1 do
    enter_state.registers.(i) <- Xparam i
  done;
  iter_node enter_state func.code
  