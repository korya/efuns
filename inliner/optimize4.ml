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

(**
We try to go directly from one test to another. For this, we must have an 
accurate view of back_edges, since we can only modify a node if it is
  only reachable after passing the conditions we are testing.
**)

open Args
open Misc
open Asm
open Approx

exception Found of int
  
let stat_jump2jump = ref 0
let stat_move_header = ref 0

  
let printf a =
  if !debug_opt4 then 
    Printf.printf a
  else
  let rec f = (fun _ -> Obj.magic f) in Obj.magic f

let print_newline () = 
  if !debug_opt4 then 
    Pervasives.print_newline ()

  
type condition =
  SwitchOn of int
| ValueComp of signed_comparison
| BitComp of signed_comparison

  (* From a set of conditions, which are known to be true at the starting 
  point, we follow some nodes, by applying these conditions. If we find
  a condition not already solved, or an instruction with side-effect, or
  differences in live registers, we fall back to the previous destination, else
  we use the new computed one. 
  Note: in a loop, we could find that the values are the same after a
  Jcond to the begin of the loop, whereas it is not true !
  *)

let rec indep v node = 
  match v with
    Xvar (_, n) when n == node -> raise Exit
  | Xheader v -> indep v node
  | Xaddress v -> indep v node
  | Xop (_, vs) -> List.iter (fun v -> indep v node) vs
  | _ -> ()
      
let rec find_next conds leave_state old_node next = 
  (* NOTE: the conditions which depend on a meet value at this node
become unknown. *)
  
  let conds = List.fold_left (fun list (((v1,v2),comp) as cond) ->
        try
          indep v1 next;
          indep v2 next;
          cond :: list
        with _ -> list) [] conds in
  
  let old_node =
    if next == old_node then old_node else
    if next.enter_node.stack_size <> leave_state.stack_size then old_node else
    try
      let liveness = next.enter_node.liveness in      
      let len = Array.length liveness in
      for i = 0 to  len - 1 do
        if liveness.(i) <> [] && 
          (if i < 7 then begin
                leave_state.registers.(i) <> next.enter_node.registers.(i) ||
                (match next.enter_node.registers.(i) with
                    Xvar (_, node) when node == next -> true
                    (* clearly, we have a meet at this point. Thus, the value in the register of the original node is received from this node, and it is a loop. We cannot compare two meet values at a meet point. *)
                  | _ -> false) end else
            let index = len - 1 - i in
            leave_state.stack.(index) <> next.enter_node.stack.(index) ||
            (match next.enter_node.stack.(index) with
                Xvar (_, node) when node == next -> true
                    (* clearly, we have a meet at this point. Thus, the value in the register of the original node is received from this node, and it is a loop. We cannot compare two meet values at a meet point. *)
              | _ -> false) 
          ) then
          raise Exit
      done;
      incr stat_jump2jump;
      next
    with Exit -> old_node
  in
  try
  (** assert that all instructions are safe **)
    Array.iter (fun instr -> 
        match instr with
          { opcode = (Testl | Cmpl) }
        | { opcode = (
              Movl | Addl | Sarl | Sall | Shrl | Subl | 
              Imull | Idivl | Andl | Orl | Xorl
            );
            args = [_; Register _]; } 
        | { opcode = (Incl| Decl); args = [Register _ ] } 
          -> ()
        | _ -> raise Exit
    ) next.instrs;
    match next.link with
      { opcode = Switch nodes; args = [Register r] } ->
        let v = next.leave_node.registers.(regindex r) in
        begin
          try
            let rec iter list =
              match list with
                ((v1,v2), SwitchOn index) :: tail
                  when v1 = v2 && v = v1 -> 
                  if !debug_opt4 then begin
                      printf "INDEX FOUND"; print_newline ();
                    end;
                  index
              | ((Xconst (Const_int n),v1), ValueComp comp) :: tail 
                  when v1 = v && (comp = Isigned Ceq || comp = Iunsigned Ceq)
                -> 
                  if !debug_opt4 then begin
                      printf "INDEX (comp) FOUND"; print_newline ();
                    end;
                  n
              | ((Xconst (Const_int n),v1), ValueComp comp) :: tail 
                  when Xop(Sar, [Xconst (Const_int 1); v1]) = v &&
                (comp = Isigned Ceq || comp = Iunsigned Ceq)
                -> 
                  if !debug_opt4 then begin
                      printf "INDEX (Sarl) FOUND"; print_newline ();
                    end;
                  n lsr 1
              | h :: tail -> iter tail
              | [] -> raise Not_found
            in
            let index = iter conds in
            if !debug_opt4 then begin
                printf "Found index in second switch ...";
                print_newline ();
              end;
            find_next conds leave_state old_node (Array.of_list nodes).(index)
          with Not_found ->
              old_node
        end
    | { opcode = Jmp; args = [ConstantBase(Const_label next)] } ->
        find_next conds leave_state old_node next
    | { opcode = Jcond (comp, new_next); 
        args = [ConstantBase(Const_label new_other)] } -> begin
          assert (next.instrs <> [||]);
          let test = next.instrs.(Array.length next.instrs - 1) in
          match test with
          | { opcode = Testl; args = [Const c; Register r] } ->
              begin
              (* has this test already been done ? *)
                let v = next.leave_node.registers.(regindex r) in
                try
                  let rec iter list =
                    match list with
                      [] -> raise Not_found
                    | ((Xconst c1,v2), BitComp new_comp) :: tail when 
                      c1 = c && v = v2 ->
                        if !debug_opt4 then begin
                            printf "TEST FOUND"; print_newline ();
                          end;
                        if new_comp = comp then
                          find_next conds leave_state old_node new_other
                        else
                        if new_comp = Misc.inverse_cond comp then
                          find_next conds leave_state old_node new_next
                        else begin
                            if !debug_opt4 then begin
                                printf "BAD COMP"; print_newline ();
                              end;
                            old_node
                          end
                    | h :: tail -> iter tail
                  in
                  iter conds
                with Not_found -> old_node
              end
          | _ -> old_node
        end
    | _ -> old_node
  with 
    _ -> old_node

      (* we must be very careful to ensure that we cannot arrive to a node
      and believe we are the only path to this node if it is not true.
    *)
      
let rec iter_node conds node =
  if !debug_opt4 then begin
      printf "ITER NODE %d" node.node_ident.label; print_newline ();
    end;
  match node.link with
  | { opcode = Jcond (cond, next); 
      args = [ConstantBase(Const_label 
            ({ instrs = [||]; 
              link = { opcode = Jmp; args = [ConstantBase(Const_label other)] }
            } as old_other)
        )] } ->
      node.link.args <- [ConstantBase(Const_label other)];
      Cfg.remove_edge node old_other;
      Cfg.add_edge node other;
      incr stat_jump2jump;
      iter_node conds node
  | { opcode = Switch nodes;
      args = [Register r] } ->
      let nodes = Array.of_list nodes in
      assert (node.leave_node.registers <> [||]);
      let v = node.leave_node.registers.(regindex r) in
      let nodes = Array.mapi (fun i next ->
            let old_next = next in
            let next = 
              find_next (((v,v), SwitchOn i)::conds) node.leave_node next next
            in
            Cfg.add_edge node next;
            Cfg.remove_edge node old_next;
            next
        ) nodes in
      node.link.opcode <- Switch (Array.to_list nodes)
  | { opcode = Jcond (comp, next); 
      args = [ConstantBase(Const_label other)] }
    -> 
      begin
        let old_next = next in
        let old_other = other in
        try
          let test =
            if node.instrs = [||] then 
              match node.back_edges with
              | [ { instrs = [||] }] -> raise Exit
              | [ node ] -> node.instrs.(Array.length node.instrs - 1)
              | _ -> raise Exit
            else
              node.instrs.(Array.length node.instrs - 1) in
          let normal_comp, inverse_comp = match test.opcode with
              Testl -> BitComp comp, BitComp (Misc.inverse_cond comp)
            | Cmpl -> ValueComp comp, ValueComp (Misc.inverse_cond comp)
            | _ -> raise Exit
          in
          let couple = match  test.args with
              [Register r1; Register r2] ->
                assert (node.leave_node.registers <> [||]);
                let v1 = node.leave_node.registers.(regindex r1) in
                let v2 = node.leave_node.registers.(regindex r2) in
                (v1,v2)
            | [Const c; Register r2] ->
                assert (node.leave_node.registers <> [||]);
                let v2 = node.leave_node.registers.(regindex r2) in
                (Xconst c, v2)
            | [Register r1; Const c] ->
                assert (node.leave_node.registers <> [||]);
                let v1 = node.leave_node.registers.(regindex r1) in
                (v1, Xconst c)
            | [Const c1; Const c2] ->
                (Xconst c1, Xconst c2)
            | _ -> raise Exit
          in
          match couple, test.opcode with
            (Xconst (Const_int n1), Xconst (Const_int n2)), Cmpl ->
              (** Yes, this is unbelievable, but it appears in
            efuns/toplevel/interp.s ... *)
              
              let next = 
                if match comp with
                        (* BUG we don't care about signed or unsigned. *)
                        | Isigned Ceq -> n1 = n2
                        | Isigned Cne -> n1 <> n2
                        | Isigned Clt -> n1 > n2
                        | Isigned Cle -> n1 >= n2
                        | Isigned Cgt -> n1 < n2
                        | Isigned Cge -> n1 <= n2
                        
                        | Iunsigned Ceq -> n1 = n2
                        | Iunsigned Cne -> n1 <> n2
                        | Iunsigned Clt -> n1 > n2
                        | Iunsigned Cle -> n1 >= n2
                        | Iunsigned Cgt -> n1 < n2
                        | Iunsigned Cge -> n1 <= n2
                then other else next
              in
              (* don't remove the comparison it the next node is a new test *)
              if next.instrs <> [||] then
                node.instrs <- 
                  Array.sub node.instrs 0 (Array.length node.instrs - 1);
              node.link.opcode <- Jmp;
              node.link.args <- [ConstantBase(Const_label next)];
              incr stat_jump2jump
          | (Xloadconst c1, Xloadconst c2), Cmpl when c1 = c2 ->
              let next = match comp with
                  Isigned Ceq -> other
                | Isigned Cne -> next
                | Iunsigned Ceq -> other
                | Iunsigned Cne -> next
                | _ -> raise Exit
              in
              if next.instrs <> [||] then
                node.instrs <- 
                  Array.sub node.instrs 0 (Array.length node.instrs - 1);
              node.link.opcode <- Jmp;
              node.link.args <- [ConstantBase(Const_label next)];
              incr stat_jump2jump
          | (Xconst c1, Xconst c2), Cmpl when c1 = c2 ->
              let next = match comp with
                  Isigned Ceq -> other
                | Isigned Cne -> next
                | Iunsigned Ceq -> other
                | Iunsigned Cne -> next
                | _ -> raise Exit
              in
              if next.instrs <> [||] then
                node.instrs <- 
                  Array.sub node.instrs 0 (Array.length node.instrs - 1);
              node.link.opcode <- Jmp;
              node.link.args <- [ConstantBase(Const_label next)];
              incr stat_jump2jump
          | _ -> 
              let normal_conds = (couple, normal_comp)::conds in
              let other = find_next normal_conds node.leave_node other other
              in
              let inverse_conds = (couple, inverse_comp)::conds in
              let next = find_next inverse_conds node.leave_node next next
              in
              node.link.opcode <- Jcond (comp,next);
              node.link.args <- [ConstantBase(Const_label other)];
          (* conservative approximation of edges *)
              Cfg.add_edge node next;
              Cfg.add_edge node other;
              Cfg.remove_edge node old_next;
              Cfg.remove_edge node old_other;
              if List.length next.back_edges = 1 then begin
                  if !debug_opt4 then begin
                      printf "From %d Examining next %d with cond"
                        node.node_ident.label next.node_ident.label
                      ; print_newline ();
                    end;
                  iter_node inverse_conds next
                end;
              if List.length other.back_edges = 1 then begin
                  if !debug_opt4 then begin
                      printf "From %d Examining other %d with cond"
                      node.node_ident.label other.node_ident.label
                      ; print_newline ();
                    end;
                  iter_node normal_conds other
                end;
              
        with Exit -> ()
      end
  | _ -> ()

let is_abort node =
  match node.link.opcode with
    Ret -> true
  | Raise -> true
  | _ -> false
      
let make_trywith next =
  match next.link.opcode with
    Setuptrap _ -> true
  | _ -> false
      
let jump2jump node =
  let rec iter nodes =
    match node.link with
      { opcode = Jmp; 
        args = [ConstantBase(Const_label next)] } when 
      not (List.memq next nodes)->
        if Array.length next.instrs < !max_concat_len &&
          not (make_trywith next)
          then begin
            if !debug_opt4 then begin
                printf "CONCAT NODE %d WITH NODE %d"
                node.node_ident.label next.node_ident.label;
                print_newline ();
              end;
            node.instrs <- 
              Array.concat [ node.instrs; copy_instrs next.instrs ];
            node.link <- copy_instr next.link;
            node.leave_node <- copy_state next.leave_node;
            List.iter (Cfg.add_edge node) (node_succ next);
            Cfg.remove_edge node next;
            incr stat_jump2jump;
            iter (next::nodes)
          end
    | { opcode = Jcond (comp, next); args = [ConstantBase(Const_label other)] }
      ->
        if is_abort next && not (is_abort other) && 
          List.length other.back_edges = 1 then begin
            printf "Jcond inversed"; print_newline ();
            node.link.opcode <- Jcond (Misc.inverse_cond comp, other);
            node.link.args <- [ConstantBase(Const_label next)]
          end
    | _ -> ()
  in
  iter [node]

  (*********
  
  A very simplified version of code motion in the header.
  There are lots of requirements. We should be able to remove them
  with some more work.
  
  *******)
  
let rec code_motion func node =
  try
    let len = Array.length node.instrs in
    if len < 2 then raise Exit;
    let instrs = node.instrs in
    match instrs.(0) with
      { opcode = Subl; args = [Const(Const_int n); Register Esp] } ->
        begin
          match node.link with
            { opcode = Jcond (cond, next); 
              args = [ConstantBase(Const_label other)] } ->
              begin
                try     
                  match other.link with
                    { opcode = Ret } -> 
                      check_instrs len node other;
                      if !debug_opt4 then begin
                          printf "FOUND POSSIBLE CODE MOTION IN %s: %d %d" func.fun_name (Array.length instrs) (Array.length other.instrs);
                          print_newline () ;
                        end;
                      move_check func node other next
                  | _ -> raise Exit
                with _ ->
                    if List.length next.back_edges > 1 then raise Exit;
                    match next.link with
                      { opcode = Ret } -> 
                        check_instrs len node next;
                        move_check func node next other;
                    | _ -> ()    
              end
          | _ -> raise Exit
        end
    |  _ -> raise Exit
  with e -> ()


and check_instrs len node next =
  for i = 1 to len - 1 do
    match node.instrs.(i) with
      { opcode = Call _ }
    | { args = [Const(Const_int _); Register Esp] }
    | { opcode = Pushl }
    | { opcode = Popl }
    | { opcode = Alloc _ }
      -> raise Exit
    | _ -> ()
  done;
  let leno = Array.length next.instrs in
  if leno < 2 then raise Exit;
  for i = 0 to leno - 2 do
    match next.instrs.(i) with
      { opcode = Call _ }
    | { args = [Const(Const_int _); Register Esp] }
    | { opcode = Pushl }
    | { opcode = Popl }
    | { opcode = Alloc _ }
      -> raise Exit
    | _ -> ()
  done;
  match next.instrs.(leno - 1) with
    { opcode = Addl; args = [Const(Const_int _); Register Esp] } -> ()
  | _ -> raise Exit

and move_check func node return continue =
  if !debug_opt4 then begin 
      printf "FOUND POSSIBLE CODE MOTION IN %s: %d %d" 
        func.fun_name (Array.length node.instrs) (Array.length return.instrs) ;
      print_newline ();
    end;
  if not (List.length return.back_edges = 1 &&
      List.length continue.back_edges = 1) then begin 
      if !debug_opt4 then begin
          printf "ABORTED BECAUSE MULTIPLE BACK-EDGES";
          print_newline ();
        end;
    end else
  
  let len = Array.length node.instrs in
  if len > 0 then begin
      let comp = node.instrs.(len-1) in
      match comp with
        { opcode = Cmpl|Testl;
          args = [Register r1; Register r2];
          leave_instr = leave_state;
        } ->
          let v1 = leave_state.registers.(regindex r1) in
          let v2 = leave_state.registers.(regindex r2) in
          let comp = match v1, v2 with
              Xparam i1, Xparam i2 -> 
                comp.args <- [Register regs.(i1); Register regs.(i2)];
                [| comp |]
            | Xparam i1, Xconst c -> 
                comp.args <- [Register regs.(i1); Const c];
                [| comp |]
            | Xconst c, Xparam i2 -> 
                comp.args <- [Const c; Register regs.(i2)];
                [| comp |]
            | _ -> raise Exit
          in
          move_header func node return continue comp
      | { opcode = Cmpl|Testl;
          args = [Register r2; Const c];
          leave_instr = leave_state;
        } ->
          let v2 = leave_state.registers.(regindex r2) in
          let comp = match v2 with
              Xparam i -> 
                comp.args <- [Register regs.(i); Const c];
                [| comp |]
            | _ -> raise Exit
          in
          move_header func node return continue comp
      | { opcode = Cmpl|Testl;
          args = [Const c; Register r2];
          leave_instr = leave_state;
        } ->
          let v2 = leave_state.registers.(regindex r2) in
          let comp = match v2 with
              Xparam i -> 
                comp.args <- [Const c;Register regs.(i)];
                [| comp |]
            | Xop(op, [Xconst c; Xparam i]) ->
                let r = 
                  if node.instrs.(0).enter_instr.liveness.(regindex r2)=[] then
                    Register r2
                  else
                    find_dead node in
                comp.args <- [Const c; r];
                [|
                  mkinstr Movl [Register regs.(i); r] [];
                  mkinstr (opcode_of_operation op) [Const c; r] [];
                  comp
                |]                
            | _ -> raise Exit
          in
          move_header func node return continue comp
      | i -> ()
    end
    
and find_dead node =
  let enter_state = node.enter_node in
  assert( enter_state.liveness <> [||]);  
  let leave_state = node.link.leave_instr in
  assert(leave_state.liveness <> [||]);
  try
    for i = 6 downto 0 do
      if leave_state.liveness.(i) = [] &&
        enter_state.liveness.(i) = [] &&
        enter_state.registers.(i) = leave_state.registers.(i) then begin
          raise (Found i)
          end
    done;
    printf "No Dead reg found for move header"; print_newline ();
    raise Exit
  with Found i -> 
      Register regs.(i)
    
and move_header func node return continue comp =  
  let len = Array.length node.instrs in
  let header_cont = copy_instrs (Array.sub node.instrs 0 (len-1)) in
  let header_ret = copy_instrs (Array.sub node.instrs 1 (len-2)) in
  node.instrs <- comp;          
  let len_ret = Array.length return.instrs in
  return.instrs <- Array.concat [
    header_ret;
    Array.sub return.instrs 0 (len_ret - 1);
  ];
  continue.instrs <- Array.concat [
    header_cont;
    continue.instrs
  ];
  incr stat_move_header;
  restart_analysis := true;
  code_motion func continue


let stat_unroll = ref 0
  
let max_unroll_size = ref 15
  
  (* We unroll really simple loops, ie loops which only contain one block *)
let unroll_node node =
  if (not !no_unroll) && Array.length node.instrs < !max_unroll_size then
    match node.link with
      { opcode = Jcond(cond, next); args = [ConstantBase(Const_label other)] }
        when other == node || next == node ->
      
        (* We should check which part of the loop should be unroll. In
        particular, we can move only the first moves outside the loop. *)
        
        (try
          let len = Array.length node.instrs in
          for i = 0 to len - 1 do
            match node.instrs.(i) with
              { opcode = Movl } -> ()
            | _ ->
                  if !debug_opt4 then begin 
                      printf "UNROLL NODE %d" node.node_ident.label; print_newline ();
                      end;
                if i = 0 then raise Exit;
                let header = Array.sub node.instrs 0 i in
                let body = Array.sub node.instrs i (len-i) in
                let h2 = mknode () in
                let b = mknode () in
                node.instrs <- copy_instrs header;
                node.link <- mkinstr Jmp [ConstantBase(Const_label b)] [];
                h2.instrs <- copy_instrs header;
                  h2.link <- mkinstr Jmp [ConstantBase(Const_label b)] [];
                  b.back_edges <- [node; h2];
                  b.instrs <- copy_instrs body;
                  if other == node then
                    b.link <- mkinstr (Jcond(cond, next))
                    [ConstantBase(Const_label h2)] []
                  else
                    b.link <- mkinstr (Jcond(cond, h2))
                    [ConstantBase(Const_label other)] [];
                  h2.back_edges <- [b];
                  Cfg.remove_edge node node;
                restart_analysis := true;
                incr stat_unroll;
                raise Exit
          done          
        with _ -> ())
        (*
      if !debug_opt4 then begin 
          printf "UNROLL NODE %d" node.node_ident.label; print_newline ();
        end;
        restart_analysis := true;
        incr stat_unroll;
      let loop = { node with node_ident = mkident () } in
      loop.instrs <- copy_instrs node.instrs;
      loop.link <- copy_instr node.link;
      Cfg.add_edge node loop;
      Cfg.remove_edge node node;
      Cfg.add_edge loop loop;
      if other == node then begin
          node.link.args <- [ConstantBase(Const_label loop)];
          loop.link.args <- [ConstantBase(Const_label loop)];
          Cfg.add_edge loop next;
        end else begin
          node.link.opcode <- Jcond(cond, loop);
          loop.link.opcode <- Jcond(cond, loop);
          Cfg.add_edge loop other;
        end;
        *)
        
  | _ -> ()
      
  
let func func =
  
  if !debug_opt4 then begin
      printf "****************************** Optimizing(4) %s" func.fun_name;
      print_newline ();
    end;
  let old_stat_jump2jump = !stat_jump2jump in  
  (* code motion *)
  code_motion func func.code;

    (* remove predictable jumps *)
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes (iter_node []);

  (* In a second pass, since this can modify the edges without updating
  conservatively the back-edges. *)
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes jump2jump;
  if !stat_jump2jump <> old_stat_jump2jump then
    restart_analysis := true

let change_arg orig_stack_size instr_stack_size arg =
  match arg with
    OffsetBase(Const_int n, Esp) ->
      let diff = (instr_stack_size - orig_stack_size) * 4 in
      assert (diff >= 0);
      if n < diff then begin 
          printf "Stack: No offset"; print_newline ();
          arg 
        end else begin
          printf "Stack: offset"; print_newline ();
          OffsetBase(Const_int (n-8), Esp)
        end
  | _ -> arg

let change_frame orig_stack_size instr_stack_size frame =
  frame.size <- frame.size - 8;
  frame.pos <- List.map (fun pos ->
      if pos land 1 = 0 then (* a stack location *)
        let diff = (instr_stack_size - orig_stack_size) * 4 in
        assert (diff >= 0);
        if pos < diff then begin 
            printf "Frame: No offset"; print_newline ();
            pos end
        else begin
            printf "Frame: Offset"; print_newline ();
            pos-8
          end
      else pos
  ) frame.pos
      
let remove_trap stack_size node =
  let nodes = ref [] in
  let rec iter node =
    if not(List.memq node !nodes) then
      begin
        nodes := node :: !nodes;
        Array.iter (fun instr ->
            match instr with
              { opcode = Alloc (_,_, frame, _) } ->
                change_frame stack_size instr.enter_instr.stack_size frame
            | _ ->
                instr.args <- 
                  List.map (change_arg stack_size instr.enter_instr.stack_size)
                instr.args
        ) node.instrs;
        match node.link with
          { opcode = Poptrap next } ->
            node.instrs <- Array.concat [node.instrs; copy_instrs next.instrs];
            node.link <- copy_instr next.link
        | _ -> 
            let succs = node_succ node in
            List.iter iter succs
      end
  in
  iter node

let stat_try_removed = ref 0
  
let rec find_setuptrap node =
  match node.link with
    { opcode = Setuptrap (body, handler) } ->
      begin
        try
          if List.length body.back_edges <> 1 then raise Exit;
          let poptraps = ref [] in
          let nodes = ref [] in
          let rec iter node =
            if not (List.memq node !nodes) then
              begin
                nodes := node :: !nodes;
                Array.iter (fun instr ->
                    match instr with
                      { opcode = Call (Some _) } ->
                        printf "Call with alloc in find_setuptrap";
                        print_newline ();
                        raise Exit
                    | { opcode = Alloc _ } ->
                        printf "Alloc in find_setuptrap";
                        print_newline ();
                    | { opcode = CheckBound _ } ->
                        if !catch_bound_checks then begin
                            printf "Checkbound in find_setuptrap";
                            print_newline ();
                            raise Exit
                          end;
                    | _ -> ()                        
                ) node.instrs;
                match node.link with
                  { opcode = Poptrap next } -> 
                    poptraps := node :: !poptraps
                | { opcode = Jmp; args = [OffsetBase _] } ->
                    printf "Indirect jump in find_setuptrap";
                    print_newline ();
                    raise Exit
                | { opcode = Raise } -> 
                    printf "Raise in find_setuptrap";
                    print_newline ();
                    raise Exit
                | { opcode = Setuptrap _ } ->
                    printf "Try ... with ... find_setuptrap";
                    print_newline ();
                    raise Exit
                | _ -> 
                    let succs = node_succ node in
                    List.iter iter succs
              end
          in
          iter body;
          
          (* Now, we can replace the Setuptrap with a direct call to the body.
          *)
          node.link <- mkinstr Jmp [ConstantBase(Const_label body)] [];
          printf "find_poptrap finished";
          print_newline ();
          let stack_size = body.enter_node.stack_size + 1 in
          printf "Stack size %d" stack_size;
          print_newline ();
          assert (body.instrs.(0).opcode = Pushtrap);
          body.instrs <- Array.sub body.instrs 1 (Array.length body.instrs -1);
          remove_trap stack_size body;
          printf "Trap removed"; print_newline ();
          (* merge nodes *)
          node.instrs <- Array.concat [node.instrs; copy_instrs body.instrs];
          node.link <- copy_instr body.link;
          incr stat_try_removed;
          restart_analysis := true
        with Exit -> 
          printf "find_poptrap aborted";
          print_newline ();            
            ()
      end
  | _ -> ()

    
let remove_trywith func =
  (* This is complex. We have to follow the function code, find a
  Setuptrap and find inside the code if a raise can append.
  For now, we have to change the Esps ..., but not the frames, so
  we don't allow allocation inside the body ! This optimization is
  useless on standard caml code, but can appear useful:
  - after inlining (the inlined function cannot raise)
- after raise removal (if the enclosing try..with is found). *)
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes find_setuptrap

let jump2jump func =
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes jump2jump
