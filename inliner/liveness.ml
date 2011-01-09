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

let find_indirect_tailcall_liveness instr =
  let len = nregs + instr.enter_instr.stack_size in
  let liveness = Array.create len [] in
  try
    List.iter (fun dir ->
        match dir with
          Dir_tailcall -> ()
        | Dir_live regs ->
            List.iter (fun reg -> 
                match reg with 
                  Register reg ->
                    begin
                      try
                        liveness.(regindex reg) <- [instr];
                      with _ -> assert false
                    end
                | _ -> assert false ) regs;
            raise Exit
        | Dir_args regs ->
            List.iter (fun reg -> 
                match reg with 
                  Register reg ->
                    begin
                      try
                        liveness.(regindex reg) <- [instr];
                      with _ -> assert false
                    end
                | _ -> assert false ) regs;
            raise Exit
        | _ -> ()) instr.directives;
    (* No information found !!! *)
    assert false
  with _ -> liveness
      
let find_tailcall_liveness s instr =
  let len = nregs + instr.enter_instr.stack_size in
  let liveness = Array.create len [] in
  try
    let func = try Hashtbl.find program.desc s
      with Not_found -> Hashtbl.find program.env s
    in
    assert (func.arity <= nparams);
    for i = 0 to func.arity - 1 do
      liveness.(i) <- [instr];
    done;
    (*
    for i = 1 to func.arity - nparams do
          liveness.(len-1-i) <- [instr];
    done;
    *)
    liveness
  with Not_found -> find_indirect_tailcall_liveness instr
      (*
      try
        List.iter (fun dir ->
            match dir with
              Dir_tailcall -> ()
            | Dir_live regs ->
                List.iter (fun reg -> 
                    match reg with 
                      Register reg ->
                        begin
                          try
                            liveness.(regindex reg) <- [instr];
                          with _ -> assert false
                        end
                    | _ -> assert false ) regs;
                raise Exit
            | _ -> ()) instr.directives;
                        (* No information found !!! *)
        assert false
      with _ -> liveness
*)          
  
let use_register live_before instr reg =
  try
    let r = regindex_all reg in
    live_before.(r) <- instr :: live_before.(r);
  with _ -> ()
  
let rec source live_before instr arg = 
  match arg with
    Register reg -> 
      use_register live_before instr reg
  | OffsetBase (Const_int n, Esp) ->
      let len = Array.length live_before in
      let index = len - 1 - (n/4) in
      live_before.(index) <- instr :: live_before.(index)
  | Indirect r -> 
      source live_before instr r
  | _ -> ()
      
let address live_before instr arg =
  match arg with
  | OffsetBase (c,reg) -> 
      use_register live_before instr reg
  | OffsetBaseIndex (c,reg1,reg2) ->  
      use_register live_before instr reg1;
      use_register live_before instr reg2
  | OffsetBaseIndexScale (c,reg1,reg2,s) -> 
      use_register live_before instr reg1;
      use_register live_before instr reg2      
  | OffsetIndexScale (c, reg, s) ->
      use_register live_before instr reg
  | _ -> ()

let addresses live_before instr args =
  List.iter (address live_before instr) args
      
let rec add_used_by instr instrs =
  match instrs with
    [] -> ()
  | i :: tail ->
      if not (List.memq i instr.used_by) then
        instr.used_by <- i :: instr.used_by;
      add_used_by instr tail
      
let side_effect instr = 
  if !debug_live then
    (Printf.printf "SIDE EFFECT"; print_newline ());
  add_used_by instr [instr]

let print_instr_list list =
  List.iter (fun instr -> Printf.printf "%d " instr.instr_num) list
  
let destination live_before live_after instr arg = 
  match arg with
    Register reg ->
      begin
        try
          let index = regindex_all reg in
          add_used_by instr live_after.(index);
          live_before.(index) <- []
        with _ -> 
            side_effect instr
      end
      
  | OffsetBase (Const_int n, Esp) ->
      let len = Array.length live_after in
      let index = len - 1 - (n/4) in
      add_used_by instr live_after.(index);
      if index < Array.length live_before then
        live_before.(index) <- []
  | OffsetBase _ 
  | OffsetBaseIndex _ 
  | OffsetBaseIndexScale _
  | OffsetIndexScale _ 
  | ConstantBase _ ->
      (* This instruction modifies the memory *)
      side_effect instr
  | _ -> ()

let use_directives live_before live_after instr =
  try
    List.iter (fun dir ->
        match dir with
          Dir_res args -> 
            List.iter (destination live_before live_after instr) args;
            raise Exit
        | _ -> ()
    ) instr.directives;
    Printf.printf "On instr: %s" (SimplePrint.string_of_instr instr);
    print_newline ();
    assert false
  with Exit ->
      
      try
        List.iter (fun dir ->
            match dir with
              Dir_args args ->       
                if !debug_live then 
                  Printf.printf "NARGS: %d " (List.length args);
                List.iter (fun arg ->
                    if !debug_live then 
                      Printf.printf "(arg: %s)" (SimplePrint.string_of_arg arg);
                    source live_before instr arg) args;
                raise Exit
            | _ -> ()
        ) instr.directives;
        assert false
      with Exit -> ()  

let compute_instr live_after instr =
  instr.leave_instr.liveness <- live_after;
  let len_after = Array.length live_after in
  let len_before = nregs + instr.enter_instr.stack_size in
  let live_before = Array.create len_before [] in
  for i = 0 to (min len_before len_after) - 1 do
    live_before.(i) <- live_after.(i)
  done;
  instr.enter_instr.liveness <- live_before;
  begin
    match instr with
      { opcode = Call _ ; args = [src] } ->
        (* If it is a C call, we must say that some values in the stack
      are live ... *)
        if !debug_live then begin
            Printf.printf "USE DIRECTIVES (%s)" (SimplePrint.string_of_instr instr);
            print_newline ();
          end;
        use_directives live_before live_after instr;
        source live_before instr src;
        side_effect instr
    
    | { opcode = (
          Movl | Movzbl | Movzwl | Movsbl | Movswl | Movb | Movw | Leal | Lea
        ); args = [src; dst] } ->
        if src <> dst then
          begin
            destination live_before live_after instr dst;
            source live_before instr src;
          end
          
    | { opcode = (
          Addl |Subl
        | Shrl |Sarl |Sall
        | Rcrl | Rcll
        | Xorl | Andl | Orl
        ); 
        args = [arg1;arg2]} ->
        destination live_before live_after instr arg2;
        source live_before instr arg1;
        source live_before instr arg2;
    | { opcode = Idivl; args = [src;dst] } ->
        Printf.printf "Idivl with two args"; print_newline ();
        destination live_before live_after instr dst;
        destination live_before live_after instr (Register Edx);
        source live_before instr src;
        source live_before instr dst;
    | { opcode = Idivl; args = [src] } ->
        destination live_before live_after instr (Register Eax);
        destination live_before live_after instr (Register Edx);
        source live_before instr src;
        source live_before instr (Register Eax);
    | { opcode = (Testl | Cmpl | Cmpi _) } ->
        List.iter (source live_before instr) instr.args;
        addresses live_before instr instr.args;
        side_effect instr
    
    | { opcode = Alloc _; args = [dst] } ->
        destination live_before live_after instr dst
    
    | { opcode = SetCond _; args = [dst] } ->
        destination live_before live_after instr dst
    
    | { opcode = Popl; args = [dst] } ->
        destination live_before live_after instr dst;
        source live_before instr (OffsetBase(Const_int 0, Esp));
        side_effect instr
    
    | { opcode = Pushl; args = [src] } ->
        destination live_before live_after instr 
          (OffsetBase (Const_int 0, Esp));
        source live_before instr src;
        side_effect instr
    
    | _ -> 
    (* compute the destination first *)
        List.iter (fun arg -> 
            destination live_before live_after instr arg) instr.args;
        List.iter (fun arg -> source live_before instr arg) instr.args;
        side_effect instr
  end;
  addresses live_before instr instr.args;
  
  if !debug_live then
    begin
      Printf.printf "AFTER:"; print_newline ();
      print_state instr.leave_instr;
      Printf.printf "LIVE[%d]: %s" instr.instr_num (
        SimplePrint.string_of_instr instr);
      print_newline ();
      Printf.printf "USED BY: ";
      List.iter (fun i -> Printf.printf "%d " i.instr_num) instr.used_by;
      print_newline () ;
      Printf.printf "BEFORE:"; print_newline ();
      print_state instr.enter_instr;
      print_newline ();
    end;  
  live_before
          
let rec compute_node from_node liveness node =
  if node == program.start_node then () else
  let _ = () in
  if !debug_live then begin
      Printf.printf "NODE %d" node.node_ident.label;
      print_newline (); 
      print_state node.enter_node;
    end;
  if node.leave_node == noapprox then begin
      if !debug_live then begin
          Printf.printf "array bound error found:"; 
          print_newline ();
          Array.iter (fun instr ->
              Printf.printf "%s" (SimplePrint.string_of_instr instr);
              print_newline ()) node.instrs;
          Printf.printf "%s" (SimplePrint.string_of_instr node.link);
          print_newline ();
          Printf.printf "END"; print_newline ();
          match node.link with
            { opcode = Startnode _ } ->
              Printf.printf "Startnode !!!"; print_newline ();
              if node <> program.start_node then begin
                  Printf.printf "Bad start node !";
                  print_newline ();
                end else begin
                  Printf.printf "Standard start node";
                  print_newline ();
                end
                
          | _ -> ()
        end else ();
      (* mark all instrs as side-effected *)
      assert false;
      Array.iter (fun instr -> side_effect instr) node.instrs;
      side_effect node.link;
    end else    
  let instr = node.link in
  try
    let liveness =
      match instr with
        { opcode = Ret | Raise } ->
          let liveness = Array.create
              (nregs + instr.leave_instr.stack_size) [] in
          liveness.(eax) <- [instr];
          liveness
      | { opcode = Jmp; args = [ConstantBase(Const_symbol s)] } ->
          find_tailcall_liveness s instr
      | { opcode = Jmp; args = [Indirect (Register _)] } ->
          find_indirect_tailcall_liveness instr
      | instr -> 
          let liveness = match instr with
              { opcode = Setuptrap(body, next) } ->
                if body == from_node then
                  Array.sub liveness 0 (Array.length liveness - 1)
                else
                  liveness
            | _ -> liveness
          in
          match instr.leave_instr.liveness with
            [||] -> 
              if !debug_live then
                (Printf.printf "NO LIVENESS"; print_newline ());
              if liveness = [||] then begin (* probably a jump ... *)
                  Array.create (instr.leave_instr.stack_size + 7) []
                end else
                Array.copy liveness
          | old_liveness -> 
              if !debug_live then
                (Printf.printf "Merging"; print_newline ());
              let len = Array.length liveness in
              if liveness = [||] then raise Exit; (* already done *)
              assert (Array.length old_liveness = len);
              let modified = ref false in
              for i = 0 to len-1 do
                List.iter (fun instr -> 
                    if not (List.memq instr old_liveness.(i)) then 
                      begin
                        modified := true;
                        old_liveness.(i) <- instr :: old_liveness.(i)
                      end) liveness.(i)
              done;
              if !modified then old_liveness else raise Exit
    in
    side_effect node.link;
    let liveness = compute_instr liveness node.link in
    let rec iter liveness i =
      if i >= 0 then
        let instr = node.instrs.(i) in
        iter (compute_instr liveness instr) (i-1)
      else
        liveness
    in
    let len = Array.length node.instrs in
    let liveness = iter liveness (len-1) in
    let old_liveness = node.enter_node.liveness in
    if old_liveness = [||] || 
      (try
          let len = Array.length liveness in
          for i = 0 to len - 1 do
            List.iter (fun instr ->
                if not (List.memq instr old_liveness.(i)) then raise Exit  
            ) liveness.(i)
          done;
          false
        with Exit -> true) then
      begin
        node.enter_node.liveness <- liveness;
        if !debug_live then begin          
            Printf.printf "Liveness changed (n backedges:%d)" 
              (List.length node.back_edges);
            print_newline ();
          end;
        List.iter (compute_node node liveness) node.back_edges
      end else
    if !debug_live then
      (Printf.printf "Liveness at entry not changed"; print_newline ());
      
  with Exit -> 
      if !debug_live then
        (Printf.printf "Nothing to do for node %d" node.node_ident.label;
          print_newline ());
      ()

let print_instr instr =
  print_newline ();
  print_state instr.enter_instr;
  Printf.printf "INSTR[%d]: %s" instr.instr_num
    (SimplePrint.string_of_instr instr); 
  print_newline ();
  Printf.printf "USED BY: ";
  List.iter (fun i -> Printf.printf "%d " i.instr_num) instr.used_by;
  print_newline ();
  if instr.used_by = [] then
    (Printf.printf "DEAD INSTRUCTION"; print_newline ())
      
let print_node node =
  Printf.printf "NODE %d" node.node_ident.label;
  print_newline ();
  Array.iter print_instr node.instrs;
  print_instr node.link;  
  print_state node.link.leave_instr
      
let compute func =
  if !debug_live then
    (print_newline ();print_newline ();
      Printf.printf "****************************** Liveness %s" func.fun_name;
      print_newline ());
  let ret_nodes = func.ret_nodes in
  if !debug_live then begin
      Printf.printf "Ret nodes : ";
      List.iter (fun node -> Printf.printf "%d " node.node_ident.label) func.ret_nodes;  
      print_newline ();
    end;
  if func.ret_nodes = [] then begin
      (* Il faut commencer le calcul a partir d'un node pointant vers le 
  permier ici *)
      List.iter (fun node ->
          let instr = node.link in
          let len = nregs + instr.enter_instr.stack_size in
          let liveness = Array.create len [] in
          compute_node func.code liveness node
      ) func.code.back_edges      
    end    
  else
    List.iter (fun node -> compute_node node [||] node) func.ret_nodes;
  if !debug_result then
    begin
      print_newline ();
      print_newline ();
      Printf.printf "****************************** RESULT %s" func.fun_name; print_newline ();
      Iter.reset ();
      Iter.add_node func.code;
      Iter.iter_nodes print_node
    end