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

let stat_block_merged = ref 0
let stat_functions = ref 0
let stat_open_functions = ref 0 
  
let add_edge node1 node2 =
  node2.back_edges <- node1 :: node2.back_edges
  
let rec remove1 node1 list =
  match list with
    [] -> None
  | a :: tail -> 
      if a == node1 then Some tail else
      match remove1 node1 tail with
        None -> None
      | Some tail -> Some (a :: tail)

let rec remove2 node1 list =
  match list with
    [] -> []
  | a :: tail -> 
      if a == node1 then tail else
        a :: (remove2 node1 tail)
          
let rec remove_edge node1 node2 =
  node2.back_edges <- remove2 node1 node2.back_edges;
  if node2.back_edges = [] then
    begin
      let remove_edge = remove_edge node2 in 
      match node2.link with
        { opcode = Switch nodes } ->
          List.iter remove_edge nodes
      | { opcode = Setuptrap (node2, next) } ->
          remove_edge node2;
          remove_edge next
      | { opcode = Jmp; args = [ConstantBase (Const_label node2)] } ->
          remove_edge node2
      | { opcode = Jcond (_, next); 
          args = [ConstantBase (Const_label node2)] } ->
          remove_edge node2;
          remove_edge next
      | { opcode = Poptrap next } ->
          remove_edge next
      | { opcode = Startnode nodes } -> assert false
      | _ -> ()
    end

let start () =
  let nodes = List.map (fun global ->
        (Hashtbl.find program.desc global).code
    ) program.globals 
  in
  program.start_node.link <- mkinstr (Startnode nodes) [] [] 

let clear_back_edges () =
  Iter.reset ();
  Iter.add_node program.start_node;
  Iter.iter_nodes (fun node -> node.back_edges <- [])
  
let make_back_edges () =
  clear_back_edges ();
  Iter.reset ();
  Iter.add_node program.start_node;
  Iter.iter_edges add_edge

let nnodes = ref 0
  
let rec merge_blocks node1 =
  incr nnodes;
  match node1.link with
    { opcode = Jmp; 
      args= [ConstantBase(Const_label ({ back_edges = [n] } as node2))] } ->
      if n == node1 then
        begin
          incr stat_block_merged;
          node1.instrs <- Array.append node1.instrs node2.instrs;
          node1.link <- node2.link;
          List.iter (fun node ->
              add_edge node1 node;
              remove_edge node2 node;
          ) 
          (match node2.link with
              { opcode = Switch nodes } -> nodes
            | { opcode = Setuptrap (node,next) } -> [node; next]
            | { opcode = Jmp; args = [ConstantBase (Const_label node)] } -> 
                [node]
            | { opcode = Jcond (comp,next); 
                args = [ConstantBase (Const_label node)] } ->
                [node; next]
            | { opcode = Poptrap next } -> [next]
            | _ -> []);
          merge_blocks node1
        end else
        (Printf.printf "Cfg.merge_blocks: bad back-edge"; print_newline ());
  | { opcode = Jcond (cond,next); 
      args = [ConstantBase(Const_label other)] } ->      
      let instrs = other.instrs in
      if Array.length instrs > 0 then begin
          match instrs.(0) with
            { opcode = Call _; 
              args = [ConstantBase(Const_symbol "caml_array_bound_error" )] }
            ->
              node1.instrs <- Array.concat [node1.instrs; 
                [| mkinstr (CheckBound cond) [] [] |]];
              node1.link <- mkinstr Jmp [ConstantBase(Const_label next)] [];
              merge_blocks node1
          | _ -> ()
        end
  | _ -> ()
      
let make_merge_blocks () =
  make_back_edges ();
  Iter.reset ();
  Iter.add_node program.start_node;
  Iter.iter_nodes merge_blocks
  
let close_functions () =
  Iter.reset ();
  List.iter (fun global ->
      incr stat_functions;
      let func = Hashtbl.find program.desc global in
      func.fun_size <- 0;
      func.ret_nodes <- [];
      func.jmp_dest <- [];
      let node = func.code in
      Iter.add_node node;
      Iter.iter_nodes (fun node ->
          let len = Array.length node.instrs in
          func.fun_size <- func.fun_size + len+1;
          match node.link with
            { opcode = Ret } -> func.ret_nodes <- node :: func.ret_nodes
          | { opcode = Jmp; args = [ ConstantBase (Const_symbol s)] } ->
              func.jmp_dest <- s :: func.jmp_dest;
              func.ret_nodes <- node :: func.ret_nodes;              
          | { opcode = Jcond _; args = [ ConstantBase (Const_symbol s)] } ->
              assert false;
              func.jmp_dest <- s :: func.jmp_dest;
              func.ret_nodes <- node :: func.ret_nodes;
              | { opcode = Raise } ->
              func.ret_nodes <- node :: func.ret_nodes;
          | _ -> ()
      );
      if func.jmp_dest <> [] then incr stat_open_functions;
  ) program.globals
  
let stat_shared_blocks = ref 0
let stat_shared_instrs = ref 0
  
let share_blocks () =
  Iter.reset ();
  let table = Hashtbl.create 511 in
  List.iter (fun global ->
      let func = Hashtbl.find program.desc global in
      let node = func.code in
      Iter.add_node node;
      Iter.iter_nodes (fun node ->
          if Array.length node.instrs > 5 then
          let instrs = node.link :: (Array.to_list node.instrs) in
          let instrs = List.map (fun instr ->
                instr.opcode, instr.args) instrs in
          try             
            let node2 = Hashtbl.find table instrs in
            if match node2.node_names, node.node_names with
                [], [] -> true
              | _ :: _, _ :: _ -> true
              | _ -> false
            then
              begin                  
                incr stat_shared_blocks;
                stat_shared_instrs := !stat_shared_instrs + 
                Array.length node.instrs;
                node2.node_names <- node2.node_names @ node.node_names;
                node.node_ident <- node2.node_ident;
              end;
(*
            if node2.next then
              begin
                Printf.printf "Lost %d ins" (Array.length node2.instrs);
                print_newline ();
            end
              *)
          with
            Not_found ->
              Hashtbl.add table instrs node
      )
  )  program.globals

let stat_permute_cond  = ref 0

  (*
let permute_conds () =
  Iter.reset ();
(*      Iter.add_node program.start_node; *)
  List.iter (fun global ->
      let func = Hashtbl.find program.desc global in
      Iter.add_node func.code;
      Iter.iter_nodes (fun node ->
          match node.link with 
            { opcode = Jcond (cond, next); 
              args = [ ConstantBase (Const_label node1) ] } ->
            
            
            None -> () | Some node2 ->
              match node2.instrs with
                [| { opcode = Jmp } as instr|] ->
                  begin
                    let len = Array.length node.instrs in
                    if len>0 then
                      match node.instrs.(len-1) with
                        { opcode = Jcond cond; 
                          args = [ ConstantBase (Const_label node1) ] } ->
                          if not node1.next then
                            begin
                              remove_edge node node2;
                              node2.next <- false;
                              incr stat_permute_cond;
                              node.link <- Some node1;
                              node1.next <- true;
                              node.instrs.(len-1).opcode <- 
                                Jcond (Misc.inverse_cond cond);
                              node.instrs.(len-1).args <- instr.args;
                              (*
                              Printf.printf "Permute in %s" global;
                              print_newline () 
                              *)
                            end
                      | _ -> ()
                  end
              | _ -> ()
      )
  ) program.globals  
    

let stat_jump_jump = ref 0

let remove_jump_jump () =
  Iter.reset ();
  (*      Iter.add_node program.start_node; *)
  List.iter (fun global ->
      let func = Hashtbl.find program.desc global in
      Iter.add_node func.code;
      Iter.iter_nodes (fun node ->
          let len = Array.length node.instrs in
          if len>0 then
            let instr = node.instrs.(len-1) in
            match instr with
              { opcode = (Jcond _ | Call _ | Jmp); 
                args = [ ConstantBase (Const_label node1) ] } -> 
                begin
                  match node1.instrs with
                    [| { opcode = Jmp; 
                        args = [ ConstantBase(Const_label node2)] } as instr1
                    |] -> 
                      incr stat_jump_jump;
                      instr.args <- instr1.args;
                      add_edge node node2;
                      remove_edge node node1
                  | [| { opcode = Jmp } as instr1 |] -> 
                      incr stat_jump_jump;
                      remove_edge node node1;
                      instr.args <- instr1.args
                  | _ -> ()
                end
            | { opcode = Switch nodes } ->
                instr.opcode <- Switch (List.map (fun node1 ->
                      match node1.instrs with
                        [|
                          { opcode = Jmp; 
                            args = [ ConstantBase(Const_label node2)] } 
                            as instr1 |] -> 
                          incr stat_jump_jump;
                          add_edge node node2;                          
                          remove_edge node node1;
                          node2
                      | _ -> node1
                  ) nodes)
            | _ -> ()
      )
  ) program.globals  
*)  
  
  