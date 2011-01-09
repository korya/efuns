(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Args
open Asm

let stat_delete_switch = ref 0
let stat_delete_raise = ref 0
  
let switches = ref 0
  
let replace_instr node i instrs =
  let instrs = copy_instrs instrs in
  let len = Array.length instrs in
  let old_instrs = node.instrs in
  let old_len = Array.length old_instrs in
  match len with
    0 -> assert false (* delete ! *)
  | 1 -> old_instrs.(i) <- instrs.(0)
  | _ ->
      let new_len = old_len + len - 1 in
      let code = Array.create new_len old_instrs.(0) in
      Array.blit old_instrs 0 code 0 i;
      Array.blit instrs 0 code i len;
      Array.blit old_instrs (i+1) code (len + i) (old_len - i -1);
      node.instrs <- code
      
let mkswitch_data list = 
  incr stat_delete_switch;
  let symbol = Printf.sprintf "%s_switch_%d" program.name !switches in
  incr switches;
  let data = {
      data_name = symbol;
      data_header = 0;
      data_value = List.map (fun c -> Long c) list;
    } in
  program.data <- data :: program.data;
  symbol

let equal_instr i1 i2 = 
  i1.opcode = i2.opcode && i1.args = i2.args
  
let equal_instrs instrs1 instrs2 =
  try
    let len1 = Array.length instrs1 in
    let len2 = Array.length instrs2 in
    if len1 <> len2 then raise Exit;
    for i = 0 to len1 - 1 do
      let i1 = instrs1.(i) in
      let i2 = instrs2.(i) in
      if not (equal_instr i1 i2) then raise Exit;
    done;
    true
  with
    Exit -> false
  
let try_replace_switch node0 index nodes node_in =
  let instrs = node0.instrs in
  let len = Array.length instrs in
  if len > 0 then
    match instrs.(0) with
      { opcode = Movl; 
        args = [ Const(_); Register r0]; } ->
        let end_instrs = Array.sub instrs 1 (len-1) in
        let consts = 
          List.map (fun node ->
              if Array.length node.instrs = len then
                match node.instrs.(0) with
                  { opcode = Movl; args = [ Const(c); Register r]; } 
                    when r = r0 &&
                  equal_instrs (Array.sub node.instrs 1 (len-1))  end_instrs &&
                  equal_instr node.link node0.link -> c
                | _ -> raise Exit
              else raise Exit
          ) nodes in
        let symbol = mkswitch_data consts in
        let instrs = copy_instrs instrs in
        instrs.(0) <- mkinstr Movl [OffsetIndexScale(
            Const_symbol symbol,
            index, 4); Register r0] [];
        let node = mknode () in
        node.instrs <- instrs;
        node.link <- node0.link;
        node_in.link <- mkinstr Jmp [ConstantBase(Const_label node)] []
    | _ -> raise Exit
  else raise Exit
    
let bits8 r =
  match r with
    Eax -> LowByte Eax
  | Ebx -> LowByte Ebx
  | Ecx -> LowByte Ecx
  | Edx -> LowByte Edx
  | _ -> raise Exit
      
let try_replace_test comp next node0 node_in =
  let instrs = node0.instrs in
  let len = Array.length instrs in
  if len > 0 && len = Array.length next.instrs && 
    equal_instr node0.link  next.link then
    match instrs.(0), next.instrs.(0) with
      { opcode = Movl; 
        args = [ Const(c1); Register r1]; }, (* c1 if cond true *)
      { opcode = Movl; 
        args = [ Const(c2); Register r2]; }  (* c2 if cond false *)
      when r1 = r2
      ->
        let index = bits8 r1 in
        let end_instrs = Array.sub instrs 1 (len-1) in
        if equal_instrs (Array.sub next.instrs 1 (len-1)) end_instrs then
          let symbol = mkswitch_data [c2;c1] in
          let instrs = copy_instrs end_instrs in
          let instrs = Array.concat
              [[|
              mkinstr (SetCond comp) [Register index] [];
              mkinstr Andl [Const(Const_int 1); Register r1] [];
              mkinstr Movl [OffsetIndexScale(
                  Const_symbol symbol,
                  r1, 4); Register r1] [];
            |];
              instrs] in
          let node = mknode () in
          node.instrs <- instrs;
          node.link <- node0.link;
          node_in.link <- mkinstr Jmp [ConstantBase(Const_label node)] []
    | _ -> raise Exit
  else raise Exit
    
    
    (*
  
    testl reg, reg
    jcond (comp, next), [jump]
    where next = jump mod movl $C, reg2
  
    -->
  
    testl reg, reg
    setcond comp, [ reg2(bits8) ]
    andl $1, reg2
    movl .L(,reg2,4), reg2
  *)

let name = ref ""
    
let remove_switch node =
  try
    match node.link with
      { opcode = Switch ((node0::_) as nodes); 
        args = [ Register index ] } ->                      
        begin
          let instrs = node0.instrs in
          let len = Array.length instrs in
          try
            try_replace_switch node0 index nodes node;
            if !debug_po then begin
                Printf.printf "switch simplified in %s (%d ins)" 
                  !name len ;
                print_newline ();
              end;
          with _ ->
              List.iter (fun node ->
                  if not (equal_instrs node.instrs node0.instrs &&
                      equal_instr node.link node0.link) then
                    raise Exit) nodes;
              node.link <- 
                mkinstr Jmp [ConstantBase(Const_label node0)] [];
              incr stat_delete_switch;
              if !debug_po then begin
                  Printf.printf "switch removed in %s" !name;
                  print_newline ();
                end;
        end
    | { opcode = Jcond (comp, next);
        args = [ConstantBase(Const_label node0)] } ->
        begin
          try
            try_replace_test comp next node0 node ;
            if !debug_po then begin
                Printf.printf "test simplified in %s" !name;
                print_newline ();
              end;
          with
            _ ->
              if equal_instrs next.instrs node0.instrs &&
                equal_instr next.link node0.link then
                begin
                  incr stat_delete_switch;
                  if !debug_po then begin
                      Printf.printf "jcond removed in %s" !name;
                      print_newline ();
                    end;
                  node.link.opcode <- Jmp;
                end
        end
    | _ -> ()
  with  _ -> ()

let rec compute_stack_size tail size =
  match tail with
    [] -> size
  | node :: nodes ->
      let size = ref size in
      let len = Array.length node.instrs in
      for i = 0 to len - 1 do
        match node.instrs.(i) with
          { opcode = Pushtrap } -> size := !size + 8;
        | { opcode = Addl; args = [ Const(Const_int n); Register Esp] } ->
            size := !size - n
        | { opcode = Subl; args = [ Const(Const_int n); Register Esp] } ->
            size := !size + n
        | { opcode = Pushl } -> size := !size + 4
        | { opcode = Popl } -> size := !size - 4
        | _ -> ()
      done;
      begin
        match node.link.opcode with
          Poptrap _ -> size := !size - 8
        | _ -> ()
      end;
      compute_stack_size nodes !size
      
      
let remove_raise node =
  match node.link with
    { opcode = Raise } -> begin
        let rec iter nodes nexts all_nodes =
          match nodes with
            (node, depth, path) :: nodes ->
              if List.memq node all_nodes then
                iter nodes nexts all_nodes
              else begin
                  let all_nodes = node :: all_nodes in
                  match node.link with
                    { opcode = Poptrap next } ->
                      assert (next == List.hd path);
                      iter nodes ((node, depth+1, path):: nexts) all_nodes
                  | { opcode = Setuptrap (body, next) } ->
                      if next == List.hd path then
                        iter nodes ((node, depth, path) :: nexts) all_nodes
                      else begin
                          assert (body == List.hd path);
                          if depth = 0 then begin (* FOUND *)
                              node :: path
                            end else begin
                              iter nodes ((node, depth-1, path) :: nexts) 
                              all_nodes
                            end                            
                        end
                  | _ -> iter nodes ((node, depth, path) :: nexts) all_nodes
                end
          
          | [] ->
              match nexts with
                [] -> []
              | _ -> iter (List.flatten (List.map 
                        (fun (node, depth, path) ->
                          List.map (fun bnode ->
                              (bnode, depth, node :: path)) node.back_edges
                      ) nexts)) [] all_nodes
        in
        let path = iter [] [(node, 0, [])] [node] in
        match path with
          [] -> ()
        | first :: tail -> 
            match first.link with
              { opcode = Setuptrap(body, handler) } ->
                incr stat_delete_raise;
                let stack_size = compute_stack_size tail 0 in
                if stack_size > 8 then 
                  node.instrs <- Array.concat [ node.instrs;
                    [| 
                      mkinstr Addl [Const(Const_int (stack_size-8)); 
                        Register Esp] []
                    |] ];
                let len = Array.length handler.instrs in
                let handler = if len < 7 then
                    copy_one_node handler
                  else handler in
                node.link.opcode <- Poptrap handler;                
                node.link.args <- []
            |  _ -> assert false
      end
  | _ -> ()

      
let simplify () =    
  
  List.iter (fun global ->
      let func = Hashtbl.find program.desc global in
      
      name := func.fun_name;
      Iter.reset ();            
      Iter.add_node func.code;
      Iter.iter_nodes remove_switch;
  ) program.globals;
  
  Cfg.make_back_edges ();
  
  (* NOTE: we don't update the back-edges after a raise has been replaced by
  a Jump to avoid a new raise to believe it is in the body of the previous 
  raise. *)
  List.iter (fun global ->
      let func = Hashtbl.find program.desc global in      
      name := func.fun_name;
      Iter.reset ();            
      Iter.add_node func.code;
      Iter.iter_nodes remove_raise
  ) program.globals;

