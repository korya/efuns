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

open Misc
open Asm
open Printf

let stat_jump_next = ref 0
  
let frames = ref []
(* let nodes = Hashtbl.create 133 *)
let floats = Hashtbl.create 31
let nframes = ref 0
let new_nodes = ref []
let previous_node = ref (mknode ())
  
let label_of_node node =
  if not node.node_ident.emitted then
    begin
      new_nodes := node :: !new_nodes
    end;
  node.node_ident.label
   
let label_of_frame frame =
  try
    List.assq frame !frames
  with Not_found ->
      let ident = mkident () in
      incr nframes;
      frames := (frame, ident.label) :: !frames;
      ident.label
   
let label_of_float f =
  try
    Hashtbl.find floats f.float
  with
    Not_found ->
      let label = f.float_ident.label in
      Hashtbl.add floats f.float label;
      label

let instr_counter = ref 0
let count_instr instr =
  instr.instr_num <- !instr_counter;
  incr instr_counter
      
let reset_emit () =
  frames := [];
  nframes := 0;
(*  Hashtbl.clear nodes; *)
(*  nnodes := 0; *)
  new_nodes := [];
  Hashtbl.clear floats;
  previous_node := mknode ();
  instr_counter := 0;
  Iter.reset ();
  Iter.add_node program.start_node;
  Iter.iter_nodes (fun node ->
      Array.iter count_instr node.instrs;
      count_instr node.link)
  
(* DATA SEGMENT *)  

let emit_string oc s =
  let last_was_escape = ref false in
  Printf.fprintf oc "\"";
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.fprintf oc "\\%o" (Char.code c)
      else output_char oc c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
        output_char oc c;
        last_was_escape := false
      end else begin
        Printf.fprintf oc "\\%o" (Char.code c);
        last_was_escape := true
      end
  done;
  Printf.fprintf oc  "\""

let rec emit_const oc constant =
  match constant with
    Const_int int -> Printf.fprintf oc "%d" int
  | Const_symbol string -> Printf.fprintf oc "%s" string
  | Const_label node -> assert false
(*      Printf.fprintf oc ".L%d" (label_of_node node) *)
  | Const_data_label string -> Printf.fprintf oc "%s" string
  | Const_sub (c1,c2) ->
      emit_const oc c1; Printf.fprintf oc " - "; emit_const oc c2    
  | Const_add (c1,c2) ->  
      emit_const oc c1; Printf.fprintf oc " + "; emit_const oc c2
  | Const_float _ -> assert false
  | Const_mul (c1,c2) ->
      Printf.fprintf oc "(";
      emit_const oc c1; 
      Printf.fprintf oc ") + ("; 
      emit_const oc c2;
      Printf.fprintf oc ")"
      
let emit_item oc item =
  match item with
  | Long constant ->
      Printf.fprintf oc "\t.long\t"; emit_const oc constant
  | Ascii string ->
      Printf.fprintf oc "\t.ascii\t"; emit_string oc string
  | Word constant ->
      Printf.fprintf oc "\t.word\t"; emit_const oc constant
  | Byte constant ->
      Printf.fprintf oc "\t.byte\t"; emit_const oc constant
  | Space int ->
      Printf.fprintf oc "\t.space\t%d" int
  | Align int ->
      Printf.fprintf oc "\t.align\t%d" int
  | Skip int ->         
      Printf.fprintf oc "\t.skip\t%d" int
  | String string ->         
      Printf.fprintf oc "\t.string\t%s" string
  | Label string ->         
      Printf.fprintf oc "%s:" string
  | Double d ->
      Printf.fprintf oc "\t.double\t%f" d
      
let emit_data oc data =
  let rec iter data =
    match data with
      [] -> ()
    | data :: tail ->
        Printf.fprintf oc "\t.align\t4\n";
        Printf.fprintf oc "\t.long\t%d\n" data.data_header;
        Printf.fprintf oc "\t.globl\t%s\n" data.data_name;
        Printf.fprintf oc "%s:\n" data.data_name;
        iter_items data.data_value;
        iter tail
  and iter_items items =
    match items with
      [] -> ()
    | item :: tail ->
        emit_item oc item;
        Printf.fprintf oc "\n";
        iter_items tail
  in
  iter data
  
(* TEXT SEGMENT *)

module Print = Asm.Print(struct
      let label_of_node = label_of_node
      let label_of_frame = label_of_frame
      let new_label () = let id = mkident () in id.label
      let label_of_float = label_of_float
    end)
  
let num_nodes = ref 0  

let bound_error_node = ref None
    
let rec iter_node oc node =
  if not node.node_ident.emitted then
    let _ =
      if node.node_names <> [] then
        begin
          Printf.fprintf oc "\t.align\t4\n";
          List.iter (fun global ->
              Printf.fprintf oc "\t.globl\t%s\n" global;
              Printf.fprintf oc "%s:\n" global) node.node_names;
        end
    in let _ =
      if 
        match node.back_edges with
          [node2] -> 
            let _ = not (!previous_node.node_ident == node2.node_ident) in
            true (* Tant que les noeuds ne sont pas bien indiquees *)
        | _ -> true
      then
        let label = label_of_node node in
        Printf.fprintf oc ".L%d:\n" label;        
    in
    node.node_ident.emitted <- true;
    previous_node := node;
    incr num_nodes;
    iter_instrs oc node.instrs;
    let instr = node.link in
    match instr with
      { opcode = Jmp; args = [ ConstantBase(Const_label next) ]} 
        when not next.node_ident.emitted && next.node_names = [] ->
        decr instr_counter;
        iter_node oc next
    | { opcode = Setuptrap (trywith, next) } 
        when not next.node_ident.emitted && next.node_names = [] ->
        Printf.fprintf oc "\tcall\t.L%d\n" (label_of_node trywith);
        iter_node oc next
    | { opcode = Jcond (comp, next); args= [arg] } 
        when not next.node_ident.emitted && next.node_names = [] ->
        Printf.fprintf oc "\tj%s\t%s\n" (Print.string_of_comp comp) 
        (Print.string_of_arg arg);
        iter_node oc next
    | { opcode = Poptrap next } 
        when not next.node_ident.emitted && next.node_names = [] ->
        Printf.fprintf oc "\tpopl\tcaml_exception_pointer\n\taddl\t$4, %s\n"
          "%esp";
        iter_node oc next
    | instr ->
        Printf.fprintf oc "%s\n" (Print.string_of_instr instr)

and iter_instrs oc instrs =
  for i = 0 to Array.length instrs - 1 do
    let instr = instrs.(i) in
    match instr with
      { opcode = CheckBound comp } ->
        Printf.fprintf oc "\tj%s\t.L%d\n" (Print.string_of_comp comp) 
        (match !bound_error_node with
              None -> let node = mknode () in
                bound_error_node := Some node; node
          | Some node -> node).node_ident.label
    | _ -> 
        Printf.fprintf oc "%s\n" (Print.string_of_instr instr)
  done
  
let rec iter_new_nodes oc =
  match !new_nodes with
  | node :: tail ->
      new_nodes := tail;
      iter_node oc node;
      iter_new_nodes oc
  | [] -> ()
    
let emit_code oc program = 
  List.iter (fun global ->
      bound_error_node := None;
      iter_node oc (Hashtbl.find program.desc global).code;
      iter_new_nodes oc;
      match !bound_error_node with
        None -> ()
      | Some node ->
          Printf.fprintf oc ".L%d:\tcall\tcaml_array_bound_error\n"
            node.node_ident.label;
  ) program.globals;
  assert (!new_nodes = [])
  
let emit_frametable oc =  
  Printf.fprintf oc "\t.long\t%d\n" (List.length !frames);
  let rec iter list =
    match list with
      [] -> ()
    | (frame, label) :: tail ->
        Printf.fprintf oc "\t.long\t.L%d\n" label;
        Printf.fprintf oc "\t.word\t%d\n" frame.size;
        Printf.fprintf oc "\t.word\t%d\n" (List.length frame.pos);
        List.iter (fun pos ->
            Printf.fprintf oc "\t.word\t%d\n" pos) frame.pos;
        Printf.fprintf oc "\t.align\t4\n";
        iter tail
  in
  iter !frames
  
let emit oc program = 
  reset_emit ();
  num_nodes := 0;
  
  (* DATA SEGMENT *)  
  Printf.fprintf oc ".data\n";
  Printf.fprintf oc "\t.globl\t%s_data_begin\n" program.name;
  Printf.fprintf oc "%s_data_begin:\n" program.name;
  if not !Args.no_data then
    emit_data oc program.data;  
  Printf.fprintf oc "\t.globl\t%s_data_end\n" program.name;
  Printf.fprintf oc "%s_data_end:\n" program.name;
  Printf.fprintf oc "\t.long\t0\n";
  
  (* TEXT SEGMENT *)
  Printf.fprintf oc ".text\n";
  Printf.fprintf oc "\t.globl\t%s_code_begin\n" program.name;
  Printf.fprintf oc "%s_code_begin:\n" program.name;
  emit_code oc program;
  Printf.fprintf oc "\t.globl\t%s_code_end\n" program.name;
  Printf.fprintf oc "%s_code_end:\n" program.name;
  
  (* FRAMETABLE *)
  Printf.fprintf oc ".data\n";
  Printf.fprintf oc "\t.globl\t%s_frametable\n" program.name;
  Printf.fprintf oc "%s_frametable:\n" program.name;
  
  if not !Args.no_data then
    emit_frametable oc;
  
  (* FLOATS *)
  if not !Args.no_data then
    Hashtbl.iter (fun f label ->
        Printf.fprintf oc "\t.L%d:\n\t.double\t%f\n" label f
    ) floats  
    