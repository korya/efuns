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
open Args

let nodes = Hashtbl.create 111
let new_nodes = ref []
let graph = ref (Ocamldot.create "cfg" [])
  
let add_node node =
  try
    Hashtbl.find nodes node.node_ident.label
  with
    Not_found ->
      let dot = Ocamldot.node !graph "" [Ocamldot.NodeShape Ocamldot.Box] in
      let res = (dot, ref false) in
      Hashtbl.add nodes node.node_ident.label res;
      res

let reset_cfg g =
  graph := g;
  Hashtbl.clear nodes;
  new_nodes := []

external is_printable: char -> bool = "is_printable"
external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

open String
  
let escaped s =
  let n = ref 0 in
  let modified = ref false in
  for i = 0 to String.length  s - 1 do
    n := !n +
      (match unsafe_get s i with
        '"' | '\\' | '\n' -> modified := true; 2
      | '\t' -> modified := true; 1
      | c -> if is_printable c then 1 else (modified := true; 4))
  done;
  if not !modified then s else begin
      let s' = create !n in
      n := 0;
      for i = 0 to length s - 1 do
        begin
          match unsafe_get s i with
            ('"' | '\\') as c ->
              unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
          | '\n' ->
              unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
          | '\t' -> 
              unsafe_set s' !n '\t'
          | c ->
              if is_printable c then
                unsafe_set s' !n c
              else begin
                  let a = char_code c in
                  unsafe_set s' !n '\\';
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a / 100));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a mod 10))
                end
        end;
        incr n
      done;
      s'
    end

let title_escaped s =
  let n = ref 0 in
  let modified = ref false in
  for i = 0 to String.length  s - 1 do
    n := !n +
      (match unsafe_get s i with
        '"' | '\\' | '\n' -> modified := true; 2
      | '\t' | '$' -> modified := true; 1
      | c -> if is_printable c then 1 else (modified := true; 4))
  done;
  if not !modified then s else begin
      let s' = create !n in
      n := 0;
      for i = 0 to length s - 1 do
        begin
          match unsafe_get s i with
            ('"' | '\\') as c ->
              unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
          | '\n' ->
              unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
          | '\t' -> 
              unsafe_set s' !n '\t'
          | '$' ->
              unsafe_set s' !n 'S'
          | c ->
              if is_printable c then
                unsafe_set s' !n c
              else begin
                  let a = char_code c in
                  unsafe_set s' !n '\\';
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a / 100));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a mod 10))
                end
        end;
        incr n
      done;
      s'
    end
    
let print_node = ref false
let rec iter_node node =
  if !print_node then
    begin
      let (dot, emitted) = add_node node in
    if not !emitted then      
      let module Labels = struct
          let label_of_node node2 = 
            let (dot2, _ ) = add_node node2 in
            let label = node2.node_ident.label in
            Ocamldot.add_edge dot dot2 [
              Ocamldot.EdgeLabel (Printf.sprintf "l%d" label)];
            new_nodes := node2 :: !new_nodes; 
            label
          let label_of_frame frame = 0
        let new_label () = 0
        let label_of_float f = 0
      end in
    let module Print = Asm.Print(Labels)
    in
    emitted := true;
    let instrs = ref "" in
    for i = 0 to Array.length node.instrs -1 do
      instrs := 
      Printf.sprintf "%s\\n%s" !instrs (
        escaped (Print.string_of_instr node.instrs.(i)))
    done;
    let instrs =  
      Printf.sprintf "%s\\n%s" !instrs 
      (escaped (Print.string_of_instr node.link))
    in
    Ocamldot.rename_node dot (Printf.sprintf "NODE %d\\n%s" node.node_ident.label instrs);
    dot
  else dot
    end
  else
  if List.mem node.node_ident.label !debug_nodes then 
    (print_node := true; iter_node node)
  else
  let rec iter list =
    match list with 
      [] -> raise Exit
    | node :: tail ->
        try
          iter_node node;
        with Exit -> iter tail
  in
  iter (node_succ node)
    
let rec iter_nodes () =
  match !new_nodes with
    [] -> ()
  | node :: tail ->
      new_nodes := tail;
      let _ = iter_node node in
      iter_nodes ()
      
let print program base =
  if !print_cfg then
    let graph = Ocamldot.create "cfg" [] in
    reset_cfg graph;
    let start_node = mknode () in
    let dot,_ = add_node start_node in
    Ocamldot.rename_node dot "START";
    List.iter (fun global ->
        let node = (Hashtbl.find program.desc global).code in
        Ocamldot.add_edge dot (iter_node node)
        [Ocamldot.EdgeLabel (escaped global)];
    ) program.globals;
    iter_nodes ();
    let cfg = base ^ ".cfg" in
    Ocamldot.save graph cfg;
    let ps = base ^ ".ps" in
    if !print_ps then
      Ocamldot.dot2ps cfg ps

let print_index base symbols =
  let oc = open_out (base ^ ".idx") in
  let page = ref 1 in
  List.iter (fun global ->
      try
        let func = Hashtbl.find program.desc global in
        Printf.fprintf oc "%-60s on page %d\n" (
          Printf.sprintf "%s :%d ins:%s%s%s" global func.fun_size 
            (if func.fun_alloc then "" else " noalloc:")
          (if func.jmp_dest = [] then "" else " open:")
          (if func.ret_nodes = [] then "" else (
                Printf.sprintf " %d rets:" (List.length func.ret_nodes))
          ))
        !page;
        incr page
      with _ -> ()
  ) symbols;
  close_out oc
      
let print_globals base =
  if !print_cfg then
    let old_no_fast_alloc = !no_fast_alloc in
    no_fast_alloc := true;
    let cfg = base ^ ".cfg" in
    let oc = open_out cfg in
    let symbols = 
      if !debug_symbols = [] then program.globals else !debug_symbols in
    print_index base symbols;
    List.iter (fun global ->
        try
          let node = (Hashtbl.find program.desc global).code in        
          let graph = Ocamldot.create (title_escaped global) [] in
          reset_cfg graph;
          let start_node = mknode () in
          let dot,_ = add_node start_node in
          Ocamldot.rename_node dot global;
          print_node := (!debug_nodes = []);
          (try
              Ocamldot.add_edge dot (iter_node node) [
                Ocamldot.EdgeLabel global];
              iter_nodes ();
            with
              Exit ->
                let (dot2, emitted) = add_node node in
                Ocamldot.rename_node dot2 "NO DEBUG NODE";
                Ocamldot.add_edge dot dot2 [
                  Ocamldot.EdgeLabel global]);
          Ocamldot.save_in graph oc;
        with _ -> ()
    ) symbols;
    no_fast_alloc := old_no_fast_alloc;
    close_out oc;
    let ps = base ^ ".ps" in
    if !print_ps then
      Ocamldot.dot2ps cfg ps
      
let print_func oc func =
  if !print_cfg then try
      let old_no_fast_alloc = !no_fast_alloc in
      no_fast_alloc := true;
      let node = func.code in        
      let graph = Ocamldot.create (title_escaped func.fun_name) [] in
      reset_cfg graph;
      let start_node = mknode () in
      let dot,_ = add_node start_node in
      Ocamldot.rename_node dot func.fun_name;
      print_node := (!debug_nodes = []);
      (try
          Ocamldot.add_edge dot (iter_node node) [
            Ocamldot.EdgeLabel func.fun_name];
          iter_nodes ();
        with
          Exit ->
            let (dot2, emitted) = add_node node in
            Ocamldot.rename_node dot2 "NO DEBUG NODE";
            Ocamldot.add_edge dot dot2 [
              Ocamldot.EdgeLabel func.fun_name]);
      Ocamldot.save_in graph oc;
      no_fast_alloc := old_no_fast_alloc;
    with _ -> ()
        
let create_file func =
  if !print_cfg then
    let cfg = func.fun_name ^ ".cfg" in
    let oc = open_out cfg in
    oc
  else stdout
 
let close_file oc func =
  if !print_ps then
    let ps = func.fun_name ^ ".ps" in
    let cfg = func.fun_name ^ ".cfg" in
    close_out oc;
    Ocamldot.dot2ps cfg ps
    