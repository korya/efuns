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

(*
This file generate a .dot file to be processed by "dot" to generate a 
.ps file.
*)

type graph = {
    graph_name : string;
    mutable graph_nodes : node list;
    mutable graph_edges : edge list;
    mutable node_counter : int;
    graph_attributes : graph_attributes list;
  }
  
and node = {
    mutable node_name : string;    
    node_id : int;
    node_graph : graph;
    node_attributes : node_attributes list;
  }
  
and edge = {
    edge_from : node;
    edge_to : node;
    edge_attributes : edge_attributes list;
  }

and graph_attributes =
  GraphSize of float * float
  
and node_attributes =
  NodeColor of string |
  NodeFontColor of string |
  NodeFontName of string |
  NodeShape of shape |
  NodeHeight of float |
  NodeWidth of float |
  NodeStyle of goptions 
  
and edge_attributes = 
  EdgeDirection of direction |
  EdgeLabel of string |
  EdgeStyle of goptions |
  EdgeWeight of int 
  
and direction =
  Forward |
  Backward |
  Bothdir |
  Nodir
  
and goptions = 
  Bold |
  Dotted |
  Filled
  
and shape = 
  Ellipse | 
  Box | 
  Circle | 
  DoubleCircle | 
  Diamond | 
  PlainText | 
  Record | 
  Polygon of int * (polygon_options list) | 
  Epsf of string

and polygon_options =
  Skew of float
| Distortion of float
  
let create name graph_attributes = {
    graph_name = name;
    node_counter = 0;
    graph_nodes = []; 
    graph_edges = [];
    graph_attributes = graph_attributes;
  }
  
let node graph name node_attributes = 
  let node = {
      node_name = name;
      node_id = graph.node_counter;
      node_graph = graph;
      node_attributes = node_attributes;
    } in
  graph.node_counter <- graph.node_counter + 1;
  graph.graph_nodes <- node :: graph.graph_nodes;
  node

let edge node1 node2 edge_attributes =
  let graph = node1.node_graph in
  if not (graph == node2.node_graph) then
    failwith "Ocamldot.edge: nodes in different graphs";
  let edge = {
      edge_from = node1;
      edge_to = node2;
      edge_attributes = edge_attributes;
    }
  in 
  graph.graph_edges <- edge :: graph.graph_edges;
  edge

let add_edge node1 node2 list = ignore (edge node1 node2 list)
  
let add_edges node1 nodes list =
  List.iter (fun node2 -> add_edge node1 node2 list) nodes
  
let add_path nodes list =
  match nodes with
    [] -> ()
  | node1 :: nodes ->
      ignore (List.fold_right (fun node1 node2 ->
            add_edge node1 node2 list; node2  
        ) nodes node1)

let rename_node node name = node.node_name <- name
      
open Printf

let save_in graph oc =
  let graph_attribute attr =
    match attr with
      GraphSize (f1,f2) -> fprintf oc "  size=\"%f,%f\";\n" f1 f2
        
  and edge_attribute attr = 
    match attr with
      EdgeDirection direction ->
        fprintf oc " dir=%s" (match direction with
            Forward -> "forward"
          | Backward -> "backward"
          | Bothdir -> "both"
          | Nodir -> "none")
    | EdgeLabel label ->
        fprintf oc " label=\"%s\"" label        
    | EdgeStyle option ->
        fprintf oc "style=%s" (match option with
             Bold -> "bold"
          | Dotted -> "dotted"
          | Filled -> "filled"          
        )
    | EdgeWeight weight ->
        fprintf oc "weight=%d" weight
        
  and node_attribute attr = 
    match attr with
      NodeColor color -> 
        fprintf oc ", color=\"%s\"" color
    | NodeFontColor color ->
        fprintf oc ", fontcolor=\"%s\"" color
    | NodeFontName font ->
        fprintf oc ", fontname=\"%s\"" font
    | NodeShape shape ->
        fprintf oc ", shape=%s" (match shape with
            Ellipse -> "ellipse"
          | Box -> "box"
          | Circle -> "circle"
          | DoubleCircle -> "doublecircle"
          | Diamond -> "diamond"
          | PlainText -> "plaintext"
          | Record -> "record"
          | Polygon (sides, options) -> 
              sprintf "polygon, sides=%d%s" sides
                (List.fold_left (fun s attr ->
                  match attr with
                    Skew f -> sprintf ",skew=%f%s" f s
                  | Distortion f -> sprintf ",distortion=%f%s" f s
                ) "" options)
          | Epsf filename -> sprintf "epsf, shapefile=\"%s\"" filename
        )
    | NodeHeight height ->
        fprintf oc ", height=%f" height
    | NodeWidth width -> 
        fprintf oc ", width=%f" width
    | NodeStyle option ->
        fprintf oc ", style=%s" (match option with
            Bold -> "bold"
          | Dotted -> "dotted"
          | Filled -> "filled"          
        )
        
  in
  fprintf oc "digraph %s {\n" graph.graph_name;
  List.iter graph_attribute graph.graph_attributes;
  List.iter (fun node -> 
      fprintf oc "  node%d [ label=\"%s\"" node.node_id node.node_name;
      List.iter node_attribute node.node_attributes;
      fprintf oc " ];\n";
      ) graph.graph_nodes;  
  List.iter (fun edge -> 
      fprintf oc "  node%d -> node%d [" 
        edge.edge_from.node_id edge.edge_to.node_id;
      (match edge.edge_attributes with
          [] -> ()
        | attr :: tail ->
            edge_attribute attr;
            List.iter (fun attr -> 
                fprintf oc ", ";
                edge_attribute attr) tail);
      fprintf oc " ];\n";
  ) graph.graph_edges;
  fprintf oc "}\n"
  
let save graph filename =
  let oc = open_out filename in
  save_in graph oc;
  close_out oc
  
let view g =
  let filename = Filename.temp_file "dotfile" ".dot" in
  try
    save g filename;
    let e = Sys.command (Printf.sprintf "dot -Tps < %s > %s.ps" filename filename) in
    if e <> 0 then failwith "Ocamldot: error while dot was processing file";
    let _ = Sys.command (Printf.sprintf "gv %s.ps" filename) in
    Sys.remove filename; 
    Sys.remove (filename^".ps");
    ()
  with e -> 
      Sys.remove filename;
      Sys.remove (filename^".ps");
      raise e 
      
let dot2ps file_dot file_ps =
  let e = Sys.command (
      Printf.sprintf "dot -Tps < %s > %s" file_dot file_ps) in
  if e <> 0 then failwith "Ocamldot: error while dot was processing file"
  
      