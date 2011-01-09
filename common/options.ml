(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


    (* Simple options:
  This will enable very simple configuration, by a mouse-based configurator.
  Options will be defined by a special function, which will also check
  if a value has been provided  by the user in its .gwmlrc file.
  The .gwmlrc will be created by a dedicated tool, which could be used
  to generate both .gwmlrc and .efunsrc files.

Note: this is redundant, since such options could also be better set
in the .Xdefaults file (using Xrm to load them). Maybe we should merge
both approaches in a latter release.
  
    *)
  
type option_value =
  Module of option_module
| Value of  string
| List of option_value list
| SmallList of option_value list
  
and option_module =
  (string * option_value) list

type 'a option_class = {
    class_name : string;
    from_value : option_value -> 'a;
    to_value : 'a -> option_value;
    mutable class_hooks : ('a option_record -> unit) list;
  }

and 'a option_record = {
    option_name : string list;
    option_class : 'a option_class;
    mutable option_value : 'a;
    option_help : string;
    mutable option_hooks : (unit -> unit) list;
  }

let define_option_class 
    (class_name : string)
  (from_value : option_value -> 'a)
  (to_value : 'a -> option_value) =
  let c = {
      class_name = class_name;
      from_value = from_value;
      to_value = to_value;
      class_hooks = [];
    } in
(*  classes := (Obj.magic c : Obj.t option_class) :: !classes; *)
  c  

  
let filename = ref (Filename.concat Utils.homedir
      ("." ^ (Filename.basename Sys.argv.(0)) ^ "rc"))
let gwmlrc = ref []

let options = ref []
  
let rec find_value list m =
  match list with
    [] -> raise Not_found
  | name :: tail ->
      let m = List.assoc name m in
      match m, tail with
        _, [] -> m
      | Module m, _ :: _ -> find_value tail m
      | _ -> raise Not_found
  
let define_option 
    (option_name : string list)
  (option_help : string)
  (option_class : 'a option_class)
  (default_value : 'a)
  =
  let o = {
      option_name = option_name;
      option_help = option_help;
      option_class = option_class;
      option_value = default_value;
      option_hooks = [];
    } in
  
  options := (Obj.magic o : Obj.t option_record) :: !options;
  
  (* is this option already loaded ??? *)
  o.option_value <- (try
      o.option_class.from_value 
        (find_value option_name !gwmlrc)
    with e -> 
        Log.printf "Options.define_option, for option %s: " 
          (match option_name with [] -> "???" | name :: _ -> name);
        Log.exn "%s\n" e;
        default_value);
  o

  
open Genlex
  
let lexer = make_lexer [ "=" ; "{" ; "}"; "["; "]"; ";" ; "("; ")"; ","; "."]
  
let rec parse_gwmlrc = parser
    [< id = parse_id; 'Kwd "="; v = parse_option ; 
      eof = parse_gwmlrc >] -> (id, v) :: eof
| [< >] -> []

and parse_option = parser
| [< 'Kwd "{"; v = parse_gwmlrc; 'Kwd "}" >] -> Module v
| [< 'Ident s >] -> Value s
| [< 'String s >] -> Value s
| [< 'Int i >] -> Value (string_of_int i)
| [< 'Float f >] -> Value (string_of_float f)
| [< 'Char c >] -> Value (let s = String.create 1 in s.[0] <- c; s)    
| [< 'Kwd "["; v = parse_list >] -> List v
| [< 'Kwd "("; v = parse_list >] -> List v
    
and parse_id = parser
    [< 'Ident s >] -> s
|   [< 'String s >] -> s

and parse_list = parser
    [< 'Kwd ";"; v = parse_list >] -> v
|   [< 'Kwd ","; v = parse_list >] -> v
|   [< 'Kwd "."; v = parse_list >] -> v
|   [< v = parse_option; t = parse_list >] -> v :: t
|   [< 'Kwd "]" >] -> []
|   [< 'Kwd ")" >] -> []

let exec_hooks o =
  List.iter (fun f -> try f () with _ -> ()) o.option_hooks  

let exec_chooks o =
  List.iter (fun f -> try f o with _ -> ()) o.option_class.class_hooks  
  
let really_load filename = 
  let ic = open_in filename in
  let s = Stream.of_channel ic in
  try
    let stream = lexer s in
    let list = try parse_gwmlrc stream with
        e -> 
          Printf.printf "At pos %d/%d" (Stream.count s) (Stream.count stream);
          print_newline ();
          raise e in
    List.iter (fun o ->
        try
          o.option_value <- o.option_class.from_value
            (find_value o.option_name list);
          exec_chooks o;
          exec_hooks o;
        with _ -> () (* no error if option is not defined here *)
    ) !options;
    list
  with   e -> 
      Printf.printf "Error %s in %s" (Printexc.to_string e) filename;
      print_newline ();
      []
      
let load () =
  try
    gwmlrc := really_load !filename
  with Not_found ->
      Printf.printf "No %s found" !filename; print_newline ()

let append filename =
  try
    gwmlrc := (really_load filename) @ !gwmlrc
  with Not_found ->
      Printf.printf "No %s found" filename; print_newline ()
      
let init () = load ()

let (!!) o = o.option_value
let (=:=) o v = 
  o.option_value <- v;
  exec_chooks o;
  exec_hooks o
    
let value_to_string v =
  match v with Value s -> s | _ -> raise Not_found
let string_to_value s = Value s
  
let value_to_int v =
  match v with Value s -> int_of_string s | _ -> raise Not_found  
let int_to_value i = Value (string_of_int i)

(* The Pervasives version is too restrictive *)
let bool_of_string s = match String.lowercase s with
  | "true" -> true
  | "false" -> false
  | "yes" -> true
  | "no" -> false
  | "1" -> true
  | "0" -> false
  | _ -> invalid_arg "bool_of_string"

let value_to_bool v =
  match v with Value s -> bool_of_string s | _ -> raise Not_found
let bool_to_value i = Value (string_of_bool i)

let value_to_float v =
  match v with Value s -> float_of_string s | _ -> raise Not_found
let float_to_value i = Value (string_of_float i)

let value_to_string2 v =
  match v with List [Value s1; Value s2] -> s1,s2 | _ -> raise Not_found
let string2_to_value (s1,s2) = SmallList [Value s1; Value s2]

let value_to_list v2c v =
  match v with List l -> List.map v2c l | _ -> raise Not_found

let list_to_value c2v l = List (
    List.fold_right (fun v list ->
        try (c2v v) :: list with _ -> list
    )  l [])
  
let smalllist_to_value c2v l = SmallList (
    List.fold_right (fun v list ->
        try (c2v v) :: list with _ -> list
    )  l [])
  
let value_to_path v =
  List.map Utils.string_to_filename
    (match v with Value s -> Utils.string_to_path s
    | List l -> List.map (fun v -> match v with
              Value s -> Utils.string_to_filename s | _ -> raise Not_found) l
    | _ -> raise Not_found)
      
let path_to_value list = Value (Utils.path_to_string 
      (List.map Utils.filename_to_string list))
  
let string_option = define_option_class "String" 
  value_to_string string_to_value
let color_option = define_option_class "Color" value_to_string string_to_value
let font_option = define_option_class "Font" value_to_string string_to_value
let int_option = define_option_class "Int" value_to_int int_to_value
let bool_option = define_option_class "Bool" value_to_bool bool_to_value
let float_option = define_option_class "Float" value_to_float float_to_value
let path_option = define_option_class "Path" value_to_path path_to_value

let string2_option = define_option_class "String2" 
  value_to_string2 string2_to_value
  
let list_option cl = define_option_class (cl.class_name ^ " List")
  (value_to_list cl.from_value) (list_to_value cl.to_value)

let smalllist_option cl = define_option_class (cl.class_name ^ " List")
  (value_to_list cl.from_value) (smalllist_to_value cl.to_value)

let to_value cl = cl.to_value
let from_value cl = cl.from_value
  
let value_to_sum l v =
  match v with
    Value s -> List.assoc s l | _ -> raise Not_found
  
let sum_to_value l v = Value (List.assq v l)
  
let sum_option l = 
  let ll = List.map (fun (a1,a2) -> a2,a1) l in
  define_option_class "Sum" (value_to_sum l) (sum_to_value ll)

let exit_exn = Exit
let safe_string s =
  if s = "" then "\"\"" else
  try
    match s.[0] with
      'a' .. 'z' | 'A' .. 'Z' ->
        for i = 1 to String.length s - 1 do
          match s.[i] with
            'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> ()
          | _ -> raise exit_exn
        done; s 
    | '0' .. '9' ->
        for i = 1 to String.length s - 1 do
          match s.[i] with
            '0' .. '9' | '.' -> ()
          | _ -> raise exit_exn
        done; s 
    | _ -> raise exit_exn
  with _ ->
      Printf.sprintf "\"%s\"" (String.escaped s)

let with_help = ref false
      
let rec save_module indent oc list = 
  let subm = ref [] in
  List.iter (fun (name, help, value) ->
      match name with
        [] -> assert false
      | [ name ] -> 
          if !with_help && help <> "" then
            Printf.fprintf oc "(* %s *)\n" help;
          Printf.fprintf oc "%s %s = " indent (safe_string name);
          save_value indent oc value;
          Printf.fprintf oc "\n";
      | m :: tail ->
          let p = try List.assoc m !subm
            with _ -> 
                let p = ref [] in
                subm := (m, p) :: !subm;
                p in
          p := (tail, help, value) :: !p) list;
  List.iter (fun (m, p) ->
      Printf.fprintf oc "%s %s = {\n" indent (safe_string m);
      save_module (indent ^ "  ") oc !p;
      Printf.fprintf oc "%s}\n" indent
  ) !subm

and save_list indent oc list =
  match list with
    [] -> ()
  | [v] -> save_value indent oc v;
  | v :: tail ->
      save_value indent oc v;
      Printf.fprintf oc ", ";
      save_list indent oc tail

and save_list_nl indent oc list =
  match list with
    [] -> ()
  | [v] -> 
      Printf.fprintf oc "\n%s" indent;
      save_value indent oc v;
  | v :: tail ->
      Printf.fprintf oc "\n%s" indent;
      save_value indent oc v;
      Printf.fprintf oc ";";
      save_list_nl indent oc tail
      
and save_value indent oc v =
  match v with
    Value s -> 
      Printf.fprintf oc "%s" (safe_string s)
  | List l -> 
      Printf.fprintf oc "[";
      save_list_nl (indent ^ "  ") oc l;
      Printf.fprintf oc "]";
  | SmallList l -> 
      Printf.fprintf oc "(";
      save_list (indent ^ "  ") oc l;
      Printf.fprintf oc ")";
  | Module _ -> 
      Printf.fprintf oc "\"\""
    
let save () =
  let oc = open_out !filename in
  save_module "" oc (List.map (fun o -> 
        o.option_name, o.option_help,
        try o.option_class.to_value o.option_value with e -> 
            Printf.printf "Error while saving option \"%s\": %s" 
              (try List.hd o.option_name with _ -> "???") (Utils.printexn e);
            print_newline ();
            Value "") 
    (List.rev !options));
    (* Save unknown options from gwmlrc *)
  List.iter (fun (name, value) ->
      try
        List.iter (fun o -> 
            match o.option_name with 
              n :: _ -> if n = name then raise Exit
            | _ -> ())
        !options;
        Printf.fprintf oc "%s = " (safe_string name);
        save_value "  " oc value;
        Printf.fprintf oc "\n";
      with _ -> ()
  ) !gwmlrc;
  close_out oc

let save_with_help () =
  with_help := true;
  (try save () with _ -> ());
  with_help := false
  
let option_hook option f =
  option.option_hooks <- f :: option.option_hooks
  
let class_hook option_class f =
  option_class.class_hooks <- f :: option_class.class_hooks

let rec iter_order f list =
  match list with [] -> () | v :: tail -> f v; iter_order f tail
  
let help oc =
  List.iter (fun o ->
      Printf.fprintf oc"OPTION \"";
      (match o.option_name with
          [] -> Printf.fprintf oc "???"
        | [name] -> Printf.fprintf oc "%s" name
        | name :: tail ->
            Printf.fprintf oc "%s" name;
            iter_order (fun name -> 
                Printf.fprintf oc ":%s" name
                ) o.option_name);
      Printf.fprintf oc "\" (TYPE \"%s\"): %s\n   CURRENT: \n" 
      o.option_class.class_name o.option_help;
      (try save_value "" oc (o.option_class.to_value o.option_value) 
        with _ -> ());
      Printf.fprintf oc "\n"
  ) !options;
  flush oc
  
    
let tuple2_to_value (c1,c2) (a1,a2) =
  SmallList [to_value c1 a1; to_value c2 a2]
let value_to_tuple2 (c1,c2) v = match v with
    List [v1;v2] -> (from_value c1 v1, from_value c2 v2)
  | _ -> raise Not_found
let tuple2_option p = define_option_class "tuple2_option" 
     (value_to_tuple2 p) (tuple2_to_value p)
  
let tuple3_to_value (c1,c2,c3) (a1,a2,a3) =
  SmallList [to_value c1 a1; to_value c2 a2; to_value c3 a3]
let value_to_tuple3 (c1,c2,c3) v = match v with
    List [v1;v2;v3] -> (from_value c1 v1, from_value c2 v2, from_value c3 v3)
  | _ -> raise Not_found
let tuple3_option p = define_option_class "tuple3_option" 
     (value_to_tuple3 p) (tuple3_to_value p)

      
let value_to_filename v =  Utils.string_to_filename (match v with
      Value s -> s | _ -> raise Not_found)
  
let filename_to_value v = Value (Utils.filename_to_string v)
      
let filename_option = define_option_class "Filename" value_to_filename filename_to_value

let shortname o = String.concat ":" o.option_name
let get_class o = o.option_class
let get_help o = let help = o.option_help in
  if help = "" then "No Help Available" else help