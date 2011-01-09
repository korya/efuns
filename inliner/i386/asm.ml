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

open Misc

  (* Since we are always using 31 bits integer, we are limited in
  the size of blocks to 4 Mo per block. We cannot optimize a program
  using bigger blocks (unless no such constant appears in the text). *)
  
type data =
| Long of constant
| Ascii of string
| Word of constant
| Byte of constant
| Space of int
| Align of int
| Skip of int
| String of string
| Label of string
| Double of float

and modifier =
  ON_BYTE
| ON_WORD
| ON_LONG
  
and opcode =
| Movl  
| Addl  
| Subl  
| Imull  
| Andl  
| Orl  
| Xorl  
| Sall  
| Shrl  
| Sarl  
| Testl  
| Cmpl  
| Call of frame option
| Movzbl  
| Movsbl  
| Movzwl  
| Movswl  
| Bswap
| Movb  
| Movw  
| SetCond of signed_comparison 
| Cltd
| Idivl 
| Incl 
| Decl 
| Pushl 
| Lea  
| Leal  
| Ret
| Popl   
| Stc
| Rcrl
| Rcll
  
| Cmpi of modifier
| Andi of modifier
| Xori of modifier
| Deci of modifier
  
| Fstpl 
| Fldl 
| Fchs
| Fabs
| Faddl 
| Fsubl 
| Fmull 
| Fdivl 
| Fsubrl 
| Fdivrl 
| Faddp  
| Fsubp  
| Fmulp  
| Fdivp  
| Fsubrp  
| Fdivrp  
| Fldz
| Fld1
| Fnstcw 
| Fldcw 
| Fistpl 
| Fildl 
| Fcompl
| Fcompp
| Fnstsw
  
| Alloc of int * int * frame * bool

(* These can only be seen as the last instruction (link) of a node *)
| Jmp
| Switch of node list
| Setuptrap of node * node
| Raise
| Startnode of node list
| Jcond of signed_comparison * node
| Pushtrap
| Poptrap of node
| Removetrap
| CheckBound of signed_comparison

| Movsl
| Rep of opcode
  
  (* In fact, there is no need for Dir_begindata et Dir_enddata car asmopt
ne modifie pas l'ordre de definition des donnees. *)

and 'a directive =
| Dir_args of 'a argument list
| Dir_res  of 'a argument list
| Dir_saved  of 'a argument list
| Dir_save  
| Dir_live   of 'a argument list
| Dir_begincode of string
| Dir_endcode of string
| Dir_begindata of string
| Dir_enddata of string
| Dir_switch of int
| Dir_entry
| Dir_exit
| Dir_tailcall
| Dir_continue
| Dir_record of string
| Dir_nogc
| Dir_frametable
| Dir_pushtrap
| Dir_poptrap
| Dir_noalloc of int
  
and constant =
  Const_int of int
| Const_symbol of string
| Const_label of node
| Const_data_label of string
| Const_float of cfloat
| Const_add of constant * constant
| Const_sub of constant * constant
| Const_mul of constant * constant

and operation =
  Add
| Sub
| Not
| And
| Or
| Xor
| Shr (* [arg2;arg1] --> arg1 << arg2 *)
| Sar
| Sal
| Mul
| Div
| Mod
| Combine of (* byte *) int * (* bits *) int  
| Partial of (* byte *) int * (* bits *) int
  
  (* NOTE:
  Combine (0,8)[old,new] = (old & 0xffffff00) | (new & 0xff)
  Combine (1,8)[old,new] = (old & 0xffff00ff) | ((new << 8) & 0xff00)
  Combine (0,16)[old,new] = (old & 0xffff0000) | (new & 0xffff)

  Partial(0,8)[v] = v & 0xff
  Partial(1,8)[v] = (v >> 8) & 0xff
  Partial(0,16)[v] = v & 0xffff
  
  *)
  
  
and approx_state = {
    registers : approx_value array; (* eax ... ebp *)
    mutable stack_size : int;
    mutable stack : approx_value array;
    mutable store : (approx_value * (approx_value array)) list;
    mutable liveness : instruction list array;
    state_num : int;
  }
  
and approx_value =
  (* phi-node: when a node received a different value for a location already
  initialized with a different value. The node indicates where the phi-node
  was created for the variable. *)
| Xvar of int * node
(* A parameter of the function *)
| Xparam of int
(* A constant value *)
| Xconst of constant
| Xloadconst of constant
| Xheader of approx_value
(* The address of a just allocated object *)
| Xaddress of approx_value
(* An operation *)
| Xop of operation * (approx_value list)
  
and 'a argument =  
  Register of 'a
| Const of constant
| ConstantBase of constant
| OffsetBase of constant * 'a
| OffsetBaseIndex of constant * register * 'a
| OffsetBaseIndexScale of constant * register * 'a * int
| OffsetIndexScale of constant * 'a * int
| Indirect of 'a argument

and register =
  Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp
| LowByte of register
| HighByte of register
| LowWord of register
| St of int
  
and instruction = {
    mutable opcode : opcode;
    mutable args : register argument list;
    mutable directives : register directive list;
    mutable used_by : instruction list;
    mutable use : instruction list;
    mutable instr_num : int;
    mutable enter_instr : approx_state;
    mutable leave_instr : approx_state;
    mutable dead : bool;
  }
  
and ident = {
    mutable label : int;
    mutable emitted : bool;
  }
  
and node = {
    mutable node_ident : ident;
    mutable instrs : instruction array; 
    mutable link : instruction;
    mutable back_edges : node list;
    mutable enter_node : approx_state;
    mutable leave_node : approx_state;
    mutable node_names : string list;
  }

and frame = {
    mutable size : int;
    mutable pos : int list;
  }

and macro =
  Macro_replace of (string * bool) * node
| Macro_inline of (string * bool)
| Macro_peephole of match_vars * string * node * predicat list * node

and predicat = 
  Dead of approx_state * register list
| Distinct of register list
| Equal of constant list
| SameReg of register list
| SameValue of (approx_state * register) list
| Instr of opcode * (opcode list)
  
and program = {
    mutable name : string;
    mutable data : data_desc list;
    mutable globals : string list;
    desc : (string, func_desc) Hashtbl.t;
    env : (string, func_desc) Hashtbl.t;
    mutable depends : string list;
    mutable start_node : node;
    mutable peepholes : (string * match_vars * node * predicat list * node) list;
  }

and func_desc = {
    mutable fun_name : string;
    mutable code : node;
    mutable jmp_dest : string list;
    mutable ret_nodes : node list;
    arity : int;
    mutable fun_alloc : bool;
    mutable fun_size : int;
    mutable fun_inline : bool;
    mutable fun_debug : bool;
    (* Has this function some side-effects. The ints represent fields
    inside blocks that could be modified in the function.
    None -> no side effect.
    Some [] -> no info = side effect.
    Some [i1;i2;...] -> side effect limited.
    *)
    mutable fun_raise : bool;
    mutable fun_side_effect : bool;
    mutable fun_functional: int list option;
  }
  
and data_desc = {
    data_name : string;
    data_header : int;
    data_value : data list;
  }

and cfloat = {
    mutable float : float;
    mutable float_ident : ident;
  }

and match_vars = {
    match_regs : register ref list;
    match_consts : constant ref list;
    match_states : approx_state ref list;
    match_opcodes : opcode ref list;
    match_args : register argument ref list;
  }
  
let nparams = 6 (* eax ebx ecx edx esi edi *)
let nregs = 7 (* eax ebx ecx edx esi edi ebp 0(esp) 4(esp) ... *)
let regs = [|
    Eax; Ebx; Ecx; Edx; 
    Esi; Edi; Ebp; Esp;
    LowByte Eax; LowByte Ebx; LowByte Ecx; LowByte Edx; 
    HighByte Eax; HighByte Ebx; HighByte Ecx; HighByte Edx; 
    LowWord Eax; LowWord Ebx; LowWord Ecx; LowWord Edx;
    LowWord Esi; LowWord Edi; LowWord Ebp; 
  |]

let floats = Hashtbl.create 91
let labels = Hashtbl.create 91
let frames = Hashtbl.create 91

let vars = ref 100
let mkvar node =
  incr vars;
  Xvar (!vars, node)
  
let label_counter = ref 0
let mkident () =
  {
      label = (incr label_counter; !label_counter);
      emitted = false;
  }
  
let instrs = ref (-1)

  
let state_nums = ref 0
let next_state () = incr state_nums; !state_nums
  
let noapprox = {
    registers = [||];
    stack_size = 0;
    stack = [||];
    store = [];
    liveness = [||];
    state_num = 0;
  }
  
let new_instr () = incr instrs; !instrs
  
let mkinstr op args dirs = { 
    opcode = op; 
    args = args; 
    directives = dirs;
    used_by = [];
    use = [];
    instr_num = new_instr (); (* to be set before Emit *)
    enter_instr = noapprox;
    leave_instr = noapprox;
    dead = false;
    }
  
let mknode () = { 
    instrs = [||];
    link = mkinstr Ret [] []; 
    node_ident = mkident ();
    back_edges = [];
    enter_node = noapprox;
    leave_node = noapprox;
    node_names = [];
  }

let mkfunc name arity code =
  code.node_names <- [name];
  {
    fun_name = name;
    code = code;
    jmp_dest = [];
    ret_nodes = [];
    arity = arity;
    fun_alloc = true;
    fun_size = 0;
    fun_inline = false;
    fun_side_effect = true;
    fun_functional = Some [];
    fun_raise = true;
    fun_debug = false;
  }

let nonode = mknode ()
  
let program = {
    name  = "";
    data = [];
    globals = [];
    desc = Hashtbl.create 511;
    env = Hashtbl.create 511;
    depends = [];
    start_node = nonode;
    peepholes = [];
  }

let reset_parser () = 
  label_counter := 0;
  Hashtbl.clear labels;
  Hashtbl.clear frames;
  Hashtbl.clear floats;
  Hashtbl.clear program.desc;
  Hashtbl.clear program.env;
  program.start_node <- mknode ();
  program.depends <- [];
  Parsing.clear_parser ()

let mkframe () = { size = 0; pos = [];  }
let mkfloat () = { float = 0.0; float_ident = mkident (); }
  
let zero = Const (Const_int 0)

let label (lbl: string) = 
  try Hashtbl.find labels lbl 
  with Not_found -> 
      let node = mknode () in
      Hashtbl.add labels lbl node;
      node

let frame (lbl: string) = 
  try Hashtbl.find frames lbl 
  with Not_found -> 
      let frame = mkframe () in
      Hashtbl.add frames lbl frame;
      frame
      
let float (lbl: string) = 
  try Hashtbl.find floats lbl 
  with Not_found -> 
      let float = mkfloat () in
      Hashtbl.add floats lbl float;
      float
      
let eax = 0
let ebx = 1
let ecx = 2
let edx = 3
let esi = 4
let edi = 5
let ebp = 6
  
let zero = Const (Const_int 0)

let string_of_data data =
  match data with
  | Long _ -> "long"
  | Ascii _ -> "ascii"
  | Word _ -> "word"
  | Byte _ -> "byte"
  | Space _ -> "space"
  | Align _ -> "align"
  | Skip _ -> "skip"
  | String _ -> "string"
  | Label _ -> "label"      
  | Double _ -> "double"  

let string_of_modifier m =
  match m with
    ON_BYTE -> "b"
  | ON_WORD -> "w"
  | ON_LONG -> "l"
      
let rec string_of_code instr = 
  match instr with
  | Movl-> "movl"
  | Addl-> "addl"
  | Subl-> "subl"
  | Imull-> "imull"
  | Andl-> "andl"
  | Orl-> "orl"
  | Xorl-> "xorl"
  | Sall-> "sall"
  | Shrl-> "shrl"
  | Sarl-> "sarl"
  | Testl-> "testl"
  | Cmpl-> "cmpl"
      
  | Cmpi m -> "cmp" ^ (string_of_modifier m)
  | Andi m -> "and" ^ (string_of_modifier m)
  | Xori m -> "xor" ^ (string_of_modifier m)
  | Deci m-> "dec" ^ (string_of_modifier m)

  | Call _ -> "call"
  | Jmp -> "jmp"
  | Movzbl-> "movzbl"
  | Movsbl-> "movsbl"
  | Movzwl-> "movzwl"
  | Movswl-> "movswl"
  | Movb-> "movb"
  | Movw-> "movw"
  | SetCond _ -> "setcond"
  | Jcond _ -> "jcond"
  | Cltd -> "cltd"
  | Idivl -> "idivl"
  | Incl -> "incl"
  | Decl -> "decl"
  | Pushl -> "pushl"
  | Lea-> "lea"
  | Leal-> "leal"
  | Ret -> "ret"
  | Popl   -> "popl"
  | Bswap -> "bswap"  
  | Stc -> "stc"
  | Rcrl -> "rcrl"
  | Rcll -> "rcll"

  | Switch _ -> "Switch"
  | Setuptrap _ -> "Setuptrap"
  | Poptrap _ -> "Poptrap"
  | Removetrap -> "Removetrap"
  | Fstpl -> "fstpl"
  | Fldl -> "fldl"
  | Fchs -> "fchs"
  | Fabs -> "fabs"
  | Faddl -> "faddl"
  | Fsubl -> "fsubl"
  | Fmull -> "fmull"
  | Fdivl -> "fdivl"
  | Fsubrl -> "fsubrl"
  | Fdivrl -> "fdivrl"
  | Faddp-> "faddp"
  | Fsubp-> "fsubp"
  | Fmulp-> "fmulp"
  | Fdivp-> "fdivp"
  | Fsubrp-> "fsubrp"
  | Fdivrp-> "fdivrp"
  | Fldz -> "fldz"
  | Fld1 -> "fld1"
  | Fildl -> "fildl"
  | Fnstcw -> "fnstcw"
  | Fldcw -> "fldcw"
  | Fistpl -> "fistpl"
  | Fcompl -> "fcompl"
  | Fcompp -> "fcompp"
  | Fnstsw -> "fnstsw"

  | CheckBound comp -> "CheckBound"
  | Alloc _ -> "Alloc"
  | Startnode _ -> "Startnode"
      
  | Movsl -> "movsl"
  | Rep _ -> "Rep"
  | _ -> "not implemented"
      
let arch = "i386"
  
    
let string_of_register reg =
  match reg with    
  | Eax -> "%eax"
  | Ebx -> "%ebx"
  | Ecx -> "%ecx"
  | Edx -> "%edx"
  | Esi -> "%esi"
  | Edi -> "%edi"
  | Ebp -> "%ebp"
  | Esp -> "%esp"  
  | LowByte Eax -> "%al"
  | LowByte Ebx -> "%bl"
  | LowByte Ecx -> "%cl"
  | LowByte Edx -> "%dl"
  | HighByte Eax -> "%ah"
  | HighByte Ebx -> "%bh"
  | HighByte Ecx -> "%ch"
  | HighByte Edx -> "%dh"
  | LowWord Eax -> "%ax"
  | LowWord Ebx -> "%bx"
  | LowWord Ecx -> "%cx"
  | LowWord Edx -> "%dx"
  | LowWord Esi -> "%si"
  | LowWord Edi -> "%di"
  | LowWord Ebp  -> "%bp"
  | St 0 -> "%st"
  | St i -> Printf.sprintf "%cst(%d)" '%' i
  | _ -> assert false

let node_succ node =
  match node.link with
    { opcode = Switch nodes } -> nodes
  | { opcode = Setuptrap (node, next) } -> [node; next]
  | { opcode = Jmp; args = [ConstantBase (Const_label node)] } -> [node]
  | { opcode = Jcond (_, next); 
      args = [ConstantBase (Const_label node)] } -> [node; next]
  | { opcode = Startnode nodes } -> nodes
  | { opcode = Poptrap next } -> [next]
  | _ -> []
      
      
module Iter = struct
    let table = Hashtbl.create 511
    let nodes = ref []
    
    let reset () = 
      nodes := [];
      Hashtbl.clear table
    
    let add_node node =
      nodes := node :: !nodes
    
    let rec iter apply =
      match !nodes with
        [] -> ()
      | node :: tail ->
          nodes := tail;
          (try Hashtbl.find table node.node_ident.label with
              Not_found -> Hashtbl.add table node.node_ident.label ();
                apply node);
          iter apply
          
          
    let iter_instr apply node =
      let rec iter i =
        if i < Array.length node.instrs then
          let instr = node.instrs.(i) in          
          apply node i instr;
          iter (i+1)
        else
        match node.link with
          { opcode = Switch nodes } ->
            List.iter add_node nodes
        | { opcode = Setuptrap (node, next) } ->
            add_node node; add_node next
        | { opcode = Jmp; args = [ConstantBase (Const_label node)] } ->
            add_node node
        | { opcode = Jcond (_, next); 
            args = [ConstantBase (Const_label node)] } ->
            add_node node;
            add_node next;
        | { opcode = Startnode list } ->
            List.iter add_node list
        | { opcode = Poptrap next } ->
            add_node next
        | _ -> ()
      in
      iter 0
    
    let iter_instr_s apply = iter (iter_instr apply)
    
    let rec iter_node_edges apply node1 =
      match node1.link with
        { opcode = Switch nodes } ->
          List.iter (fun node2 -> 
              add_node node2; apply node1 node2) nodes
      | { opcode = Setuptrap (node2,next) } ->
          add_node node2; 
          add_node next;
          apply node1 node2; 
          apply node1 next;
      | { opcode = Jmp; args = [ConstantBase (Const_label node2)] } ->
          add_node node2; 
          apply node1 node2
      | { opcode = Jcond (_, next); 
          args = [ConstantBase (Const_label node2)] } ->
          add_node node2; 
          add_node next;
          apply node1 node2;
          apply node1 next;
      | { opcode = Poptrap next } ->
          add_node next;
          apply node1 next
      | { opcode = Startnode nodes } ->
          List.iter (fun node2 -> 
              add_node node2; apply node1 node2) nodes            
      | _ -> ()          
    
    let iter_edges apply =
      iter (iter_node_edges apply)
    
    let rec iter_node_nodes apply node1 =
      apply node1;
      match node1.link with
        { opcode = Switch nodes } ->
          List.iter add_node nodes
      | { opcode = Setuptrap (node2, next) } ->
          add_node node2;
          add_node next
      | { opcode = Jmp; args = [ConstantBase (Const_label node2)] } ->
          add_node node2
      | { opcode = Jcond (_, next); 
          args = [ConstantBase (Const_label node2)] } ->
          add_node node2;
          add_node next
      | { opcode = Poptrap next } ->
          add_node next;
      | { opcode = Startnode nodes } ->
          List.iter add_node nodes
      | _ -> ()

    let iter_nodes apply =
      iter (iter_node_nodes apply);
      assert (!nodes = [])

    let rec iter_before_node apply node =
      List.iter add_node (node_succ node);
      apply node
      
    let iter_before_nodes apply =
      iter (iter_before_node apply)            
      
    let iterq_nodes node apply =
      let nodes = ref [] in
      let rec iter_node node =
        if not (List.memq node !nodes) then
          begin
            nodes := node :: !nodes;
            apply node;            
            match node.link with
              { opcode = Switch nodes } ->
                List.iter iter_node nodes
            | { opcode = Setuptrap (node2, next) } ->
                iter_node node2;
                iter_node next
            | { opcode = Jmp; args = [ConstantBase (Const_label node2)] } ->
                iter_node node2
            | { opcode = Jcond (_, next); 
                args = [ConstantBase (Const_label node2)] } ->
                iter_node node2;
                iter_node next
            | { opcode = Poptrap next } ->
                iter_node next;
            | { opcode = Startnode nodes } ->
                List.iter iter_node nodes
            | _ -> ()
          end
      in
      iter_node node
      
  end

      
module Print = functor (Labels: sig
      val label_of_node : node -> int
      val label_of_frame : frame -> int
      val label_of_float : cfloat -> int
      val new_label : unit -> int
    end) -> struct
      open Labels
      
      let opp sign = match sign with '+' -> '-'
        | '-' -> '+'
        | ' ' -> '-'
        | _ -> sign
            
      let norm sign = match sign with ' ' -> '+' | _ -> sign
            
      let rec string_of_const sign c = match c with
          Const_int n -> Printf.sprintf "%c%d" sign n
        | Const_symbol s -> Printf.sprintf "%c%s" sign s
        | Const_label node -> 
            Printf.sprintf ".L%d" (label_of_node node)
        | Const_data_label label -> 
            Printf.sprintf "%s" label
        | Const_sub (c1,c2) -> 
            Printf.sprintf "%s%s" (string_of_const sign c1)
            (string_of_const (opp sign) c2)
        | Const_add (c1,c2) -> 
            Printf.sprintf "%s%s" (string_of_const sign c1)
            (string_of_const (norm sign) c2)
        | Const_float f ->
            Printf.sprintf ".L%d" (label_of_float f)
        | Const_mul (c1, c2) ->
            Printf.sprintf "%s * %s"
              (string_of_const sign c1) (string_of_const ' ' c2)

      let string_of_const c = string_of_const ' ' c
            
      let string_of_offset c = match c with
          Const_int 0 -> ""
        | _ -> string_of_const c
      
      let rec string_of_arg arg = 
        match arg with
          Register r -> string_of_register r
        | Const (Const_int n) -> Printf.sprintf "$%d" n
        | Const (Const_symbol s) -> Printf.sprintf "$%s" s
        | Const c -> Printf.sprintf "$%s" (string_of_const c)
        | ConstantBase c -> string_of_const c
        | OffsetBase (Const_int 0, r) -> Printf.sprintf "(%s)"
              (string_of_register r)
        | OffsetBase (c, r) -> Printf.sprintf "%s(%s)"
              (string_of_offset c) (string_of_register r)
        | OffsetBaseIndex (Const_int 0, r1,r2) -> Printf.sprintf "(%s, %s)"
              (string_of_register r1) (string_of_register r2)
        | OffsetBaseIndex (c, r1,r2) -> Printf.sprintf "%s(%s, %s)"
              (string_of_offset c) (string_of_register r1) (string_of_register r2)
        | OffsetBaseIndexScale (Const_int 0, r1,r2, c2) -> 
            Printf.sprintf "(%s, %s, %d)"
              (string_of_register r1) (string_of_register r2) c2
        | OffsetBaseIndexScale (c1, r1,r2, c2) -> 
            Printf.sprintf "%s(%s, %s, %d)" (string_of_offset c1)
            (string_of_register r1) (string_of_register r2) c2
        | OffsetIndexScale (Const_int 0, r, c2) -> 
            Printf.sprintf "(, %s, %d)" (string_of_register r) c2
        | OffsetIndexScale (c1, r, c2) -> 
            Printf.sprintf "%s(, %s, %d)"
              (string_of_offset c1) (string_of_register r) c2
        | Indirect arg -> "*" ^ (string_of_arg arg)
      
      let string_of_comp = function
          Isigned Ceq -> "e"     | Isigned Cne -> "ne"
        | Isigned Cle -> "le"     | Isigned Cgt -> "g"
        | Isigned Clt -> "l"     | Isigned Cge -> "ge"
        | Iunsigned Ceq -> "e"   | Iunsigned Cne -> "ne"
        | Iunsigned Cle -> "be"  | Iunsigned Cgt -> "a"
        | Iunsigned Clt -> "b"  | Iunsigned Cge -> "ae"
      
      open Printf
      
      let string_of_instr instr =
        match instr.opcode, instr.args with    
        | Jcond (comp, next), [arg] ->
            Printf.sprintf "\tj%s\t%s\n\tjmp\t.L%d" (string_of_comp comp) (string_of_arg arg) (label_of_node next); 
        | CheckBound (comp), [] ->
            Printf.sprintf "\tbound%s" (string_of_comp comp);
        | SetCond comp, [arg] ->
            Printf.sprintf "\tset%s\t%s" (string_of_comp comp) (string_of_arg arg)
        | Switch (nodes), [Register reg] ->
            let label = new_label () in
            (Printf.sprintf "\tjmp\t*.L%d(, %s, 4)\n.data\n.L%d:\n" 
                label (string_of_register reg) label) ^
              (let rec iter nodes =
                match nodes with
                  [] -> ""
                | node :: tail ->
                    Printf.sprintf "\t.long\t.L%d\n%s" (label_of_node node)
                    (iter tail)
              in
              iter nodes) ^  ".text";
        
        | Raise,_ ->
            "\tmovl\tcaml_exception_pointer, %esp\n\tpopl\tcaml_exception_pointer\n\tret"        
        | Setuptrap (node,next), _ ->
            let _ = label_of_node next in
            Printf.sprintf "\tcall\t.L%d\n\tjmp\t.L%d" (label_of_node node)
            (label_of_node next)
        | Pushtrap , _ ->
            Printf.sprintf "\tpushl\tcaml_exception_pointer\n\tmovl\t%s, caml_exception_pointer" "%esp"
        | Removetrap , _ ->
            Printf.sprintf "\tpopl\tcaml_exception_pointer\n\taddl\t$4, %s"
              "%esp"
        | Poptrap next, _ ->
            Printf.sprintf "\tpopl\tcaml_exception_pointer\n\taddl\t$4, %s\n\tjmp\t.L%d"
              "%esp" (label_of_node next)
        
        | Call (Some frame), [arg] ->
            Printf.sprintf "\tcall\t%s\n.L%d:" (string_of_arg arg)
            (label_of_frame frame)
        
        | Alloc (n_orig, n, frame, fast), [arg] ->
            if (not fast) || !Args.no_fast_alloc then
              begin
                match n with
                  8  ->
                    Printf.sprintf "\tcall\tcaml_alloc1\n.L%d:\n\tleal\t4(%s), %s" (label_of_frame frame) "%eax" (string_of_arg arg)
                | 12 -> 
                    Printf.sprintf "\tcall\tcaml_alloc2\n.L%d:\n\tleal\t4(%s), %s" (label_of_frame frame) "%eax" (string_of_arg arg)
                | 16 -> 
                    Printf.sprintf "\tcall\tcaml_alloc3\n.L%d:\n\tleal\t4(%s), %s" (label_of_frame frame) "%eax" (string_of_arg arg)
                | _  ->
                    Printf.sprintf "\tmovl\t$%d, %s\n\tcall\tcaml_alloc\n.L%d:\n\tleal\t4(%s), %s" n "%eax" (label_of_frame frame) "%eax" (string_of_arg arg)
              end
            else              
            let gc_node = mknode () in
            let redo_node = mknode () in
            gc_node.instrs <- [|
              mkinstr (Call (Some frame))  
              [ConstantBase(Const_symbol "caml_call_gc")] [] |];
            gc_node.link <- 
              mkinstr Jmp [ConstantBase (Const_label redo_node)] [];
            redo_node.node_ident.emitted <- true;
            let redo_label = label_of_node redo_node in
            let gc_label = label_of_node gc_node in
            Printf.sprintf ".L%d:\n\tmovl\tyoung_ptr, %s\n\tsubl\t$%d, %s\n\tmovl\t%s, young_ptr\n\tcmpl\tyoung_limit, %s\n\tjb\t.L%d\n\tleal\t4(%s), %s\n" redo_label "%eax" n "%eax" "%eax" "%eax" gc_label "%eax" (string_of_arg arg)
    
        | Rep i, args ->
            Printf.sprintf "\trep\n\t%s" (string_of_code i)
        | i, args -> sprintf "\t%s%s" (string_of_code i)
            (let rec iter s args =
                match args with
                  [] -> s
                | e1 :: tail ->
                    iter (Printf.sprintf "%s, %s" s (string_of_arg e1)) tail
              in
              match args with
                [] -> ""
              | e1::tail -> iter ("\t"^(string_of_arg e1)) tail)
    
    end
      
module SimplePrint = Print (struct 
      let label_of_node node = node.node_ident.label
      let new_label () = 0
      let label_of_float f = 0
      let label_of_frame f = 0
    end)

let print_func func =
  Iter.iterq_nodes func.code (fun node ->
      Printf.printf ".L%d:" node.node_ident.label; print_newline ();
      Array.iter (fun instr ->
          Printf.printf "%s\n" (
            SimplePrint.string_of_instr instr)) node.instrs;
      Printf.printf "%s\n" (SimplePrint.string_of_instr node.link))          

let rec string_of_approx v =
  match v with
    Xvar (i,node) -> Printf.sprintf "v%dn%d" i node.node_ident.label
  | Xparam i -> Printf.sprintf "p%d" i
  | Xconst c -> SimplePrint.string_of_const c
  | Xloadconst c -> Printf.sprintf "[%s]" (SimplePrint.string_of_const c)
  | Xheader a -> Printf.sprintf "h[%s]" (string_of_approx a)
  | Xaddress a -> Printf.sprintf "m[%s]" (string_of_approx a)
  | Xop (op, args) -> 
      Printf.sprintf "OP(%s,%s)"
        (match op with
          
          Shr -> "Shr"
        | Add -> "Add"
        | Sub -> "Sub"
        | And -> "And"
        | Xor -> "Xor"
        | Or -> "Or"
        | Sar -> "Sar"
        | Sal -> "Sal"
        | Mul -> "Mul"
        | Div -> "Div" 
        | Not -> "Not"
        | Mod -> "Mod"
        | Combine (part, bits) ->
            Printf.sprintf "Combine[%d,%d]" part bits
        | Partial (part, bits) ->
            Printf.sprintf "Partial[%d,%d]" part bits
      ) (List.fold_left (fun s arg ->
            Printf.sprintf "%s%s," s (string_of_approx arg)
        ) "" args)
      

let regindex reg =
  match reg with
    Eax -> 0
  | Ebx -> 1
  | Ecx -> 2
  | Edx -> 3
  | Esi -> 4
  | Edi -> 5
  | Ebp -> 6
  | _ -> raise Exit

let regindex_all reg =
  match reg with
  | LowWord r -> regindex r
  | LowByte r -> regindex r
  | HighByte r -> regindex r
  | _ -> regindex reg

let copy_frame frame = { frame with pos = frame.pos }
let copy_callframe frame =
  match frame with None -> None |
    Some frame -> Some (copy_frame frame)
      
let copy_instr i =      
  let copy = { i with instr_num = new_instr () } in
  begin
    match i.opcode with
      Call (Some frame) ->   
        copy.opcode <- Call (Some (copy_frame frame))
    | Alloc (n_orig, n, frame, fast) -> 
        copy.opcode <- Alloc (n_orig, n, copy_frame frame, fast)
    | Startnode _ -> assert false
    | _ -> ()
  end;
  copy
  
let copy_instrs instrs = Array.map copy_instr instrs

let copy_one_node node = 
  let new_node = mknode () in
  new_node.instrs <- copy_instrs node.instrs;
  let instr = copy_instr node.link in
  new_node.link <- instr;
  new_node

let copy_node node = 
  let nodes = ref [] in
  let rec copy node =
    try
      List.assq node !nodes
    with Not_found ->
        let new_node = mknode () in
        new_node.instrs <- copy_instrs node.instrs;
        let instr = copy_instr node.link in
        new_node.link <- instr;
        (match instr with
            { opcode = Switch nodes } ->
              instr.opcode <- Switch (List.map copy nodes) 
          | { opcode = Setuptrap (node, next) } ->
              instr.opcode <- Setuptrap (copy node, copy next) 
          | { opcode = Jmp; 
              args = [ConstantBase (Const_label node)] } ->
              instr.args <- [ConstantBase (Const_label (copy node))]
          | { opcode = Jcond(comp, next); 
              args = [ConstantBase (Const_label node)] } ->
              instr.opcode <- Jcond (comp, copy next);
              instr.args <- [ConstantBase (Const_label (copy node))]
          | { opcode = Poptrap next } ->
              instr.opcode <- Poptrap (copy next)
          | { opcode = Startnode _ } -> assert false
          | _ -> ()
        );
        nodes := (node, new_node) :: !nodes;
        new_node
  in
  copy node

  (* cut_node node1 len: cut node1 in two nodes (node1,node2) with
  node1 containing the len first instructions and node2 the other ones.
  *)
  
let cut_node node1 i =
  let node2 = mknode () in
  node2.link <- node1.link;
  let link = mkinstr Jmp [ConstantBase(Const_label node2)] [] in
  node1.link <- link;
  let instrs = node1.instrs in
  let len = Array.length instrs in
  node2.instrs <- Array.sub instrs i (len-i);
  node1.instrs <- Array.sub instrs 0 i;
  node2
  
let cut_instr node1 i =
  let node2 = mknode () in
  node2.link <- node1.link;
  let link = mkinstr Jmp [ConstantBase(Const_label node2)] [] in
  node1.link <- link;
  let instrs = node1.instrs in
  let len = Array.length instrs in
  node2.instrs <- Array.sub instrs (i+1) (len-i-1);
  node1.instrs <- Array.sub instrs 0 i;
  node2

type block = {
    b_header : int;
    mutable b_value : bvalue list;
  }
and bvalue =
  B_pointer of block
| B_float of float
| B_long of constant
| B_string of string

let block_of_data data =
  let datas = 
    (Label "VALUE") ::
    data.data_value in
  let blocks = ref [] in
  let rec compute_block header list label = 
    try
      List.assoc label !blocks
    with Not_found ->
        match list with
          [] -> assert false
        | v :: tail ->
            match v with
              Label l when l = label -> 
                let block = { b_header = header;
                    b_value = [] } in
                blocks := (label, block) :: !blocks;
                let len = header asr 10 in
                let rec iter tail len list =
                  if len = 0 then list else
                  match tail with
                    [] -> assert false
                  | v :: tail ->
                      match v with
                        Long i ->                     
                          iter tail (len-4) (B_long i::list)
                      | String s ->
                          [B_string s]
                      | Ascii s ->
                          [B_string s]
                      | Double f ->
                          iter tail (len-8) (B_float f::list)
                      | Label l ->
                          iter tail (len-4) 
                          (B_pointer 
                              (compute_block data.data_header datas l)::list)
                      | _ -> assert false
                in
                let inside = iter tail len [] in
                block.b_value <- List.rev inside;
                block
        | Long (Const_int n) -> compute_block n tail label
        | _ -> compute_block 0 tail label
  in      
  compute_block data.data_header datas "VALUE"
  
  
let print_state enter_state =  
  let liveness = enter_state.liveness in
  if liveness <> [||] then
    begin
      Printf.printf "STATE %d" enter_state.state_num;
      print_newline ();
      let len = Array.length liveness in
      Array.iteri (fun i v -> 
          Printf.printf "%s=%s(" (string_of_register regs.(i))
          (string_of_approx v);          
          let used_by = liveness.(i) in
          if used_by = [] then 
            Printf.printf "DEAD"
          else
            List.iter (fun i -> Printf.printf "%d " i.instr_num) used_by;
          Printf.printf ") "              
      ) enter_state.registers;
      Array.iteri (fun i v -> 
          Printf.printf "ESP(%d)=%s( " i (string_of_approx v);
          let used_by = liveness.(len-1-i) in
          if used_by = [] then 
            Printf.printf "DEAD"
          else
            List.iter (fun i -> Printf.printf "%d " i.instr_num) used_by;
          Printf.printf ") "                        
      ) enter_state.stack;
    end
  else begin 
      Printf.printf "STATE %d" enter_state.state_num;
      print_newline ();
      Printf.printf "No liveness"; print_newline ();
      Array.iteri (fun i v -> 
          Printf.printf "%s=%s " (string_of_register regs.(i))
          (string_of_approx v)) 
      enter_state.registers;
      Array.iteri (fun i v -> 
          Printf.printf "ESP(%d)=%s " i (string_of_approx v))
      enter_state.stack;
    end;
  print_newline ();
  List.iter (fun (addr, block) ->
      Printf.printf "%s --> [ " (string_of_approx addr);
      Array.iter (fun v ->
          Printf.printf "%s, " (string_of_approx v)) block;
      Printf.printf "]"; print_newline ();
  ) enter_state.store
  
  
let rec bits32 r = 
  match r with
    St _ -> raise Exit
  | LowByte r -> r
  | LowWord r -> r
  | HighByte r -> r
  | r -> r

let print_instr instr =
  Printf.printf "[%d] %s" instr.instr_num (SimplePrint.string_of_instr instr);
  print_newline ()

let print_node node =
  Printf.printf ".L%d:" node.node_ident.label;
  print_newline ();
  Array.iter (fun instr -> 
      Printf.printf "\t%s" (SimplePrint.string_of_instr instr);
      print_newline ()) node.instrs;
  Printf.printf "\t%s" (SimplePrint.string_of_instr node.link);
  print_newline ()
  
let print_func func =
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes print_node

let rec simplify_const c =
  match c with
    Const_add (c1, c2) ->
      let c1 = simplify_const c1 in
      let c2 = simplify_const c2 in
      begin
        match c1,c2 with
          Const_int c1, Const_int c2 -> Const_int (c1+c2)
        | Const_int c1, Const_add (Const_int c2, c) ->
            Const_add (Const_int (c1+c2), c)
        | Const_add (Const_int c2, c), Const_int c1 ->
            Const_add (Const_int (c1+c2), c)
        | Const_add (Const_int n1, c1), Const_add (Const_int n2, c2) ->
            Const_add (Const_int (n1+n2), Const_add (c1, c2))
        | c, Const_int c2 -> Const_add (Const_int c2, c)
        | c, Const_sub (c1, c2) ->
            simplify_const (Const_sub(Const_add (c,c1), c2))
        | Const_sub (c1, c2),c ->
            simplify_const (Const_sub(Const_add (c,c1), c2))
        | _ -> Const_add (c1,c2)
      end
  | Const_sub (c1, c2) ->
      let c1 = simplify_const c1 in
      let c2 = simplify_const c2 in
      begin
        match c1,c2 with
          Const_int c1, Const_int c2 -> Const_int (c1-c2)
        | Const_add (Const_int n1, c), Const_int n2 ->
            Const_add (Const_int (n1-n2), c)
        | Const_int n1, Const_add (Const_int n2, c) ->
            Const_sub (Const_int (n1-n2), c)
        | Const_add (Const_int n1, c1), Const_add (Const_int n2, c2) ->
            Const_sub (Const_add (Const_int (n1-n2), c1), c2)
        | Const_add (Const_int n1, c1), Const_sub (Const_int n2, c2) ->
            Const_add (Const_int (n1-n2), Const_add (c1, c2))
        | Const_sub (Const_int n1, c1), Const_add (Const_int n2, c2) ->
            Const_sub (Const_int (n1-n2), Const_add (c1, c2))
        | Const_sub (Const_int n1, c1), Const_sub (Const_int n2, c2) ->
            Const_sub (Const_add (Const_int (n1-n2),c2), c1)
        | _ -> Const_sub (c1,c2)
      end
  | _ -> c
      
      
let opcode_of_operation op = match op with
    Shr -> Shrl
  | Add -> Addl
  | Sub -> Subl
  | And -> Andl
  | Xor -> Xorl
  | Or -> Orl
  | Sar -> Sarl
  | Sal -> Sall
  | Mul -> Imull
  | Div -> Idivl
  | _ -> assert false     
      