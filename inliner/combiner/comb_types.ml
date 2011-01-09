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


(* D'apres Davidson et Fraser, 5 phases:
- Expander
- Cacher (Common sub-expressions elimination)
- Combiner (Combinaison of RTLs -> RTL)
- Assigner (RTL -> Instructions)
- Allocater (pseudo-regs -> registers)
*)

(***

When it works, we must simplify it, by using something similar to what
Fraser and al. used in po, ie multiple passes:
- in the first pass, simplify every thing that can be by combining simple
operations.
- in the second pass, apply strengh reduction.
- in the third pass, new simplification (if some strenght reduction was
  possible only).
- in the fourth pass, compute liveness.
- in the fifth pass, combine operations as close as possible to the
instruction set possibilities.
- in the sixth pass, translate to machine code.

  ***)

(****

Using a good structure is important: Until now, we have used an array
of pseudo-registers that cannot be extended by insertion. This is clearly
a bad approach, since the "distribute" pass creates values which are
not computable in a single step. Thus, the combine pass is unable to
improve these values.

***)




(****
Le combineur:

Il prend la sortie de Analysis (nodes avec valeurs et liveness) et 
simplifie le code.
eax=r0, ebx=r1, ecx=r2, edx=r3, esi=r4, edi=r5, ebp=r6

 movl    (%ecx), %esi       r7 = mem[r2]  (movl (%r2), %r7)
 movl    %ebx, %edi         r8 = r1       (movl %r1, %r8)
 sarl    $1, %edi           r9 = r8 >> 1  (movl %r8, %r9; sarl $1, %r9)
 sarl    $1, %esi           r10 = r7 >> 1 (movl %r7, %r10; sarl $1, %r10)
 movw    %si, (%eax, %edi)
                            r11 = r0 + r9 (leal (%r0, %r9), %r11) 
                          * mem[r11] = lw(r10) (movw LOWWORD(%r10), (%r11))
  
 movl    %ebx, %esi         r12 = r1      (movl %r1, %r12)
 addl    $4, %esi           r13 = r12 + 4 (leal 4(%r12), %r13)
 movl    4(%ecx), %ebp      r14 = r2 + 4  (leal 4(%r2), %r14)
                            r15 = mem[r14](movl (%r14), %r15) 
 sarl    $1, %esi           r16 = r13 >> 1(movl %r13, %r16; sarl $1, %r16)
 sarl    $1, %ebp           r17 = r15 >> 1(movl %r15, %r17; sarl $1, %r17)
 movw    %bp, (%eax, %esi)  r18 = r0 + r16(leal (%r0, %r16), %r18)
                          * mem[r18] = lw(r17) (movw LOWWORD(%r17), (%r18))

 movl    $1, %eax           r19 = 1       (movl $1, %r19)

 LIVE                       eax = r19      (movl %r19, %eax)

======================================================================
  
  eax=r0, ebx=r1, ecx=r2

 r7 = mem[r2]
 r8 = r1
 r9 = r8 >> 1
 r10 = r7 >> 1
 r11 = r0 + r9
 mem[r11] = lw(r10)
 r12 = r1
 r13 = r12 + 4
 r14 = r2 + 4
 r15 = mem[r14]
 r16 = r13 >> 1
 r17 = r15 >> 1
 r18 = r0 + r16
 mem[r18] = lw(r17)
 r19 = 1
 eax = r19

  ==========================================================
  
 r7 = mem[r2]
 r8 = r1
 r9 = r8 >> 1
 r10 = r7 >> 1
 r11 = r0 + r9
 mem[r11] = lw(r10)
 r12 = r8
 r13 = r12 + 4
 r14 = r2 + 4
 r15 = mem[r14]
 r16 = r13 >> 1
 r17 = r15 >> 1
 r18 = r0 + r16
 mem[r18] = lw(r17)
 r19 = 1
 eax = r19

===================================================================

 AFTER COMBINER:
  
 r7 = mem[r2]
 r8 = r1
 r9 = r8 >> 1
 r10 = r7 >> 1
 r11 = r0 + r9
 mem[r11] = lw(r10) **
 r12 = r1     
 r13 = r12 + 4
 r14 = r2 + 4
 r15 = mem[r14]
 r16 = r13 >> 1
 r17 = r15 >> 1
 r18 = r11 + 2
 mem[r18] = lw(r17) **
 r19 = 1
 eax = r19          **

====================================================================

  AFTER LIVENESS:

  r0 = eax 
  r1 = ebx
  r2 = ecx
  r7 = mem[r2]                    -- r0 r1 r2 r7
  r9 = r1 >> 1        r1 DEAD     -- r0 r2 r7 r9
  r10 = r7 >> 1       r7 DEAD     -- r0 r2 r9 r10
  r11 = r0 + r9       r9, r0 DEAD -- r2 r10 r11 
  mem[r11] = lw(r10)  r10 DEAD    -- r2 r11
  r14 = r2 + 4        r2 DEAD     -- r11 r14
  r15 = mem[r14]      r14 DEAD    -- r11 r15
  r17 = r15 >> 1      r15 DEAD    -- r11 r17
  r18 = r11 + 2       r11 DEAD    -- r17 r18
  mem[r18] = lw[r17]  r18, r17 DEAD 
  r19 = 1                         -- r19
  eax = r19           r19 DEAD    -- eax



  
  
        movl    %ebx, %esi
        addl    $8, %esi
        movl    8(%ecx), %ebp
        sarl    $1, %esi
        sarl    $1, %ebp
        movw    %bp, (%eax, %esi)
        movl    %ebx, %esi
        addl    $12, %esi
        movl    12(%ecx), %ebp
        sarl    $1, %esi
        sarl    $1, %ebp
        movw    %bp, (%eax, %esi)
...
  
****)

open Args
open Asm

  

let stat_combine_ins = ref 0
  
let printf a =
  if !debug_combine then 
    Printf.printf a
  else
  let rec f = (fun _ -> Obj.magic f) in Obj.magic f

let print_newline () = 
  if !debug_combine then 
    Pervasives.print_newline ()

let print_instr instr =
  if !debug_combine then 
    print_instr instr

type force_map =
| Map_reg8
| Map_reg of int
    
type value =
| C_const of constant
| C_load of part * value
| C_op of operation * value * value
| C_cast of part * value
| C_stack of int
| C_reg of reg
| C_store of part * value * value (* size, src,dst *)
| C_param of int
  (*
  This is an unknow instruction. It should not:
  - be applied on the stack
  - modify any register
*)
| C_unknown of opcode * register argument list * value list
  
and part = 
  C_LowByte
| C_HighByte
| C_LowWord
| C_Long
| C_SignWL
| C_SignBL
| C_ZeroBL
| C_ZeroWL
| C_Float
  
  (***********
  
  reg should use numbers only at the end, when liveness and death is 
  concerned. we must be more careful, and allocate numbers to registers.
  
  ***********)
  
  
and reg = {
    mutable reg_num : int;
    mutable reg_value : value;
    mutable reg_used : reg list;
    mutable used_by_value : int;
    mutable used_by_addr : int;
    (* This is used to indicate a register preferred location *)
    mutable prefered_reg: int option;
    
    (* This indicates that the register must be in a given location at a
    given position. *)
    mutable force_reg : (reg * force_map) list;
    mutable map: int;
    mutable nused : int;
    mutable tail : reg list;
  }

and state = {
    pseudo_regs : reg list;
    regs : reg array;
    ninstrs : int;
    next_reg : int;
    old_instrs : instruction list;
    new_liveness : instruction list array;
  }

  
type combiner_state = {
    (* init_regs: liste des registres vivants a l'entrée du combiner.
    Ces registres sont pré-alloués et ne peuvent donc pas être alloués
    à nouveau. *)  
    mutable begin_regs : reg list;
    
    (* pseudo_regs: liste des pseudo-registres utilisés dans le combiner
    pour générer le calcul du bloc. *)
    mutable bloc_regs: reg list;
  
    (* live_regs: tableau contenant, pour chaque register physique 
    vivant à la fin du calcul, le pseudo-registre contenant sa valeur. *)
    
    mutable end_regs : reg option array;
  }
  
let rec no_store r1 r2 =
  if r1 == r2 then begin
      printf "NO STORE until r%d" r1.reg_num;
      print_newline ();
      true end else
  match r1.reg_value with
    C_store _ -> 
      printf "HAS STORE in r%d" r1.reg_num;
      print_newline ();
      false
  | _ -> 
      match r1.tail with
        [] -> assert false
      | r1 :: _ -> no_store r1 r2
  
let rec list_iter f list =
  match list with [] -> () | r :: tail -> f r; list_iter f tail

let rec list_iter_tail f list =
  match list with [] -> () | r :: tail -> 
      r.tail <- tail;
      f r; list_iter_tail f tail
  
let rec list_riter f list =
  match list with [] -> () | r :: tail -> list_riter f tail; f r

  
let string_of_part part = match part with
    C_LowByte -> "b"
  | C_HighByte -> "h"
  | C_LowWord -> "w"
  | C_Long -> "l"
  | C_SignWL -> "swl"
  | C_SignBL -> "sbl"
  | C_ZeroBL -> "zbl"
  | C_ZeroWL -> "zwl"
  | C_Float -> "f"
      
let string_of_operation op = match op with
    Add -> "+"
  | Sub -> "-of"
  | Not -> "not"
  | And -> "&"
  | Or -> "|"
  | Xor -> "^"
  | Shr -> ">>"
  | Sar -> "a>>"
  | Sal -> "<<"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | _ -> assert false
    
let rec string_of_value v =
  match v with
    C_const c -> SimplePrint.string_of_const c
  | C_load (part,v) -> Printf.sprintf "mem_%s[%s]"
        (string_of_part part)
      (string_of_value v)
  | C_op (op,v1,v2) ->
      Printf.sprintf "(%s %s %s)"
        (string_of_value v1)
      (string_of_operation op)
      (string_of_value v2)
  | C_cast (part,v) -> Printf.sprintf "cast_%s[%s]"
        (string_of_part part)
      (string_of_value v)
  | C_stack int -> Printf.sprintf "%d(%s)" int "%esp"
  | C_reg reg -> Printf.sprintf "r%d" reg.reg_num
  | C_store (part, src, dst) ->
      Printf.sprintf "mem_%s[%s] <- %s" 
        (string_of_part part)
      (string_of_value dst)
      (string_of_value src)
  | C_param i -> string_of_register regs.(i)
  | C_unknown (opcode , args, values) -> 
      Printf.sprintf "UNKNOWN: %s%s%s" (string_of_code opcode)
      (match args with
          [] -> ""
        | [arg] -> " "^(SimplePrint.string_of_arg arg)
        | [arg1;arg2] -> 
            Printf.sprintf " %s, %s" (
              SimplePrint.string_of_arg arg1) (
              SimplePrint.string_of_arg arg2)
        | _ -> assert false)
      (match values with
          [] -> ""
        | [v] -> " "^(string_of_value v)
        | [v1;v2] -> 
            Printf.sprintf " %s, %s" (string_of_value v1) (string_of_value v2)
        | _ -> assert false)
            

(* We use a limited number of semi_regs (200).*)
let no_value = C_stack 0
let max_pseudo_regs = 200

let init_pseudo_regs = List.map (fun i ->
      { reg_num = i; 
        reg_value = C_param i; 
        reg_used = [];
        used_by_addr = 0; 
        used_by_value = 0; 
        prefered_reg = None;
        force_reg = [];
        map = i; nused = 0; tail = [];
      }        
  ) [6;5;4;3;2;1;0]
  
let init_regs =  Array.of_list (List.rev init_pseudo_regs)

let min_combine_instrs = 2
    
let init_state = {
    pseudo_regs = init_pseudo_regs;
    regs = init_regs;
    next_reg = 7;
    ninstrs = 0;
    old_instrs = [];
    new_liveness = [||];    
  }
        

let string_of_list f list =
  let rec iter list =
    match list with 
      [] -> ""
    | a :: tail ->
        Printf.sprintf ",%s%s" (f a) (iter tail)
  in
  match list with
    [] -> ""
  | a :: tail ->
      Printf.sprintf "%s%s" (f a) (iter tail)
  
let print_rtl label pseudo_regs =
  if !debug_combine then begin
      printf "%s" label; print_newline ();
      printf "BEGIN RTL"; print_newline ();
      list_riter (fun r ->
          printf "r%d%s = %s"        
            r.reg_num
            (if r.used_by_value + r.used_by_addr <> 0 then
              Printf.sprintf "[%d,%d%s%s]" r.used_by_value r.used_by_addr
                (match r.prefered_reg with
                  Some i ->
                    Printf.sprintf ":P%s" (string_of_register regs.(i))
                | _ -> "") 
              (string_of_list (fun (r,map) ->
                    Printf.sprintf "{r%d in %s}" r.reg_num 
                    (match map with
                      Map_reg i -> (string_of_register regs.(i))
                    | Map_reg8 -> "reg8")
                ) r.force_reg)
            else "")
          (string_of_value r.reg_value);
          print_newline ()) (List.rev pseudo_regs);
      printf "END RTL"; print_newline ()
    end

let rec map f v =
  let v = f v in
  match v with
  | C_cast (cast, v) -> C_cast (cast, map f v)
  | C_op (op, v1, v2) -> C_op (op, map f v1, map f v2)
  | C_load (size,v) -> C_load (size, map f v)
  | C_store (size, src, dst) ->
      C_store (size, map f src, map f dst)  
  | C_unknown (opcode, args, values) ->
      C_unknown (opcode, args, List.map (map f) values)
  | _ -> v      
  
let rec value_iter f v =
  f v;
  match v with
  | C_cast (cast, v) -> value_iter f v
  | C_op (op, v1, v2) -> value_iter f v1; value_iter f v2
  | C_load (size,v) -> value_iter f v
  | C_store (size, src, dst) -> value_iter f src; value_iter f dst
  | C_unknown (opcode, args, values) -> List.iter (value_iter f) values
  | _ -> ()
