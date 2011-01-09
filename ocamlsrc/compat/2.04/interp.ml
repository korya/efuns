(***********************************************************************)
(*                                                                     *)
(*                           Interp.ml                                 *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(*
   This module implement an interpretor for OCAML bytecode. The bytecode is
supposed to be linked (ie symbols are resolved). Closures are compiled as
native closures taking only one argument each time. For example, GRAB creates
a native closure waiting for "arity" arguments, then putting them on the stack
and calling the interpretor with a new pc.
*)


open Instructs
open Printf

  
  type block =
| Block0 of int
| Block1 of int
| Block2 of int
| Block3 of int
| Block4 of int
| Block5 of int
| Block6 of int
| Block7 of int
| Block8 of int
| Block9 of int
| Block10 of int
| Block11 of int
| Block12 of int
| Block13 of int
| Block14 of int
| Block15 of int
| Block16 of int
| Block17 of int
| Block18 of int
| Block19 of int
| Block20 of int
| Block21 of int
| Block22 of int
| Block23 of int
| Block24 of int
| Block25 of int
| Block26 of int
| Block27 of int
| Block28 of int
| Block29 of int
| Block30 of int
| Block31 of int
| Block32 of int
| Block33 of int
| Block34 of int
| Block35 of int
| Block36 of int
| Block37 of int
| Block38 of int
| Block39 of int
| Block40 of int
| Block41 of int
| Block42 of int
| Block43 of int
| Block44 of int
| Block45 of int
| Block46 of int
| Block47 of int
| Block48 of int
| Block49 of int
| Block50 of int
| Block51 of int
| Block52 of int
| Block53 of int
| Block54 of int
| Block55 of int
| Block56 of int
| Block57 of int
| Block58 of int
| Block59 of int
| Block60 of int
| Block61 of int
| Block62 of int
| Block63 of int
| Block64 of int
| Block65 of int
| Block66 of int
| Block67 of int
| Block68 of int
| Block69 of int
| Block70 of int
| Block71 of int
| Block72 of int
| Block73 of int
| Block74 of int
| Block75 of int
| Block76 of int
| Block77 of int
| Block78 of int
| Block79 of int
| Block80 of int
| Block81 of int
| Block82 of int
| Block83 of int
| Block84 of int
| Block85 of int
| Block86 of int
| Block87 of int
| Block88 of int
| Block89 of int
| Block90 of int
| Block91 of int
| Block92 of int
| Block93 of int
| Block94 of int
| Block95 of int
| Block96 of int
| Block97 of int
| Block98 of int
| Block99 of int
| Block100 of int
| Block101 of int
| Block102 of int
| Block103 of int
| Block104 of int
| Block105 of int
| Block106 of int
| Block107 of int
| Block108 of int
| Block109 of int
| Block110 of int
| Block111 of int
| Block112 of int
| Block113 of int
| Block114 of int
| Block115 of int
| Block116 of int
| Block117 of int
| Block118 of int
| Block119 of int
| Block120 of int
| Block121 of int
| Block122 of int
| Block123 of int
| Block124 of int
| Block125 of int
| Block126 of int
| Block127 of int
| Block128 of int
| Block129 of int
| Block130 of int
| Block131 of int
| Block132 of int
| Block133 of int
| Block134 of int
| Block135 of int
| Block136 of int
| Block137 of int
| Block138 of int
| Block139 of int
| Block140 of int
| Block141 of int
| Block142 of int
| Block143 of int
| Block144 of int
| Block145 of int
| Block146 of int
| Block147 of int
| Block148 of int
| Block149 of int
| Block150 of int
| Block151 of int
| Block152 of int
| Block153 of int
| Block154 of int
| Block155 of int
| Block156 of int
| Block157 of int
| Block158 of int
| Block159 of int
| Block160 of int
| Block161 of int
| Block162 of int
| Block163 of int
| Block164 of int
| Block165 of int
| Block166 of int
| Block167 of int
| Block168 of int
| Block169 of int
| Block170 of int
| Block171 of int
| Block172 of int
| Block173 of int
| Block174 of int
| Block175 of int
| Block176 of int
| Block177 of int
| Block178 of int
| Block179 of int
| Block180 of int
| Block181 of int
| Block182 of int
| Block183 of int
| Block184 of int
| Block185 of int
| Block186 of int
| Block187 of int
| Block188 of int
| Block189 of int
| Block190 of int
| Block191 of int
| Block192 of int
| Block193 of int
| Block194 of int
| Block195 of int
| Block196 of int
| Block197 of int
| Block198 of int
| Block199 of int
| Block200 of int
| Block201 of int
| Block202 of int
| Block203 of int
| Block204 of int
| Block205 of int
| Block206 of int
| Block207 of int
| Block208 of int
| Block209 of int
| Block210 of int
| Block211 of int
| Block212 of int
| Block213 of int
| Block214 of int
| Block215 of int
| Block216 of int
| Block217 of int
| Block218 of int
| Block219 of int
| Block220 of int
| Block221 of int
| Block222 of int
| Block223 of int
| Block224 of int
| Block225 of int
| Block226 of int
| Block227 of int
| Block228 of int
| Block229 of int
| Block230 of int
| Block231 of int
| Block232 of int
| Block233 of int
| Block234 of int
| Block235 of int
| Block236 of int
| Block237 of int
| Block238 of int
| Block239 of int
| Block240 of int
| Block241 of int
| Block242 of int
| Block243 of int
| Block244 of int
| Block245 of int
| Block246 of int
| Block247 of int

let blocktag = function
  | Block0 _  -> 0
| Block1 _  -> 1
| Block2 _  -> 2
| Block3 _  -> 3
| Block4 _  -> 4
| Block5 _  -> 5
| Block6 _  -> 6
| Block7 _  -> 7
| Block8 _  -> 8
| Block9 _  -> 9
| Block10 _  -> 10
| Block11 _  -> 11
| Block12 _  -> 12
| Block13 _  -> 13
| Block14 _  -> 14
| Block15 _  -> 15
| Block16 _  -> 16
| Block17 _  -> 17
| Block18 _  -> 18
| Block19 _  -> 19
| Block20 _  -> 20
| Block21 _  -> 21
| Block22 _  -> 22
| Block23 _  -> 23
| Block24 _  -> 24
| Block25 _  -> 25
| Block26 _  -> 26
| Block27 _  -> 27
| Block28 _  -> 28
| Block29 _  -> 29
| Block30 _  -> 30
| Block31 _  -> 31
| Block32 _  -> 32
| Block33 _  -> 33
| Block34 _  -> 34
| Block35 _  -> 35
| Block36 _  -> 36
| Block37 _  -> 37
| Block38 _  -> 38
| Block39 _  -> 39
| Block40 _  -> 40
| Block41 _  -> 41
| Block42 _  -> 42
| Block43 _  -> 43
| Block44 _  -> 44
| Block45 _  -> 45
| Block46 _  -> 46
| Block47 _  -> 47
| Block48 _  -> 48
| Block49 _  -> 49
| Block50 _  -> 50
| Block51 _  -> 51
| Block52 _  -> 52
| Block53 _  -> 53
| Block54 _  -> 54
| Block55 _  -> 55
| Block56 _  -> 56
| Block57 _  -> 57
| Block58 _  -> 58
| Block59 _  -> 59
| Block60 _  -> 60
| Block61 _  -> 61
| Block62 _  -> 62
| Block63 _  -> 63
| Block64 _  -> 64
| Block65 _  -> 65
| Block66 _  -> 66
| Block67 _  -> 67
| Block68 _  -> 68
| Block69 _  -> 69
| Block70 _  -> 70
| Block71 _  -> 71
| Block72 _  -> 72
| Block73 _  -> 73
| Block74 _  -> 74
| Block75 _  -> 75
| Block76 _  -> 76
| Block77 _  -> 77
| Block78 _  -> 78
| Block79 _  -> 79
| Block80 _  -> 80
| Block81 _  -> 81
| Block82 _  -> 82
| Block83 _  -> 83
| Block84 _  -> 84
| Block85 _  -> 85
| Block86 _  -> 86
| Block87 _  -> 87
| Block88 _  -> 88
| Block89 _  -> 89
| Block90 _  -> 90
| Block91 _  -> 91
| Block92 _  -> 92
| Block93 _  -> 93
| Block94 _  -> 94
| Block95 _  -> 95
| Block96 _  -> 96
| Block97 _  -> 97
| Block98 _  -> 98
| Block99 _  -> 99
| Block100 _  -> 100
| Block101 _  -> 101
| Block102 _  -> 102
| Block103 _  -> 103
| Block104 _  -> 104
| Block105 _  -> 105
| Block106 _  -> 106
| Block107 _  -> 107
| Block108 _  -> 108
| Block109 _  -> 109
| Block110 _  -> 110
| Block111 _  -> 111
| Block112 _  -> 112
| Block113 _  -> 113
| Block114 _  -> 114
| Block115 _  -> 115
| Block116 _  -> 116
| Block117 _  -> 117
| Block118 _  -> 118
| Block119 _  -> 119
| Block120 _  -> 120
| Block121 _  -> 121
| Block122 _  -> 122
| Block123 _  -> 123
| Block124 _  -> 124
| Block125 _  -> 125
| Block126 _  -> 126
| Block127 _  -> 127
| Block128 _  -> 128
| Block129 _  -> 129
| Block130 _  -> 130
| Block131 _  -> 131
| Block132 _  -> 132
| Block133 _  -> 133
| Block134 _  -> 134
| Block135 _  -> 135
| Block136 _  -> 136
| Block137 _  -> 137
| Block138 _  -> 138
| Block139 _  -> 139
| Block140 _  -> 140
| Block141 _  -> 141
| Block142 _  -> 142
| Block143 _  -> 143
| Block144 _  -> 144
| Block145 _  -> 145
| Block146 _  -> 146
| Block147 _  -> 147
| Block148 _  -> 148
| Block149 _  -> 149
| Block150 _  -> 150
| Block151 _  -> 151
| Block152 _  -> 152
| Block153 _  -> 153
| Block154 _  -> 154
| Block155 _  -> 155
| Block156 _  -> 156
| Block157 _  -> 157
| Block158 _  -> 158
| Block159 _  -> 159
| Block160 _  -> 160
| Block161 _  -> 161
| Block162 _  -> 162
| Block163 _  -> 163
| Block164 _  -> 164
| Block165 _  -> 165
| Block166 _  -> 166
| Block167 _  -> 167
| Block168 _  -> 168
| Block169 _  -> 169
| Block170 _  -> 170
| Block171 _  -> 171
| Block172 _  -> 172
| Block173 _  -> 173
| Block174 _  -> 174
| Block175 _  -> 175
| Block176 _  -> 176
| Block177 _  -> 177
| Block178 _  -> 178
| Block179 _  -> 179
| Block180 _  -> 180
| Block181 _  -> 181
| Block182 _  -> 182
| Block183 _  -> 183
| Block184 _  -> 184
| Block185 _  -> 185
| Block186 _  -> 186
| Block187 _  -> 187
| Block188 _  -> 188
| Block189 _  -> 189
| Block190 _  -> 190
| Block191 _  -> 191
| Block192 _  -> 192
| Block193 _  -> 193
| Block194 _  -> 194
| Block195 _  -> 195
| Block196 _  -> 196
| Block197 _  -> 197
| Block198 _  -> 198
| Block199 _  -> 199
| Block200 _  -> 200
| Block201 _  -> 201
| Block202 _  -> 202
| Block203 _  -> 203
| Block204 _  -> 204
| Block205 _  -> 205
| Block206 _  -> 206
| Block207 _  -> 207
| Block208 _  -> 208
| Block209 _  -> 209
| Block210 _  -> 210
| Block211 _  -> 211
| Block212 _  -> 212
| Block213 _  -> 213
| Block214 _  -> 214
| Block215 _  -> 215
| Block216 _  -> 216
| Block217 _  -> 217
| Block218 _  -> 218
| Block219 _  -> 219
| Block220 _  -> 220
| Block221 _  -> 221
| Block222 _  -> 222
| Block223 _  -> 223
| Block224 _  -> 224
| Block225 _  -> 225
| Block226 _  -> 226
| Block227 _  -> 227
| Block228 _  -> 228
| Block229 _  -> 229
| Block230 _  -> 230
| Block231 _  -> 231
| Block232 _  -> 232
| Block233 _  -> 233
| Block234 _  -> 234
| Block235 _  -> 235
| Block236 _  -> 236
| Block237 _  -> 237
| Block238 _  -> 238
| Block239 _  -> 239
| Block240 _  -> 240
| Block241 _  -> 241
| Block242 _  -> 242
| Block243 _  -> 243
| Block244 _  -> 244
| Block245 _  -> 245
| Block246 _  -> 246
| Block247 _  -> 247


module MyObj = struct  
    type t = A of int | B of float | C of string
    
    external repr : 'a -> t = "%identity"
    external obj : t -> 'a = "%identity"
    external magic : 'a -> 'b = "%identity"
    external is_block : t -> bool = "obj_is_block"
    external tag : t -> int = "obj_tag"
    external size : t -> int = "%obj_size"
    external field : t -> int -> t = "%obj_field"
    external set_field : t -> int -> t -> unit = "%obj_set_field"
    external new_block : int -> int -> t = "obj_block"
    external dup : t -> t = "obj_dup"
    external truncate : t -> int -> unit = "obj_truncate"
    external makeblock : int -> int -> t -> t array -> int -> t = "makeblock"
    external makeblock1 : int -> t -> t = "makeblock1"
    external makeblock2 : int -> t -> t -> t = "makeblock2"
    external makeblock3 : int -> t -> t -> t -> t = "makeblock3"
  end

open MyObj
  
(* caml_globals: table des donnees globales = 
        exceptions predefines + modules natifs + modules bytecode 
*)
(* table: module_name -> caml_globals_slot *)

(* a BUG appear for more than 10000 globals .... but can I clean this ? *)
let caml_globals = Array.create 10000 (repr 0)
let (globals : ( string, int) Hashtbl.t) = Hashtbl.create 53

let predef_exn (exn : exn) = MyObj.field (MyObj.repr exn) 0
let predef_exceptions = [
    "Out_of_memory", Out_of_memory;
    "Sys_error", (Sys_error "");
    "Failure",  (Failure "");
    "Invalid_argument",  (Invalid_argument "");
    "End_of_file",  End_of_file;
    "Division_by_zero",  Division_by_zero;
    "Not_found",  Not_found;
    "Match_failure",  (Match_failure ("",0,0));
    "Stack_overflow",  Stack_overflow;
    "Sys_blocked_io", Sys_blocked_io
  ]
  
let exceptions () = 
  Array.to_list
    (Array.mapi 
      (fun i name -> 
        let id =
          try List.assoc name Predef.builtin_values
          with Not_found -> failwith  "Interp: Undefined predef value" in
        let obj = 
          try List.assoc name predef_exceptions with Not_found ->
              failwith (Printf.sprintf "Interp: Undefined exception %s" name)
        in
        caml_globals.(i) <- predef_exn obj;
        id)
    Runtimedef.builtin_exceptions)


let next_free_global = ref 10
  
let caml_prims = Array.create 500 (repr 0)
let prims = Hashtbl.create 53

external asm_getglobal : int -> MyObj.t = "asm_getglobal" 
external asm_getmap : unit -> string = "asm_getmap"

let (globals_map : (string * string) list) = 
  Marshal.from_string (asm_getmap ()) 0

let (first_native_module,last_native_module) = 
  let rec iter i names =
    match names with
      [] -> ()
    | (mod_name,crc) :: tail -> 
(*        Printf.printf "Put %s in %d\n" mod_name !next_free_global; *)
        Hashtbl.add globals mod_name !next_free_global;
        incr next_free_global;
        iter (i+1) tail
  in
  let first_native_module = !next_free_global in
  iter 0 globals_map;
  (first_native_module,!next_free_global - 1)

let getglobal i = 
  repr caml_globals.(i)
  
let install_module i =
  if i>= first_native_module && i<= last_native_module then
    caml_globals.(i) <- 
        (MyObj.magic (asm_getglobal (i - first_native_module)))
    
  (*
  if i>= first_native_module && i<= last_native_module then
    begin
      let modul = (MyObj.magic (asm_getglobal (i - first_native_module))) in
      modul
    end
  else
    repr caml_globals.(i)
*)
let setglobal i v = 
  caml_globals.(i) <- v

let stack_size = 20000
  
let global_stack = Array.create stack_size (repr 0)
let extern_sp = ref stack_size

let value_size = Sys.word_size / 8
let step = ref 0
(*
  Les closures Bytecode sont des fonctions native prenant ses arguments un par
un. Lorsque tous les arguments d'une closure Bytecode sont disponibles,
la fonction native les empile et lance le Bytecode.
*)
exception ExitInterp
exception PopTrap

let poptrap = PopTrap
let poppc = ref 0
let popacc = ref (repr 0)


external unsigned : 'a -> int = "%identity"
let closure (accu : t) = (magic accu : t -> 'a)
let exit = ExitInterp
          
let rec interp (code : int array) pc accu env recenv =
  (* printf "interp"; print_newline ();  *)
  let stack = (magic global_stack : t array) in
  let caml_prims = (magic caml_prims : t array) in
  let sp = ref !extern_sp in
  let pc = ref (pc - 1) in
  let accu = ref (accu : t) in 
  let env = (env : t array) in
  try
    while true do      
      incr pc;      
(*
      incr step;
      printf "%d: PC(%d) %s SP(%d)" !step !pc Instructs.opnames.(unsigned code.(!pc)) !sp;
      print_newline ();
*)
      match magic code.(!pc) with
      | OpACC0 -> accu := stack.(!sp) 
      | OpACC1 -> accu := stack.(!sp + 1)
      | OpACC2 -> accu := stack.(!sp + 2)
      | OpACC3 -> accu := stack.(!sp + 3)
      | OpACC4 -> accu := stack.(!sp + 4)
      | OpACC5 -> accu := stack.(!sp + 5)
      | OpACC6 -> accu := stack.(!sp + 6)
      | OpACC7 -> accu := stack.(!sp + 7)
      | OpACC -> 
          incr pc; let offset = unsigned code.(!pc) in
          accu := stack.(!sp + offset)
      | OpPUSH -> decr sp; stack.(!sp) <- !accu
      | OpPUSHACC0 -> decr sp; stack.(!sp) <- !accu
      | OpPUSHACC1 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 1)
      | OpPUSHACC2 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 2)
      | OpPUSHACC3 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 3)
      | OpPUSHACC4 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 4)
      | OpPUSHACC5 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 5)
      | OpPUSHACC6 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 6)
      | OpPUSHACC7 -> decr sp; stack.(!sp) <- !accu; accu := stack.(!sp + 7)
      | OpPUSHACC -> decr sp; stack.(!sp) <- !accu; 
          incr pc; let offset = unsigned code.(!pc) in
          accu := stack.(!sp + offset)
      | OpPOP -> 
          incr pc; let offset = unsigned code.(!pc) in
          sp := !sp + offset
      | OpASSIGN ->
          incr pc; let offset = unsigned code.(!pc) in
          stack.(!sp + offset) <- !accu;
          accu := (repr 0)
      | OpENVACC1 -> accu := env.(1)
      | OpENVACC2 -> accu := env.(2)
      | OpENVACC3 -> accu := env.(3)
      | OpENVACC4 -> accu := env.(4)
      | OpENVACC ->
          incr pc; let offset = unsigned code.(!pc) in
          accu := env.(offset)
      | OpPUSHENVACC1  ->
          decr sp; stack.(!sp) <- !accu; accu := env.(1)
      | OpPUSHENVACC2  ->
          decr sp; stack.(!sp) <- !accu; accu := env.(2)
      | OpPUSHENVACC3  ->
          decr sp; stack.(!sp) <- !accu; accu := env.(3)
      | OpPUSHENVACC4  ->
          decr sp; stack.(!sp) <- !accu; accu := env.(4)
      | OpPUSHENVACC  ->
          incr pc; let offset = unsigned code.(!pc) in
          decr sp; stack.(!sp) <- !accu; accu := env.(offset)
      | OpPUSH_RETADDR  ->
          incr pc; let offset = magic code.(!pc) in
          stack.(!sp-1) <- repr (!pc + offset - 1);
          sp := !sp -3;
      | OpAPPLY  ->
          let nargs = unsigned code.(!pc+1) in
          extern_sp := !sp;
          sp := !sp + nargs;
          for i = nargs - 1 downto 0 do
            accu := (magic !accu) stack.(!sp - i - 1);
          done;
          pc := magic stack.(!sp+2); 
          sp := !sp+3;
      
      | OpAPPLY1  ->
          let arg = stack.(!sp) in 
          incr sp;
          extern_sp := !sp;
          accu := (magic !accu) arg;
      | OpAPPLY2  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          sp := !sp + 2;
          extern_sp := !sp;
          accu := (magic !accu) arg1 arg2;
      | OpAPPLY3  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          let arg3 = stack.(!sp + 2) in
          sp := !sp + 3;
          extern_sp := !sp;
          accu := (magic !accu) arg1 arg2 arg3;
      | OpAPPTERM  ->
          let nargs = unsigned code.(!pc+1) in
          pc := !pc + 2; let slotsize = unsigned code.(!pc) in
          let trapsp = !sp + slotsize in
          let newsp = trapsp - nargs in
          for i = nargs - 1 downto 0 do
            stack.(newsp + i) <- stack.(!sp + i)
          done;
          extern_sp := newsp;
          for i = 0 to nargs - 1 do
            accu := (magic !accu) stack.(newsp + i);
          done;
          sp := trapsp;
          raise exit
      | OpAPPTERM1  ->
          let arg = stack.(!sp) in
          incr pc; let slotsize = unsigned code.(!pc) in
          sp := !sp + slotsize;
          extern_sp := !sp;
          accu := (magic !accu) arg;
          raise exit
      | OpAPPTERM2  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          incr pc; let slotsize = unsigned code.(!pc) in
          sp := !sp + slotsize;
          extern_sp := !sp;
          accu := (magic !accu) arg1 arg2;
          raise exit
      | OpAPPTERM3  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp + 1) in
          let arg3 = stack.(!sp + 2) in
          incr pc; let slotsize = unsigned code.(!pc) in
          sp := !sp + slotsize;
          extern_sp := !sp;
          accu := (magic !accu) arg1 arg2 arg3;
          raise exit
      | OpRETURN  -> 
          (* inutile 
          incr pc; let offset = unsigned code.(!pc) in
          sp := !sp + offset; *)
          raise exit
      | OpRESTART  -> assert false
      | OpGRAB  ->
          incr pc; let required = unsigned code.(!pc) in
          let goto = !pc + 1 in
          let arg1 = stack.(!sp) in incr sp;
          accu :=
          (match required with
              1 -> repr
                  (fun arg2 ->
                    let sp = extern_sp in                    
                    let stack = (magic global_stack : t array) in
                    let trapsp = !sp in
                    sp := !sp - 2;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp code goto (repr 0) env recenv) in
                    sp := trapsp;
                    v
                )
            | 2 -> repr
                  (fun arg2 arg3 ->
                    let sp = extern_sp in
                    let stack = (magic global_stack : t array) in
                    let trapsp = !sp in
                    sp := !sp - 3;
                    stack.(!sp+2) <- arg3;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp  code goto (repr 0) env recenv) in
                    sp := trapsp;
                    v
                )
            | 3 -> repr
                  (fun arg2 arg3 arg4 ->
                    let sp = extern_sp in
                    let stack = (magic global_stack : t array) in
                    let trapsp = !sp in
                    sp := !sp - 4;
                    stack.(!sp+3) <- arg4;
                    stack.(!sp+2) <- arg3;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp code goto (repr 0) env recenv) in
                    sp := trapsp;
                    v
                )
            | _ ->
              (*  let args = Array.create (required+1) (repr 0) in
                let rec f n arg =
                  args.(n) <- arg;
                  if n = required then
                    let stack = (magic global_stack : t array) in
                    let sp = extern_sp in
                    let trapsp = !sp in
                    sp := !sp - required - 1;
                    for i = required downto 0 do
                      stack.(!sp + i) <- args.(i)
                    done;                  
                    let v = magic (interp code goto (repr 0) env recenv) in
                    sp := trapsp;
                    v
                  else
                    (magic f : int -> t -> t) (n+1)
                in
            repr (f 0 arg1) *)
                let rec f nargs args arg =
                  if nargs = required then
                    let sp = extern_sp in
                    let trapsp = !sp in
                    let argss = Array.of_list (List.rev (arg::args)) in
                    sp := !sp - required - 1;
                    for i = 0 to required do
                      stack.(!sp + i) <- argss.(i)
                    done;                  
                    let v = magic (interp code goto (repr 0) env recenv) in
                    sp := trapsp;
                    v
                  else
                    (MyObj.magic f : int -> t list -> t -> t) (nargs+1)
                    (arg :: args)
                in
                repr (f 1 [arg1])
          );
          raise exit
      | OpCLOSURE  ->
          let nvars = unsigned code.(!pc+1) in
          pc := !pc+2; let goto = !pc + magic code.(!pc) in
          if nvars > 0 then (decr sp; stack.(!sp) <- !accu);
          let clos = magic (makeblock 0 (1+nvars) (repr goto) stack !sp) in
(*            
          let clos = Array.create (1+nvars) unit in
          clos.(0) <- repr goto;
          for i = 0 to nvars - 1 do
            clos.(i+1) <- stack.(!sp + i)
            done;
*)  
          sp := !sp + nvars;
          let recenv = (0,clos) in
          accu :=
          if code.(goto) = opGRAB then
            let required = code.(goto+1) in
            let goto = goto + 2 in
            let env = clos in
            match required with
              1 -> repr
                  (fun arg1 arg2 ->
                    let stack = (magic global_stack : t array) in
                    let sp = extern_sp in                    
                    let trapsp = !sp in
                    sp := !sp - 2;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp code goto (repr 0) env recenv) in
                    sp := trapsp;
                    v
                )
            | 2 -> repr
                  (fun arg1 arg2 arg3 ->
                    let stack = (magic global_stack : t array) in
                    let sp = extern_sp in
                    let trapsp = !sp in
                    sp := !sp - 3;
                    stack.(!sp+2) <- arg3;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp  code goto (repr 0) env recenv) in
                    sp := trapsp;
                    v
                )
            | 3 -> repr
                  (fun arg1 arg2 arg3 arg4 ->
                    let stack = (magic global_stack : t array) in
                    let sp = extern_sp in
                    let trapsp = !sp in
                    sp := !sp - 4;
                    stack.(!sp+3) <- arg4;
                    stack.(!sp+2) <- arg3;
                    stack.(!sp+1) <- arg2;
                    stack.(!sp) <- arg1;
                    let v = magic (interp code goto (repr 0) env recenv) in
                    sp := trapsp;
                    v
                )
            | _ ->
              (*  let args = Array.create (required+1) (repr 0) in
                let rec f n arg =
                  args.(n) <- arg;
                  if n = required then
                    let sp = extern_sp in
                    let stack = (magic global_stack : t array) in
                    let trapsp = !sp in
                    sp := !sp - required - 1;
                    for i = required downto 0 do
                      stack.(!sp + i) <- args.(i)
                    done;                  
                    let v = magic (interp code goto (repr 0) env recenv) in
                    sp := trapsp;
                    v
                  else
                    (magic f : int -> t -> t) (n+1)
                in
            repr (f 0) *)
                let rec f nargs args arg =
                  if nargs = required then
                    let sp = extern_sp in
                    let trapsp = !sp in
                    let argss = Array.of_list (List.rev (arg::args)) in
                    sp := !sp - required - 1;
                    for i = 0 to required do
                      stack.(!sp + i) <- argss.(i)
                    done;                  
                    let v = magic (interp code goto (repr 0) env recenv) in
                    sp := trapsp;
                    v
                  else
                    (MyObj.magic f : int -> t list -> t -> t) (nargs+1)
                    (arg :: args)
                in
                repr (f 0 [])
          else
            repr (fun arg -> 
                let sp = extern_sp in
                let stack = (magic global_stack : t array) in
                let trapsp = !sp in
                decr sp; stack.(!sp) <- arg;
                let v = interp code goto (repr 0) clos recenv in
                sp := trapsp;
                v);
      
      | OpCLOSUREREC  -> 
          incr pc; let nfuncs = unsigned code.(!pc) in
          incr pc; let nvars = unsigned code.(!pc) in
          if nvars > 0 then (decr sp; stack.(!sp) <- !accu);
          let size = nfuncs * 2 - 1 + nvars in
          let clos = Array.create size (repr 0) in
          for i = nvars - 1 downto 0 do
            clos.(nfuncs * 2 - 1 + i) <- stack.(!sp + i);
          done;
          sp := !sp + nvars;
          let oldpc = !pc + 1 in
          for i = nfuncs - 1 downto 0 do (* decreasing for Array.sub OK *)
            let goto = oldpc + magic code.(oldpc + i) in
            let env = Array.sub clos (i*2) (size - i*2) in
            let recenv = (i*2,clos) in
            let fi =               
              if code.(goto) = opGRAB then
                let required = code.(goto+1) in
                let goto = goto + 2 in
                match required with
                  1 -> repr
                      (fun arg1 arg2 ->
                        let sp = extern_sp in                    
                        let stack = (magic global_stack : t array) in
                        let trapsp = !sp in
                        sp := !sp - 2;
                        stack.(!sp+1) <- arg2;
                        stack.(!sp) <- arg1;
                        let v = magic (interp code goto (repr 0) env recenv) in
                        sp := trapsp;
                        v
                    )
                | 2 -> repr
                      (fun arg1 arg2 arg3 ->
                        let sp = extern_sp in
                        let stack = (magic global_stack : t array) in
                        let trapsp = !sp in
                        sp := !sp - 3;
                        stack.(!sp+2) <- arg3;
                        stack.(!sp+1) <- arg2;
                        stack.(!sp) <- arg1;
                        let v = magic (interp  code goto (repr 0) env recenv) in
                        sp := trapsp;
                        v
                    )
                | 3 -> repr
                      (fun arg1 arg2 arg3 arg4 ->
                        let trapsp = !extern_sp - 4 in
                        let stack = (magic global_stack : t array) in
                        stack.(trapsp+3) <- arg4;
                        stack.(trapsp+2) <- arg3;
                        stack.(trapsp+1) <- arg2;
                        stack.(trapsp) <- arg1;
                        extern_sp := trapsp;
                        let v = magic (interp code goto (repr 0) env recenv) in
                        extern_sp := trapsp + 4;
                        v
                        )
                | _ ->
                    let args = Array.create (required+1) (repr 0) in
                    let rec f n arg =
                      args.(n) <- arg;
                      if n = required then
  let stack = (magic global_stack : t array) in
                        let sp = extern_sp in
                        let trapsp = !sp in
                        sp := !sp - required - 1;
                        for i = required downto 0 do
                          stack.(!sp + i) <- args.(i)
                        done;                  
                        let v = magic (interp code goto (repr 0) env recenv) in
                        sp := trapsp;
                        v
                      else
                        (magic f : int -> t -> t) (n+1)
                    in
                    repr (f 0)
              else
                repr (fun arg ->
                    let sp = extern_sp in
  let stack = (magic global_stack : t array) in
                    let trapsp = !sp in              
                    decr sp;stack.(!sp) <- arg;
                    let v = interp code goto (repr 0) env recenv in
                    sp := trapsp;
              v)
            in
            stack.(!sp - i - 1) <- repr fi;
            clos.(i*2) <- repr fi
          done;
          pc := !pc + nfuncs;
          sp := !sp - nfuncs
      
      | OpOFFSETCLOSUREM2  ->
          let (pos,env) = recenv in
          accu := env.(pos-2)
      | OpOFFSETCLOSURE0  ->
          let (pos,env) = recenv in
          accu := env.(pos)
      | OpOFFSETCLOSURE2  ->
          let (pos,env) = recenv in
          accu := env.(pos+2)
      | OpOFFSETCLOSURE  ->
          incr pc; let offset = magic code.(!pc) in
          let (pos,env) = recenv in
          accu := env.(pos+offset)
      | OpPUSHOFFSETCLOSUREM2  ->
          decr sp; stack.(!sp) <- !accu;
          let (pos,env) = recenv in
          accu := env.(pos-2)
      | OpPUSHOFFSETCLOSURE0  ->
          decr sp; stack.(!sp) <- !accu;
          let (pos,env) = recenv in
          accu := env.(pos)
      | OpPUSHOFFSETCLOSURE2  ->
          decr sp; stack.(!sp) <- !accu;
          let (pos,env) = recenv in
          accu := env.(pos+2)
      | OpPUSHOFFSETCLOSURE  ->
          decr sp; stack.(!sp) <- !accu;
          incr pc; let offset = magic code.(!pc) in
          let (pos,env) = recenv in
          accu := env.(pos+offset)
      | OpGETGLOBAL  ->
          
          incr pc; let offset = unsigned code.(!pc) in
          accu := getglobal offset;
      
      | OpPUSHGETGLOBAL  ->
          decr sp; stack.(!sp) <- !accu;
          incr pc; let offset = unsigned code.(!pc) in
          accu := getglobal offset
      | OpGETGLOBALFIELD  ->
          let offset = unsigned code.(!pc+1) in
          pc := !pc+2; let field = unsigned code.(!pc) in
          accu := (magic (getglobal offset): t array).(field)
      | OpPUSHGETGLOBALFIELD  ->
          
          decr sp; stack.(!sp) <- !accu;
          let offset = unsigned code.(!pc+1) in
          pc:=!pc+2;
          let pos = unsigned code.(!pc) in
          let modul = (magic (getglobal offset): t array) in
          accu := modul.(pos);
      | OpSETGLOBAL  -> 
          incr pc; let offset = unsigned code.(!pc) in
          setglobal offset !accu;
          accu := (repr 0);
      | OpATOM0  -> 
          accu := repr ([||])
      | OpATOM  -> 
          incr pc; let atom = unsigned code.(!pc) in
          accu := repr (MyObj.new_block atom 0)
      | OpPUSHATOM0  -> 
          decr sp; stack.(!sp) <- !accu;
          accu := repr ([||])
      | OpPUSHATOM  ->
          decr sp; stack.(!sp) <- !accu;
          incr pc; let atom = unsigned code.(!pc) in
          accu := repr (MyObj.new_block atom 0)
      | OpMAKEBLOCK  ->
          pc := !pc + 2;
          let wosize = unsigned code.(!pc-1) in
          let tag = unsigned code.(!pc) in
          accu := makeblock tag wosize !accu stack !sp;
          sp := !sp + wosize - 1;
(*          
          let tab = MyObj.magic (MyObj.new_block tag wosize) in
          for i = 0 to wosize - 2 do
            tab.(i+1) <- stack.(!sp); incr sp
          done;
          tab.(0) <- !accu;
          accu := repr tab;
*)
      | OpMAKEBLOCK1  ->
          incr pc; let tag = unsigned code.(!pc) in
          if tag =0 then accu := repr (ref !accu)
          else accu := makeblock1 tag !accu
(*
            let tab = new_block tag 1 in
          (magic tab: t array).(0) <- !accu;
            accu := tab
          *)
      | OpMAKEBLOCK2  ->
          incr pc; let tag = unsigned code.(!pc) in
          incr sp;
          if tag = 0 then accu := repr (!accu, stack.(!sp-1)) else
            accu := makeblock2 tag !accu stack.(!sp-1)          
(*            
          let tab = new_block tag 2 in
          (magic tab: t array).(0) <- !accu;
          (magic tab: t array).(1) <-  stack.(!sp-1); 
            accu := tab;
*)
      | OpMAKEBLOCK3  ->
          incr pc; let tag = unsigned code.(!pc) in
          sp := !sp + 2;
          if tag = 0 then 
            accu := repr (!accu, 
              stack.(!sp-2),
              stack.(!sp-1)) else
            accu := makeblock3 tag !accu stack.(!sp-2) stack.(!sp-1)
(*            
          let tab = new_block tag 3 in
          (magic tab: t array).(0) <- !accu;
          (magic tab: t array).(1) <-  stack.(!sp-2);
          (magic tab: t array).(2) <-  stack.(!sp-1); 
            accu := tab;
  *)
      | OpMAKEFLOATBLOCK  -> 
          incr pc; let wosize = unsigned code.(!pc) in
          let tab = Array.create wosize 0.0 in
          for i = 0 to wosize - 2 do
            tab.(i+1) <- magic stack.(!sp + i);
          done;
          tab.(0) <- ((magic !accu) : float);
          accu := repr tab;
          sp := !sp + wosize - 1
      
      | OpGETFIELD0  -> accu := (magic !accu).(0)
      | OpGETFIELD1  -> accu := (magic !accu).(1)
      | OpGETFIELD2  -> accu := (magic !accu).(2)
      | OpGETFIELD3  -> accu := (magic !accu).(3)
      | OpGETFIELD  -> 
          incr pc;
          accu := (magic !accu).(code.(!pc))
      | OpGETFLOATFIELD  -> 
          incr pc; let pos = unsigned code.(!pc) in
          let tab = (magic !accu : float array) in
          accu := repr (tab.(pos))
      | OpSETFIELD0  -> 
          (MyObj.magic !accu : t array).(0) <- stack.(!sp);
          incr sp;
      | OpSETFIELD1  ->
          (MyObj.magic !accu : t array).(1) <- stack.(!sp);
          incr sp;
      | OpSETFIELD2  ->
          (MyObj.magic !accu : t array).(2) <- stack.(!sp);
          incr sp;
      | OpSETFIELD3  ->
          (MyObj.magic !accu : t array).(3) <- stack.(!sp);
          incr sp;
      | OpSETFIELD ->
          incr pc;
          (magic !accu : t array).(code.(!pc)) <- stack.(!sp);
          incr sp;
      | OpSETFLOATFIELD  ->
          let arg = stack.(!sp) in incr sp;
          incr pc; let pos = unsigned code.(!pc) in
          let tab = (magic !accu : float array) in
          tab.(pos) <- (magic arg : float);
      | OpVECTLENGTH  ->
(* dangerous if float array *)
          accu := repr (Array.length (MyObj.magic !accu))
      | OpGETVECTITEM  ->
          accu := (magic !accu).(magic stack.(!sp)); incr sp
      | OpSETVECTITEM  ->
          (magic !accu).(magic stack.(!sp)) <- stack.(!sp + 1); sp := !sp + 2
      | OpGETSTRINGCHAR  ->
          accu := repr (magic !accu).[magic stack.(!sp)]; incr sp
      | OpSETSTRINGCHAR  ->
          (magic !accu).[magic stack.(!sp)] <- 
            Char.chr ((magic stack.(!sp + 1)) land 0xff); 
          sp := !sp + 2
      | OpBRANCH  ->
          let offset = magic code.(!pc+1) in
          pc := !pc + offset;
      | OpBRANCHIF  ->
          let offset = magic code.(!pc + 1) in
          if (magic !accu) <> 0 then pc := !pc + offset else incr pc
      | OpBRANCHIFNOT  ->
          let offset = magic code.(!pc + 1) in
          if (magic !accu) = 0 then pc := !pc + offset else incr pc
      | OpSWITCH  -> (* inputu + inputs *)
          incr pc; let sizes = unsigned code.(!pc) in
          let obj = repr !accu in
          (* Avec la garantie de Jacques et Xavier (garder un oeil sur
          matching.ml) *)
          if magic obj land 0 == 0 then
            begin
              let index = magic !accu in
              
              if index >= 0 && index  < (sizes land 0xffff) then
                pc := !pc + unsigned code.(!pc + 1 + index)
              else
                pc := !pc + (sizes land 0xffff) + (sizes lsr 16);
            
            end
          else
            begin
              
              pc := !pc + unsigned code.(!pc + 1 + (sizes land 0xffff) + 
                (blocktag (magic obj)));
            
            end
      | OpBOOLNOT  -> accu := repr (1 - (magic !accu))
      | OpPUSHTRAP -> 
          incr pc; 
          let trap_link = !pc - 1 + magic code.(!pc) in
          let trapsp = !sp in
          
          let goto = !pc in
          sp := !sp - 4;
          begin
            try
              extern_sp := !sp;
              accu := interp code (!pc+1) !accu env recenv;
            with
              PopTrap -> 
                pc := !poppc; 
                accu := !popacc; 
                sp := trapsp
            | e ->
                sp := trapsp;
                accu := repr e;
                pc := trap_link
          end
      | OpPOPTRAP ->
          poppc := !pc;
          popacc := !accu;
          raise poptrap
      | OpRAISE ->
          raise (MyObj.magic !accu);
      | OpCHECK_SIGNALS  -> ()
      | OpC_CALL1  -> 
          incr pc; let prim = unsigned code.(!pc) in
          extern_sp := !sp;
          
          accu := (MyObj.magic caml_prims.(prim)) !accu
      | OpC_CALL2  ->
          let arg = stack.(!sp) in
          incr sp;
          incr pc; let prim = unsigned code.(!pc) in
          extern_sp := !sp;
          
          accu := (MyObj.magic caml_prims.(prim)) !accu arg
      | OpC_CALL3  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp+1) in
          sp := !sp + 2;
          incr pc; let prim = unsigned code.(!pc) in
          extern_sp := !sp;
          accu := (MyObj.magic caml_prims.(prim)) !accu arg1 arg2;
      
      | OpC_CALL4  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp+1) in
          let arg3 = stack.(!sp+2) in
          sp := !sp + 3;
          incr pc; let prim = unsigned code.(!pc) in
          extern_sp := !sp;
          accu := (MyObj.magic caml_prims.(prim)) !accu arg1 arg2 arg3
      | OpC_CALL5  ->
          let arg1 = stack.(!sp) in
          let arg2 = stack.(!sp+1) in
          let arg3 = stack.(!sp+2) in
          let arg4 = stack.(!sp+3) in
          sp := !sp + 4;
          incr pc; let prim = unsigned code.(!pc) in
          extern_sp := !sp;
          accu := (MyObj.magic caml_prims.(prim)) !accu arg1 arg2 arg3 arg4
      | OpC_CALLN  ->
          incr pc; let nargs = unsigned code.(!pc) in
          decr sp; stack.(!sp) <- !accu;
          let nsp = !sp in
          let args = Array.init nargs (fun i -> stack.(nsp + i)) in
          sp := !sp + nargs;
          incr pc; let prim = unsigned code.(!pc) in
          extern_sp := !sp;
          accu := (MyObj.magic caml_prims.(prim)) args  nargs
      | OpCONST0  -> accu := repr 0
      | OpCONST1  -> accu := repr 1
      | OpCONST2  -> accu := repr 2
      | OpCONST3  -> accu := repr 3
      | OpCONSTINT  ->
          incr pc; accu := repr code.(!pc)
      | OpPUSHCONST0  ->
          decr sp; stack.(!sp) <- !accu; accu := repr 0
      | OpPUSHCONST1  ->
          decr sp; stack.(!sp) <- !accu; accu := repr 1
      | OpPUSHCONST2  ->
          decr sp; stack.(!sp) <- !accu; accu := repr 2
      | OpPUSHCONST3  ->
          decr sp; stack.(!sp) <- !accu; accu := repr 3
      | OpPUSHCONSTINT  ->
          decr sp; stack.(!sp) <- !accu;
          incr pc; 
          accu := repr code.(!pc)
      | OpNEGINT  -> accu := repr (- (magic !accu))
      | OpADDINT  -> 
          accu := repr ((magic !accu) + magic stack.(!sp));
          incr sp;
      | OpSUBINT  ->
          accu := repr ((magic !accu) - magic stack.(!sp));
          incr sp;
      | OpMULINT  ->
          let arg = magic stack.(!sp) in incr sp;
          accu := repr ((magic !accu) * arg)
      | OpDIVINT  ->
          let arg = magic stack.(!sp) in incr sp;
          if arg = 0 then raise Division_by_zero;
          accu := repr ((magic !accu) / arg)
      | OpMODINT  ->
          let arg = magic stack.(!sp) in incr sp;
          if arg = 0 then raise Division_by_zero;
          accu := repr ((magic !accu) mod arg)
      | OpANDINT  ->
          accu := repr ((magic !accu) land magic stack.(!sp)); 
          incr sp;
      | OpORINT  ->
          accu := repr ((magic !accu) lor magic stack.(!sp));
          incr sp
      | OpXORINT  ->
          accu := repr ((magic !accu) lxor magic stack.(!sp)); 
          incr sp
      | OpLSLINT  ->
          accu := repr ((magic !accu) lsl magic stack.(!sp));
          incr sp
      | OpLSRINT  ->
          accu := repr ((magic !accu) lsr magic stack.(!sp));
          incr sp;
      | OpASRINT  ->
          accu := repr ((magic !accu) asr magic stack.(!sp));
          incr sp;
      | OpEQ  ->
          accu := repr ((magic !accu : int) == magic stack.(!sp));
          incr sp;
      | OpNEQ  ->
          accu := repr (not ((magic !accu : int) == magic stack.(!sp)));
          incr sp;
      | OpLTINT  ->
          accu := repr ((magic !accu : int) < magic stack.(!sp));
          incr sp;
      | OpLEINT  ->
          accu := repr ((magic !accu : int) <= magic stack.(!sp));
          incr sp;
      | OpGTINT  ->
          accu := repr ((magic !accu : int) > magic stack.(!sp)); 
          incr sp;
      | OpGEINT  ->
          accu := repr ((magic !accu : int) >= magic stack.(!sp));
          incr sp;
      | OpOFFSETINT  ->
          incr pc; let offset = magic code.(!pc) in
          accu := repr ((magic !accu : int) + offset)
      | OpOFFSETREF  -> 
          incr pc; let offset = magic code.(!pc) in
          (MyObj.magic (magic !accu)).(0) <- (MyObj.magic (magic !accu)).(0) + offset;
          accu := repr 0;
      | OpGETMETHOD  ->
          let label = 2 * unsigned !accu in
          accu := (magic stack.(!sp): t array array array).(0).
              ((label lsr 16) / value_size).
            ((label / value_size) land 0xff)
      | OpSTOP  -> raise exit
      | OpEVENT  -> 
          assert false; (* Not implemented *)          
      | OpBREAK  ->
          assert false; (* Not implemented *)          
    done;
    (repr 0)
  with
    ExitInterp ->       
      extern_sp := !sp;
      !accu
      


module Meta = struct
    
    open MyObj
    open Instructs
    
    let unit = repr 0
    
    let inputu str i =
      let pos = i * 4 in
      let b1 = Char.code str.[pos] in
      let b2 = Char.code str.[pos+1] in
      let b3 = Char.code str.[pos+2] in
      let b4 = Char.code str.[pos+3] in
      (b4 lsl 24) + (b3 lsl 16) + (b2 lsl 8) + b1
    
    let inputs str i =
      let pos = i * 4 in
      let b1 = Char.code str.[pos] in
      let b2 = Char.code str.[pos+1] in
      let b3 = Char.code str.[pos+2] in
      let b4 = Char.code str.[pos+3] in
      let b4' = if b4 >= 128 then b4-256 else b4 in
      (b4' lsl 24) + (b3 lsl 16) + (b2 lsl 8) + b1
    
    let d = 0x3fffffff
    let signed v =  (* Only for 64 bits architectures *)
      let sign = if v > d then v - (1 lsl 32) else v in
      sign
    
    let fix_code str str_size =
      let code_size = str_size / 4 in
      let code = Array.create code_size 0 in
      let rec fix i =
        
        if i < code_size then 
          let op = inputu str i in
          code.(i) <- op;
          match magic op with
(* one arg *)
          | OpPUSHACC | OpACC | OpPOP | OpASSIGN | OpPUSHENVACC | OpENVACC
          | OpAPPLY | OpAPPTERM1 | OpAPPTERM2 | OpAPPTERM3 | OpRETURN | OpGRAB
          | OpPUSHGETGLOBAL | OpGETGLOBAL | OpSETGLOBAL | OpPUSHATOM | OpATOM
          | OpMAKEBLOCK1 | OpMAKEBLOCK2 | OpMAKEBLOCK3 | OpMAKEFLOATBLOCK
          | OpGETFIELD
          | OpGETFLOATFIELD | OpSETFIELD | OpSETFLOATFIELD | OpC_CALL1 | OpC_CALL2 | OpC_CALL3
          | OpC_CALL4
          | OpC_CALL5
            -> 
              let arg = inputu str (i+1) in
              code.(i+1) <- arg;
              fix (i+2)

(* signed arg *)
          | OpPUSH_RETADDR | OpOFFSETCLOSURE | OpPUSHOFFSETCLOSURE | OpBRANCH
          | OpBRANCHIF | OpPUSHTRAP | OpCONSTINT | OpPUSHCONSTINT
          | OpBRANCHIFNOT | OpOFFSETINT | OpOFFSETREF
            ->
              let arg = inputu str (i+1) in
              code.(i+1) <- (signed arg);
              fix (i+2)



(* two args *) 
          | OpAPPTERM | OpPUSHGETGLOBALFIELD | OpGETGLOBALFIELD
          | OpMAKEBLOCK
          | OpC_CALLN
            -> 
              let arg1 = inputu str (i+1) in              
              let arg2 = inputu str (i+2) in
              code.(i+1) <- arg1;
              code.(i+2) <- arg2;
              fix (i+3)

(* two args, second signed *)
          | OpCLOSURE
            -> 
              let arg1 = inputu str (i+1) in              
              let arg2 = inputu str (i+2) in
              code.(i+1) <- arg1;
              code.(i+2) <- (signed arg2);
              fix (i+3)
          
          | OpSWITCH ->
              let sizes = inputu str (i+1) in
              code.(i+1) <- sizes;
              let const_size = sizes land 0xFFFF in
              let block_size = sizes lsr 16 in
              let offset = 2 + const_size + block_size in
              for j = 2 to offset - 1 do
                code.(i + j) <- (inputu str (i + j))
              done;
              fix (i + offset)
          
          |  OpCLOSUREREC ->
              let nfuncs = inputu str (i+1) in
              let nvars = inputu str (i+2) in
              code.(i+1) <- nfuncs;
              code.(i+2) <- nvars;
              let offset = 3 + nfuncs in
              for j = 3 to offset - 1 do
                code.(i + j) <- (signed (inputu str (i + j)))
              done;
              fix (i + offset)
             

(* no args *) 
          | _ -> 
              fix (i+1) 
      
      in 
      fix 0;
      code
    
    let reify_bytecode buff code_size name =
      let new_buff = String.create code_size in
      String.unsafe_blit buff 0 new_buff 0 code_size;
      let code = fix_code new_buff code_size in
      let pc = 0 in
      let accu = repr 0 in
      let env = [||] in
      let v = interp code pc accu env (0,env) in
      v
  end

(* Primitives. *)

open Obj

let insert_one name (f : Obj.t) = Hashtbl.add prims name f


let _ =
  List.iter (fun (name, func) ->
      insert_one name func) Compat_run.caml_primitives

let insert_all () =
  for i = 0 to Array.length Runtimedef.builtin_primitives - 1 do
    let name = Runtimedef.builtin_primitives.(i) in
    try
      let f = Hashtbl.find prims name in
      caml_prims.(i) <- (MyObj.magic f);
    with
      Not_found -> failwith 
        (Printf.sprintf "No definition for primitive %s" name)
  done
    
  
external set_meta:
   MyObj.t array * (string -> int -> 'a -> MyObj.t) * MyObj.t array
   -> unit = "set_meta"      

let _ = 
  insert_all ();
  set_meta (caml_globals,Meta.reify_bytecode,caml_prims)
  
  
  
