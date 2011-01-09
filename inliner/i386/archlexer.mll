{
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
  
open Lexing  
open Archparser
open Misc
open Asm

let char_for_decimal_code3 lexbuf i =
  let c = 64 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
      8 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
      (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in  
  Char.chr(c land 0xFF)

let char_for_decimal_code2 lexbuf i =
  let c = 8 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
      (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) in  
  Char.chr(c land 0xFF)

let char_for_decimal_code1 lexbuf i =
  let c = Char.code(Lexing.lexeme_char lexbuf i) - 48 in
  Char.chr(c land 0xFF)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0


let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
      let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
    end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let instrs = Hashtbl.create 53
let _ =
  List.iter (fun (s,k) -> Hashtbl.add instrs s k) [
  (*** start with prefixed instrs ******)
    "cmpb", INSTR2(Cmpi ON_BYTE);
    "cmpw", INSTR2(Cmpi ON_WORD);
    "cmpl", INSTR2(Cmpi ON_LONG);
    "andb", INSTR2(Andi ON_BYTE);
    "andw", INSTR2(Andi ON_WORD);
    "andl", INSTR2(Andi ON_LONG);
    "xorb", INSTR2(Xori ON_BYTE);
    "xorw", INSTR2(Xori ON_WORD);
    "xorl", INSTR2(Xori ON_LONG);
    "decb", INSTR2(Deci ON_BYTE);
    "decw", INSTR2(Deci ON_WORD);
    "decl", INSTR2(Deci ON_LONG);
    
    "fcompp", INSTR0(Fcompp);
    "movl",  INSTR2(Movl) ;
    "addl",  INSTR2(Addl) ;
    "subl",  INSTR2(Subl) ;
    "imull",  INSTR2(Imull) ;
    "andl",  INSTR2(Andl) ;
    "orl",  INSTR2(Orl) ;
    "xorl",  INSTR2(Xorl) ;
    "sall",  INSTR2(Sall) ;
    "shrl",  INSTR2(Shrl) ;
    "sarl",  INSTR2(Sarl) ;
    "fstpl",  INSTR1(Fstpl) ;
    "fldl",  FLDL ;
    "fchs",  INSTR0(Fchs) ;
    "fabs",  INSTR0(Fabs) ;
    "faddl",  INSTR1(Faddl) ;
    "fsubl",  INSTR1(Fsubl) ;
    "fmull",  INSTR1(Fmull) ;
    "fdivl",  INSTR1(Fdivl) ;
    "fsubrl",  INSTR1(Fsubrl) ;
    "fdivrl",  INSTR1(Fdivrl) ;
    "faddp",  INSTR2(Faddp) ;
    "fsubp",  INSTR2(Fsubp) ;
    "fmulp",  INSTR2(Fmulp) ;
    "fdivp",  INSTR2(Fdivp) ;
    "fsubrp",  INSTR2(Fsubrp) ;
    "fdivrp",  INSTR2(Fdivrp) ;
    "fldz",  INSTR0(Fldz) ;
    "fld1",  INSTR0(Fld1) ;
    "testl",  INSTR2(Testl) ;
    "cmpl",  INSTR2(Cmpl) ;
    "call",  CALL ;
    "jmp",  JMP ;
    "movzbl",  INSTR2(Movzbl) ;
    "movsbl",  INSTR2(Movsbl) ;
    "movzwl",  INSTR2(Movzwl) ;
    "movswl",  INSTR2(Movswl) ;
    "movb",  INSTR2(Movb) ;
    "movw",  INSTR2(Movw) ;
    "cltd",  INSTR0(Cltd) ;
    "idivl",  INSTR1(Idivl) ;
    "incl",  INSTR1(Incl) ;
    "decl",  INSTR1(Decl) ;
    "fildl",  INSTR1(Fildl) ;
    "fcompl", INSTR1(Fcompl);
    "pushl",  INSTR1(Pushl) ;
    "fnstsw", INSTR1(Fnstsw);
    "fnstcw",  INSTR1(Fnstcw) ;
    "fldcw",  INSTR1(Fldcw) ;
    "fistpl",  INSTR1(Fistpl) ;
    "lea",  INSTR2(Lea) ;
    "leal",  INSTR2(Leal) ;
    "ret",  RET ;
    "popl",  INSTR1(Popl) ;      
    "bswap", INSTR1(Bswap);
    "stc", INSTR0(Stc);
    "rcrl", INSTR2(Rcrl);
    "rcll", INSTR2(Rcll);
    "sete", SETCOND(Isigned Ceq);
    "setne", SETCOND(Isigned Cne);
    "setle", SETCOND(Isigned Cle);
    "setg", SETCOND(Isigned Cgt);
    "setl", SETCOND(Isigned Clt);    
    "setge", SETCOND(Isigned Cge);
    "sete", SETCOND(Iunsigned Ceq);
    "setne", SETCOND(Iunsigned Cne);
    "setbe", SETCOND(Iunsigned Cle);  
    "seta", SETCOND(Iunsigned Cgt);
    "setb", SETCOND(Iunsigned Clt);  
    "setae", SETCOND(Iunsigned Cge);      
    "je", JCOND(Isigned Ceq);
    "jne", JCOND(Isigned Cne);
    "jle", JCOND(Isigned Cle);
    "jg", JCOND(Isigned Cgt);
    "jl", JCOND(Isigned Clt);    
    "jge", JCOND(Isigned Cge);
    "je", JCOND(Iunsigned Ceq);
    "jne", JCOND(Iunsigned Cne);
    "jbe", JCOND(Iunsigned Cle);  
    "ja", JCOND(Iunsigned Cgt);
    "jb", JCOND(Iunsigned Clt);  
    "jae", JCOND(Iunsigned Cge);
    "DEAD", DEAD;
    "DISTINCT", DISTINCT;
    "EQUAL", EQUAL;
    "SAMEREG", SAMEREG;
    "SAMEVALUE", SAMEVALUE;
    "INSTR2", BININSTR;
    "define", MACRO_DEFINE;
    "inline", MACRO_INLINE;
    "peephole", MACRO_PEEPHOLE;
  ]

let keyword s =
  try
    Hashtbl.find instrs s
  with Not_found -> 
          IDENT s
}

let blank = [' ' '\009']
let return = ['\010' '\012' '\013']
let firstidentchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246'
      '\248'-'\255']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '$'
      '\248'-'\255' '0'-'9']
let decimal_literal = ['0'-'9']+
  let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
    let oct_literal = '0' ['o' 'O'] ['0'-'7']+
      let bin_literal = '0' ['b' 'B'] ['0'-'1']+
        let float_literal =
          ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

          
rule lexfile = parse
    '#'      { comment lexbuf }
  | blank    { lexfile lexbuf }
  | return   { EOL }
  | ".data"  { DATA }
  | ".globl" { GLOBAL }
  | ".text"  { TEXT }
  | ".long"  { LONG }
  | ".ascii" { ASCII }
  | ".string" { STR }
  | ".word"  { WORD }
  | ".byte"  { BYTE }
  | ".space" { SPACE }
  | ".skip" { SKIP }
  | ".align" { ALIGN }
  | ".type"  { TYPE }
  | ".size"  { SIZE }
  | "@object" { OBJECT }
  | "@function" { FUNCTION }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | ','      { COMMA }
  | "LOWBYTE" ' '* '%'  { REGISTER (LowByte (register lexbuf)) }
  | "LOWWORD" ' '* '%'  { REGISTER (LowWord (register lexbuf)) }
  | "HIGHBYTE" ' '* '%'  { REGISTER (HighByte (register lexbuf)) }
  | "%n" ['0'-'9']? ['0'-'9' 'a'-'z']* { 
      CONSTANT(Match.mkconst (lexeme lexbuf)) }
  | "%i" ['0'-'9']? ['0'-'9' 'a'-'z']* { 
      INSTRUCTION(Match.mkstate (lexeme lexbuf)) }
  | "%bin"['0'-'9']? ['0'-'9' 'a'-'z']* { 
      INSTR2(Match.mkopcode (lexeme lexbuf)) }
  | "%v" ['0'-'9']? ['0'-'9' 'a'-'z']* { 
      ARGUMENT(Match.mkarg (lexeme lexbuf)) }
  | '%'      { REGISTER(register lexbuf) }
  | '$'      { IMMEDIAT }
  | '-'      { MINUS }
  | '+'      { PLUS }
  | '*'      { STAR }
  | ".double" { DOUBLE }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | decimal_literal { INT (int_of_string (lexeme lexbuf)) }
  | hex_literal { INT (int_of_string (lexeme lexbuf)) }
  | oct_literal { INT (int_of_string (lexeme lexbuf)) }
  | bin_literal { INT (int_of_string (lexeme lexbuf)) }
  | float_literal { FLOAT (float_of_string (lexeme lexbuf)) }    
  | firstidentchar identchar*  { keyword (lexeme lexbuf) }
  | ".L" identchar+ { LABEL (lexeme lexbuf) }
  | "\""
      { reset_string_buffer();
      let string_start = Lexing.lexeme_start lexbuf in
      try
        string lexbuf;
        STRING (get_stored_string ())
      with
        Not_found -> failwith (
            Printf.sprintf "Unterminated string at %d" string_start)
    }
  | ':' { COLON }
  | '.' { DOT }
  | eof { EOF }
  | _  { Printf.printf "in Lexer.lexfile"; print_newline (); error lexbuf }
    
and comment = parse
    blank { comment lexbuf }
  | return { EOL }
  | "$asmopt$:" { directive lexbuf }
  | _ { end_comment lexbuf }
    
and end_comment = parse
  | [^ '\010' '\012' '\013']* return { lexfile lexbuf }
  | _ { Printf.printf "in Lexer.end_comment"; print_newline (); error lexbuf }
    
and string = parse
    '"'
    { () }
| '\\' ['0'-'7'] ['0'-'7'] ['0'-'7']
    { store_string_char(char_for_decimal_code3 lexbuf 1);
    string lexbuf }
| '\\' ['0'-'7'] ['0'-'7']
    { store_string_char(char_for_decimal_code2 lexbuf 1);
    string lexbuf }
| '\\' ['0'-'7']
    { store_string_char(char_for_decimal_code1 lexbuf 1);
    string lexbuf }
| eof    { raise Not_found }
| _
    { store_string_char(Lexing.lexeme_char lexbuf 0);
    string lexbuf }

and immediat = parse
  | decimal_literal { IMM_INT(int_of_string (lexeme lexbuf)) }
  | hex_literal     { IMM_INT(int_of_string (lexeme lexbuf)) }
  | oct_literal     { IMM_INT(int_of_string (lexeme lexbuf)) }
  | bin_literal     { IMM_INT(int_of_string (lexeme lexbuf)) }
  | '-' decimal_literal { IMM_INT(int_of_string (lexeme lexbuf)) }
  | '-' hex_literal     { IMM_INT(int_of_string (lexeme lexbuf)) }
  | '-' oct_literal     { IMM_INT(int_of_string (lexeme lexbuf)) }
  | '-' bin_literal     { IMM_INT(int_of_string (lexeme lexbuf)) }
  | firstidentchar identchar*  { IMM_SYMBOL(lexeme lexbuf) }
  | _ { Printf.printf "In Lexer.immediat"; print_newline (); error lexbuf }
    
and error = parse 
    
and register =  parse
    "eax"     { Eax }
  | "ebx"     { Ebx }
  | "ecx"     { Ecx }
  | "edx"     { Edx }
  | "esi"     { Esi }
  | "edi"     { Edi }
  | "ebp"     { Ebp }
  | "esp"     { Esp }
  | "st"      { St 0 }
  | "tos"     { St 0 }
  | "al"        { LowByte Eax }
  | "bl" { LowByte Ebx }
  | "cl" { LowByte Ecx }
  | "dl" { LowByte Edx }
  | "ah" { HighByte Eax }
  | "bh" { HighByte Ebx }
  | "ch" { HighByte Ecx }
  | "dh" { HighByte Edx }
  | "ax" { LowWord Eax }
  | "bx" { LowWord Ebx }
  | "cx" { LowWord Ecx }
  | "dx" { LowWord Edx }
  | "si" { LowWord Esi }
  | "di" { LowWord Edi }
  | "bp" { LowWord Ebp }
  | 'r'  ['0'-'9']? ['0'-'9' 'a'-'z']* { Match.mkreg (lexeme lexbuf)  }
  | _ { Printf.printf "In Lexer.register"; print_newline (); 
      ignore (error lexbuf); raise Not_found }
    
and directive = parse
    blank { directive lexbuf }
  | "res:"  { DIR_RES }
  | "args:" { DIR_ARGS }
  | "saved:" { DIR_SAVED }
  | "live:"  { DIR_LIVE }
  | "begincode:" { DIR_BEGINCODE }
  | "endcode:" { DIR_ENDCODE }
  | "begindata:" { DIR_BEGINDATA }
  | "enddata:" { DIR_ENDDATA }
  | "switch:"  { DIR_SWITCH }
  | "entry:"   { DIR_ENTRY }
  | "exit:"    { DIR_EXIT }
  | "tailcall:" { DIR_TAILCALL }
  | "continue:" { DIR_CONTINUE }
  | "record:" { DIR_RECORD }
  | "nogc:" { DIR_NOGC }
  | "frametable:" { DIR_FRAMETABLE }
  | "setuptrap:"  { DIR_SETUPTRAP }
  | "pushtrap:" {DIR_PUSHTRAP }
  | "poptrap:" { DIR_POPTRAP }
  | "raise:"   { DIR_RAISE }
  | "frame:"   { DIR_FRAME }
  | "arity:"   { DIR_ARITY }
  | "alloc:"   { DIR_ALLOC }
  | _ { Printf.printf "in Lexer.directive"; print_newline (); error lexbuf }

    
    