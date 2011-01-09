(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: testscanner.mll,v 1.1 1999/11/22 10:36:02 lefessan Exp $ *)

(* The lexical analyzer for lexer definitions. *)

{
#open "syntax";;
#open "grammar";;
#open "scan_aux";;
}

rule main = parse
    _ * "qwertyuiopasdfghjklzxcvbnm0123456789!@#$%^&*()"
    { main lexbuf }
  | [' ' '\010' '\013' '\009' ] + 
    { main lexbuf }
  | "(*" 
    { comment_depth := 1;
      comment lexbuf;
      main lexbuf }
  | (['A'-'Z' 'a'-'z'] | '_' ['A'-'Z' 'a'-'z' '\'' '0'-'9'])
    ( '_' ? ['A'-'Z' 'a'-'z' ''' '0'-'9'] ) * 
    { match lexing.lexeme lexbuf with
        "rule" -> Trule
      | "parse" -> Tparse
      | "and" -> Tand
      | "eof" -> Teof
      | s -> Tident s }
  | '"' 
    { reset_string_buffer();
      string lexbuf;
      Tstring(get_stored_string()) }
  | "'" 
    { Tchar(char lexbuf) }
  | '{' 
    { let n1 = lexing.lexeme_end lexbuf in
        brace_depth := 1;
        let n2 = action lexbuf in
          Taction(Location(n1, n2)) }
  | '='  { Tequal }
  | ";;"  { Tend }
  | '|'  { Tor }
  | '_'  { Tunderscore }
  | "eof"  { Teof }
  | '['  { Tlbracket }
  | ']'  { Trbracket }
  | '*'  { Tstar }
  | '?'  { Tmaybe }
  | '+'  { Tplus }
  | '('  { Tlparen }
  | ')'  { Trparen }
  | '^'  { Tcaret }
  | '-'  { Tdash }
  | eof
    { raise(Lexical_error "unterminated lexer definition") }
  | _
    { raise(Lexical_error("illegal character " ^ lexing.lexeme lexbuf)) }

and action = parse
    '{' 
    { brace_depth := brace_depth + 1;
      action lexbuf }
  | '}' 
    { brace_depth := brace_depth - 1;
      if brace_depth = 0 then lexing.lexeme_start lexbuf else action lexbuf }
  | '"' 
    { reset_string_buffer();
      string lexbuf;
      reset_string_buffer();
      action lexbuf }
  | '\''
    { char lexbuf; action lexbuf }
  | "(*" 
    { comment_depth := 1;
      comment lexbuf;
      action lexbuf }
  | eof 
    { raise (Lexical_error "unterminated action") }
  | _ 
    { action lexbuf }
      
and string = parse
    '"' 
    { () }
  | '\\' [' ' '\010' '\013' '\009' '\026' '\012'] +
    { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r'] 
    { store_string_char(char_for_backslash(lexing.lexeme_char lexbuf 1));
      string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] 
    { store_string_char(char_for_decimal_code lexbuf 1);
      string lexbuf }
  | eof 
    { raise(Lexical_error "unterminated string") }
  | _ 
    { store_string_char(lexing.lexeme_char lexbuf 0);
      string lexbuf }

and char = parse
    [^ '\\'] "'" 
    { lexing.lexeme_char lexbuf 0 }
  | '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'" 
    { char_for_backslash (lexing.lexeme_char lexbuf 1) }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'" 
    { char_for_decimal_code lexbuf 1 }
  | _ 
    { raise(Lexical_error "bad character constant") }

and comment = parse
    "(*" 
    { comment_depth := comment_depth + 1; comment lexbuf }
  | "*)" 
    { comment_depth := comment_depth - 1;
      if comment_depth = 0 then () else comment lexbuf }
  | '"' 
    { reset_string_buffer();
      string lexbuf;
      reset_string_buffer();
      comment lexbuf }
  | eof 
    { raise(Lexical_error "unterminated comment") }
  | _ 
    { comment lexbuf }
;;
