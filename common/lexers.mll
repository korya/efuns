{
  open Lexing
  open Parsers
} 

rule lexer_passwd = parse
  eof   { EOF }
| '\n'  { EOL }
| ':'   { SEP }
| [^ '\n' ':'] *  { IDENT (lexeme lexbuf) }

and lexer_filename = parse
  eof   { EOF }
| '~'   { TILDE }
| '/'   { SEP }
| [^ '~' '/'] + { IDENT (lexeme lexbuf) }

and lexer_glob = parse
    "\*"  { "\*" }
  | "\?"  { "\?" }
  | "."   { "\." }
  | "*"   { ".*" }
  | "?"   { "." }
  | "{"   { "\\(" }
  | "}"   { "\\)" }
  | ","   { "\\|" }
  | eof   { "" }
  | _     { lexeme lexbuf }
    
{

} 
