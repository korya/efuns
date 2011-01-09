type token =
    EOF
  | EOL
  | SEP
  | IDENT of (string)
  | TILDE

val parse_passwd :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string list list
val parse_filename :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> token list
