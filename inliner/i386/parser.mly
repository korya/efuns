/***********************************************************************/
  /*                                                                     */
  /*                           AsmOpt                                    */
  /*                                                                     */
  /*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       */
  /*                                                                     */
  /*  Copyright 1999 Institut National de Recherche en Informatique et   */
  /*  Automatique.  Distributed only by permission.                      */
  /*                                                                     */
  /***********************************************************************/
  
  
  %{
  open Misc
  open Rtl
  open Asm

  let mknode () = { instrs = [] }
  let mkframe () = { size = 0; pos = []; }
    
  let zero = Const (Const_int 0)
  
  let labels = Hashtbl.create 91
  let frames = Hashtbl.create 91
    
  let reset () = 
    Hashtbl.clear labels;
    Hashtbl.clear frames
    
  let label lbl = 
    try Hashtbl.find labels lbl 
    with Not_found -> 
        let node = mknode () in
        Hashtbl.add labels lbl node;
        node

  let frame lbl = 
    try Hashtbl.find frames lbl 
    with Not_found -> 
        let frame = mkframe () in
        Hashtbl.add frames lbl frame;
        frame
        
      %}
  
  %token          EOF
  %token <string> IDENT
  %token <string> STRING
  %token <float>  FLOAT
  %token <int>    INT
  %token          TEXT
  %token          GLOBAL
  %token          DATA
  %token          LONG
  %token          ASCII
  %token          WORD
  %token          BYTE
  %token          SPACE
  %token          COLON
  %token          ALIGN
  %token <string> LABEL
  %token <string> IMM_SYMBOL
  %token <int>    IMM_INT
  %token          COMMA
  %token          LPAREN
  %token          RPAREN
  %token <Asm.register>    REGISTER
  %token          PLUS
  %token          MINUS
  %token          STAR
  %token          DOUBLE
  %token <Asm.opcode> INSTR0
  %token <Asm.opcode> INSTR1
  %token <Asm.opcode> INSTR2

  %token     FSTPL 
  %token     FLDL 
  %token     FCHS 
  %token     FABS 
  %token     FADDL 
  %token     FSUBL 
  %token     FMULL 
  %token     FDIVL 
  %token     FSUBRL 
  %token     FDIVRL 
  %token     FADDP 
  %token     FSUBP 
  %token     FMULP 
  %token     FDIVP 
  %token     FSUBRP 
  %token     FDIVRP 
  %token     FLDZ 
  %token     FLD1 
  %token     FILDL 
  %token     FNSTCW 
  %token     FLDCW 
  %token     FISTPL 

  %token     CALL 
  %token     JMP 
  %token     IDIVL 
  %token     LEA 
  %token     LEAL 
  %token     RET 

  %token  <Misc.signed_comparison>   JCOND 
  %token  <Misc.signed_comparison>   SETCOND 
  %token EOL
  %token STR
  %token TYPE
  %token SIZE
  %token OBJECT
  %token FUNCTION
  %token SKIP

  %token DIR_RES
  %token DIR_ARGS
  %token DIR_SAVED
  %token DIR_LIVE
  %token DIR_BEGINDATA
  %token DIR_ENDDATA
  %token DIR_BEGINCODE
  %token DIR_ENDCODE
  %token DIR_SWITCH
  %token DIR_ENTRY
  %token DIR_EXIT
  %token DIR_TAILCALL
  %token DIR_CONTINUE
  %token DIR_RECORD
  %token DIR_NOGC
  %token DIR_FRAMETABLE
  %token DIR_SETUPTRAP
  %token DIR_PUSHTRAP
  %token DIR_POPTRAP
  %token DIR_RAISE

  %token MACRO_DEFINE
  %token MACRO_INLINE
  %token LBRACE
  %token RBRACE

  %start asmfile
  %type <string * ((int * string * Asm.data list) list) * (string * Asm.node list) list> asmfile
  %start macrofile
  %type <Rtl.macro list> macrofile
    
    %%
  
    asmfile: 
  begin_data begin_code data_segments text_segments end_code end_data frametable
    { ($1, $3, $4) }
  ;
  
  begin_data:
  DATA EOL GLOBAL IDENT EOL IDENT COLON EOL  { $4 }
  ;

  begin_code:
  TEXT EOL GLOBAL IDENT EOL IDENT COLON EOL  { () }
  ;

  end_data:
  DATA EOL GLOBAL IDENT EOL IDENT COLON EOL  { $4 }
  ;

  end_code:
  DIR_ENDCODE EOL TEXT EOL GLOBAL IDENT EOL IDENT COLON EOL  { () }
  ;

  data_segments:
  DATA EOL LONG INT GLOBAL IDENT EOL IDENT COLON EOL data_segment data_segments
                                      { ($4,$6,$11) :: $12 }
|                                     { [] }
  ;
  
   data_segment:
  data EOL data_segment { $1 :: $3 }
|                       { [] }
  ;
    
  data:
  LABEL COLON           { Label $1 }
| LONG int              { Long $2  }
| ASCII STRING          { Ascii $2 }
| STR STRING            { String $2 }
| WORD int              { Word $2  }
| BYTE int              { Byte $2 }
| SPACE INT             { Space $2 }
| SKIP INT              { Skip $2 }
| ALIGN INT             { Align $2 }
;

  text_segments:
  TEXT EOL ALIGN INT EOL GLOBAL IDENT EOL IDENT COLON EOL text_segment text_segments
    { ($7, $12) :: $13 }
|   { [] }
  ;
  
  text_segment:
  block text_segment { $1 :: $2 }
|                    { [] }
  ;
  
  block:
  LABEL COLON EOL instrs { let node = label $1 in node.instrs <- $4; node }
| instrs                 { let node = mknode () in node.instrs <- $1; node }
  ;
  
  instrs:
| instr EOL instrs { $1 :: $3 }
| end_instr        { [$1] }
|                  { [] }
  ;
  
  end_instr:
| DIR_SWITCH  switch  { let (r,cases) = $2 in Switch (r,cases), [] }
| CALL reg       { Call, [$2] }
| JMP reg        { Jmp, [$2] }
| RET                 { Ret,[] }
| JCOND reg      { Jcond $1, [$2] }
    
  instr:
| INSTR0               { $1, [] }
| INSTR1 reg           { $1, [$2] }
| INSTR2 reg COMMA reg { $1, [$2; $4] }
| IDIVL reg            { Idivl, [$2] }
| LEA reg COMMA reg    { Lea, [$2;$4] }
| LEAL reg COMMA reg   { Leal, [$2;$4] }
| SETCOND reg          {  SetCond $1, [$2] }
| DIR_SAVED reg_list   { Directive Dir_saved, $2 }
| DIR_RES reg_list     { Directive Dir_res, $2 }
| DIR_ARGS reg_list    { Directive Dir_args, $2 }
| DIR_LIVE reg_list    { Directive Dir_live, $2 }
| DIR_RECORD IDENT COLON reg_list { Directive (Dir_record $2), $4 }
| DIR_RECORD LABEL COLON reg_list { Directive (Dir_record $2), $4 }
| DIR_ENTRY            { Directive Dir_entry, [] }
| DIR_EXIT             { Directive Dir_exit, [] }
| DIR_TAILCALL         { Directive Dir_tailcall, [] }
| DIR_CONTINUE         { Directive Dir_continue, [] }
| DIR_NOGC             { Directive Dir_nogc, [] }
  ;

  switch:
  
  INT COLON EOL
    JMP STAR LABEL LPAREN COMMA REGISTER COMMA INT RPAREN EOL
    DATA EOL
    LABEL COLON EOL cases TEXT { $9, $19 }
  ;
  
  cases:
  LONG LABEL EOL cases { (label $2) :: $4 }
| { [] }
  ;
  
  frametable:
  DIR_FRAMETABLE EOL 
    GLOBAL IDENT EOL
    IDENT COLON EOL
    LONG INT EOL frames   { () }
  ;
  
  frames:
  frame EOL frames { () }
|                  { () }
  ;
  
  frame:
  LONG LABEL EOL WORD INT EOL WORD INT EOL words ALIGN INT 
    { let frame = frame $2 in 
    frame.size <- $5;
    frame.pos <- $10
    }
  ;
  
  words:
  WORD INT EOL words { $2 :: $4 }
|                    { [] }
  ;
  
  

  asmfile2:
  any EOF             { $1 }
| EOF                 { [] }
  ;  

  macrofile:
  macro macrofile     { $1 :: $2 }
| EOL macrofile       { $2 }
| EOF                 { [] }
  ;
  
  macro:
  MACRO_DEFINE name LBRACE any RBRACE { Macro_define ($2, $4) }
| MACRO_INLINE name                   { Macro_inline $2 }
  ;

  name:
  IDENT STAR { $1, true }
| IDENT      { $1, false }
  ;
  
  any:
  instr EOL any { $1 :: $3 }
  | instr { [$1] }
  | EOL any { $2 }
  | EOL { [] }
  | { [] }
  ;
      
  instr:
  DATA         { Data, [] }
| GLOBAL IDENT { Global $2, [] }
| TEXT         { Text, [] }
| LONG int     { Long $2, [] }
| ASCII STRING { Ascii $2, [] }
| STR STRING { String $2, [] }
| WORD int     { Word $2, [] }
| BYTE int     { Byte $2, [] }
| SPACE INT     { Space $2, [] }
| SKIP INT     { Skip $2, [] }
| ALIGN INT     { Align $2, [] }

| INSTR0        { $1, [] }
| INSTR1 reg    { $1, [$2] }
| INSTR2 reg COMMA reg { $1, [$2; $4] }
  
| CALL code_reg    { Call, [$2] }
| JMP code_reg    { Jmp, [$2] }
| IDIVL reg    { Idivl, [$2] }
| LEA reg COMMA reg   { Lea, [$2;$4] }
| LEAL reg COMMA reg   { Leal, [$2;$4] }
| RET { Ret,[] }
| JCOND code_reg   { Jcond $1, [$2] }
| SETCOND reg {  SetCond $1, [$2] }
| TYPE IDENT COMMA stype { Type ($2, $4), [] }
| SIZE IDENT COMMA int   { Size ($2, $4), [] }
| DIR_SAVED reg_list { Directive Dir_saved, $2 }
| DIR_RES reg_list { Directive Dir_res, $2 }
| DIR_ARGS reg_list { Directive Dir_args, $2 }
| DIR_LIVE reg_list { Directive Dir_live, $2 }
| DIR_RECORD IDENT COLON reg_list { Directive (Dir_record $2), $4 }
| DIR_BEGINDATA IDENT COLON { Directive (Dir_begindata $2), [] }
| DIR_BEGINCODE IDENT COLON { Directive (Dir_begincode $2), [] }
| DIR_ENDDATA IDENT COLON { Directive (Dir_enddata $2), [] }
| DIR_ENDCODE IDENT COLON { Directive (Dir_endcode $2), [] }
| DIR_RECORD LABEL COLON reg_list { Directive (Dir_record $2), $4 }
| DIR_BEGINDATA LABEL COLON { Directive (Dir_begindata $2), [] }
| DIR_BEGINCODE LABEL COLON { Directive (Dir_begincode $2), [] }
| DIR_ENDDATA LABEL COLON { Directive (Dir_enddata $2), [] }
| DIR_ENDCODE LABEL COLON { Directive (Dir_endcode $2), [] }
| DIR_SWITCH INT COLON  { Directive (Dir_switch $2), [] }
| DIR_ENTRY       { Directive Dir_entry, [] }
| DIR_EXIT       { Directive Dir_exit, [] }
| DIR_TAILCALL       { Directive Dir_tailcall, [] }
| DIR_CONTINUE       { Directive Dir_continue, [] }
| DIR_NOGC           { Directive Dir_nogc, [] }
| DIR_FRAMETABLE     { Directive Dir_frametable, [] }
  ;

  /* 
| DOUBLE FLOAT { Double $2 }
  | FLDL reg            { Fldl $2 }
| FILDL reg      { Fildl $2 }
| FSTPL reg    { Fstpl $2 }
| FDIVL reg   { Fdivl $2 }
| FMULL reg   { Fmull $2 }
| FCHS { Fchs }
| FABS { Fabs }
| FADDL reg    { Faddl $2 }
| FADDP reg COMMA reg   { Faddp [$2;$4] }
| FSUBP reg COMMA reg   { Fsubp [$2;$4] }
| FMULP reg COMMA reg   { Fmulp [$2;$4] }
| FDIVP reg COMMA reg   { Fdivp [$2;$4] }
| FSUBRP reg COMMA reg   { Fsubrp [$2;$4] }
| FDIVRP reg COMMA reg   { Fdivrp [$2;$4] }
| FLDZ { Fldz }
| FLD1 { Fld1 }
*/
  
  stype:
  OBJECT { Object }
| FUNCTION { Function }
  ;
  
  reg:
/*
  REGISTER LPAREN INT RPAREN { 
    if $1 <> St 0 then failwith "PARSE ERROR" else
      Indexed(Const_int $3, Register $1, Const(Const_int 0),0) }
  */
| REGISTER                             { Register $1 }
| IDENT PLUS INT { 
    ConstantBase (Const_add (Const_int $3, Const_symbol $1)) }
| IDENT MINUS INT { 
    ConstantBase (Const_add (Const_int (- $3), Const_symbol $1)) }
| IDENT    { 
    ConstantBase (Const_add (Const_int 0, Const_symbol $1)) }
| LABEL PLUS INT { 
    ConstantBase (Const_add (Const_int $3, Const_label $1)) }
| LABEL MINUS INT { 
    ConstantBase (Const_add (Const_int (-$3), Const_label $1)) }
| LABEL    { 
    ConstantBase (Const_add (Const_int 0, Const_label $1)) }
| IMM_INT                              { Const (Const_int $1) }
| IMM_SYMBOL                           { Const (Const_symbol ($1)) }
| int LPAREN REGISTER RPAREN           { OffsetBase ($1, $3) }
| int LPAREN REGISTER COMMA REGISTER RPAREN { 
    OffsetBaseIndex($1, $3, $5) }
| int LPAREN REGISTER COMMA REGISTER COMMA INT RPAREN { 
    OffsetBaseIndexScale ($1, $3, $5, Const_int $7) }
| int LPAREN COMMA REGISTER COMMA INT RPAREN { 
    OffsetIndexScale ($1, $4, Const_int $6) }
| LPAREN REGISTER RPAREN {
    OffsetBase (Const_int 0, $2) }
| LPAREN REGISTER COMMA REGISTER RPAREN { 
    OffsetBaseIndex(Const_int 0, $2, $4) }
| LPAREN REGISTER COMMA REGISTER COMMA INT RPAREN { 
    OffsetBaseIndexScale(Const_int 0, $2, $4, Const_int $6) }
| LPAREN COMMA REGISTER COMMA INT RPAREN { 
    OffsetIndexScale(Const_int 0, $3, Const_int $5) }
  ;
  
  code_reg:
| IDENT    { Const (Const_symbol $1) }
| LABEL    { Const (Const_label $1) }  
| STAR reg { Indirect $2 }
  ;
  
  int:
  const_int PLUS const_int { Const_add ($1,$3) }
| const_int MINUS const_int { Const_sub ($1,$3) }
| const_int { $1 }
  ;
  
  const_int:
  sint { Const_int $1 }
| IDENT { Const_symbol $1 }
| LABEL { Const_label $1 }
  ;

  sint:
  INT { $1 }
| MINUS INT { - $2 }
  ;
  
  reg_list:
  reg COLON reg_list { $1 :: $3 }
|           { [] }
  ;
  