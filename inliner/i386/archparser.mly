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
  open Asm
      
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
  %token     IMMEDIAT
  %token     FLDL 
  %token     FNSTCW 
  %token     FLDCW 
  %token     FISTPL 
  %token     DOT  
  %token     CALL 
  %token     JMP 
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
  %token DIR_FRAME
  %token DIR_ARITY
  %token DIR_ALLOC
  
  %token MACRO_DEFINE
  %token MACRO_INLINE
  %token MACRO_PEEPHOLE
  %token LBRACE
  %token RBRACE
  %token DEAD
  %token DISTINCT
  %token EQUAL
  %token SAMEREG 
  %token SAMEVALUE
  %token BININSTR
  %token <Asm.register Asm.argument> ARGUMENT
  %token <Asm.constant> CONSTANT
  %token <Asm.approx_state> INSTRUCTION

  %start asmfile
  %type <unit> asmfile
    %start macrofile
    %type <Asm.macro list> macrofile
    
    %%
    
    asmfile: 
  begin_data begin_code data_segments text_segments end_code end_data frametable
    { 
    Hashtbl.clear program.desc;
    Hashtbl.clear program.env;
    program.name <- $1;
    program.data <- $3;
    program.globals <- List.map (fun (name, node) ->
        Hashtbl.add program.desc name node; name
    ) $4;
  }
  ;
  
  begin_data:
  DATA EOL GLOBAL IDENT EOL IDENT COLON EOL  { 
    let suffix = "_data_begin" in
    let name = $4 in
    String.sub name 0 (String.length name - String.length suffix)
  }
  ;
  
  begin_code:
  TEXT EOL GLOBAL IDENT EOL IDENT COLON EOL  { () }
  ;
  
  end_data:
  DATA EOL GLOBAL IDENT EOL IDENT COLON EOL LONG INT EOL  { $4 }
  ;
  
  end_code:
  DIR_ENDCODE EOL TEXT EOL GLOBAL IDENT EOL IDENT COLON EOL   { () }
  ;
  
  data_segments:
  DATA EOL LONG INT EOL GLOBAL IDENT EOL IDENT COLON EOL data_segment data_segments
    { {
      data_header = $4;
      data_name = $7;
      data_value = $12;
    } :: $13 }
|                                     { [] }
  ;
  
  data_segment:
  data EOL data_segment { $1 :: $3 }
|                       { [] }
  ;
  
  data:
  LABEL COLON           { Label $1 }
| LONG data_int         { Long $2  }
| ASCII STRING          { Ascii $2 }
| STR STRING            { String $2 }
| WORD data_int              { Word $2  }
| BYTE data_int              { Byte $2 }
| SPACE INT             { Space $2 }
| SKIP INT              { Skip $2 }
| ALIGN INT             { Align $2 }
| DOUBLE FLOAT          { Double $2 }  
| DOUBLE MINUS FLOAT    { Double (-. $3) }  
  ;
  
  text_segments:
  TEXT EOL 
    ALIGN INT EOL
    GLOBAL IDENT EOL
    DIR_ARITY INT COLON EOL 
    IDENT COLON EOL
    text_segment text_segments
    { ($7, mkfunc $7 $10 $16) :: $17 }
| DATA EOL 
    LABEL COLON EOL 
    DOUBLE FLOAT EOL text_segments { 
    let f = float $3 in
    f.float <- $7;
    $9 }
| DATA EOL 
    LABEL COLON EOL 
    DOUBLE MINUS FLOAT EOL text_segments { 
    let f = float $3 in
    f.float <- -. $8;
    $10 }
|   { [] }
  ;
  
  text_segment:
  LABEL COLON EOL instrs next_segment 
    { 
    let node = label $1 in
    let instrs = $4 in
    node.instrs <- Array.of_list instrs;
    let last_instr = $5 in
    node.link <- last_instr;
    node }
| instrs next_segment
    { 
    let node = mknode () in
    let instrs = $1 in
    node.instrs <- Array.of_list instrs;
    let last_instr = $2 in
    node.link <- last_instr;
    node 
  }
| { mknode () }
  ;
  
  next_segment:
| JCOND arg EOL text_segment      { mkinstr (Jcond ($1,$4)) [$2] [] }
| DIR_SETUPTRAP EOL CALL LABEL EOL text_segment
    { let node = label $4 in mkinstr (Setuptrap (node,$6)) [] [] }
| DIR_POPTRAP EOL
    INSTR1 IDENT EOL
    INSTR2 IMMEDIAT INT COMMA REGISTER EOL text_segment 
    { mkinstr (Poptrap $12) [] [] }
| text_segment                
    { mkinstr Jmp [ ConstantBase(Const_label $1)] [] }
| end_instr EOL text_segment      { $1 }
  ;
  
  instrs:
| directives instr EOL instrs { 
    let op, args = $2 in
    let instr = mkinstr op args $1 in
    instr :: $4
  }
| LPAREN INSTRUCTION DOT RPAREN instr EOL instrs {
    let op, args = $5 in
    let instr = mkinstr op args [] in
    instr.enter_instr <- $2;
    instr :: $7
  }
| LPAREN DOT INSTRUCTION RPAREN instr EOL instrs {
    let op, args = $5 in
    let instr = mkinstr op args [] in
    instr.leave_instr <- $3;
    instr :: $7
  }
| LPAREN INSTRUCTION DOT INSTRUCTION RPAREN instr EOL instrs {
    let op, args = $6 in
    let instr = mkinstr op args [] in
    instr.enter_instr <- $2;
    instr.leave_instr <- $4;
    instr :: $8
  }
| instr EOL instrs {
    let op, args = $1 in
    let instr = mkinstr op args [] in
    instr :: $3
  }
|                 { [] }    
    
    end_instr:
| DIR_SWITCH  switch  { let (r,cases) = $2 in 
    mkinstr (Switch (cases)) [Register r] [] }
| DIR_TAILCALL EOL directives JMP arg        
    { mkinstr Jmp [$5] (Dir_tailcall :: $3) }
| JMP arg        { mkinstr Jmp [$2] [] }
| RET            { mkinstr Ret [] []}
| DIR_RAISE EOL 
    INSTR2 arg COMMA arg EOL
    INSTR1 arg EOL
    RET { mkinstr Raise [] [] }
| DIR_RAISE EOL
    DIR_LIVE reg_list EOL
    INSTR2 arg COMMA arg EOL
    INSTR1 arg EOL
    RET { mkinstr Raise [] [Dir_live $4] }
  ;
  
  instr:
| DIR_PUSHTRAP EOL
    INSTR1 IDENT EOL
    INSTR2 REGISTER COMMA IDENT { Pushtrap, [] }
| INSTR0               { $1, [] }
| INSTR1 arg           { $1, [$2] }
| INSTR2 arg COMMA arg { $1, [$2; $4] }
| SETCOND arg          {  SetCond $1, [$2] }
| DIR_FRAME LABEL COLON EOL CALL arg { Call (Some (frame $2)), [$6] }
| CALL arg       { Call None, [$2] }
| FLDL LABEL           { Fldl, [ConstantBase (Const_float (float $2))] }
| FLDL arg             { Fldl, [$2] }
| DIR_ALLOC INT COLON LABEL COLON EOL
    alloc
    INSTR2 INT LPAREN REGISTER RPAREN COMMA REGISTER 
    { Alloc ($2, $2, frame $4, $7), [Register $14] }
  ;
  
  alloc:
  LABEL COLON EOL
    INSTR2 IDENT COMMA REGISTER EOL
    INSTR2 IMMEDIAT INT COMMA REGISTER EOL
    INSTR2 REGISTER COMMA IDENT EOL
    INSTR2 IDENT COMMA REGISTER EOL
    JCOND LABEL EOL  { true }
|  CALL IDENT EOL
    LABEL COLON EOL  { false }
| INSTR2 IMMEDIAT INT COMMA REGISTER EOL
    CALL IDENT EOL
    LABEL COLON EOL  { false }
  ;
  
  directives:
  directive EOL directives { $1 :: $3 }
| directive EOL            { [$1] }
  ;
  
  directive:
| DIR_SAVED reg_list   { Dir_saved $2 }
| DIR_RES reg_list     { Dir_res $2 }
| DIR_ARGS reg_list    { Dir_args $2 }
| DIR_LIVE reg_list    { Dir_live $2 }
| DIR_NOGC             { Dir_nogc }
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
    { 
    let frame = frame $2 in 
    frame.size <- $5;
    frame.pos <- $10
  }
  ;
  
  words:
  WORD INT EOL words { $2 :: $4 }
|                    { [] }
  ;
  
  macrofile:
  macro macrofile     { $1 :: $2 }
| EOL macrofile       { $2 }
| EOF                 { [] }
  ;
  
  macro:
  MACRO_DEFINE name LBRACE EOL text_segment RBRACE  { Macro_replace ($2, $5) }
| MACRO_INLINE name                                 { Macro_inline $2 }
| MACRO_PEEPHOLE IDENT LBRACE EOL 
    text_segment 
    RBRACE EOL
    predicats 
    LBRACE EOL
    text_segment
    RBRACE {
    let vars = Match.clear () in
    let m = Macro_peephole (vars, $2, $5, $8, $11) in    
    m
    }
  ;

  predicats:
  predicat EOL predicats  { $1 :: $3 }
|                         { [] }
  ;
  
  predicat:
  DEAD LPAREN INSTRUCTION COLON reg_comma_list RPAREN { Dead ($3,$5) }
| DISTINCT LPAREN reg_comma_list RPAREN { Distinct $3 }
| EQUAL LPAREN const_comma_list RPAREN { Equal $3 }
| SAMEREG LPAREN reg_comma_list RPAREN { SameReg $3 } 
| SAMEVALUE LPAREN value_comma_list RPAREN  { SameValue $3 }
| BININSTR LPAREN INSTR2 COLON instr2_comma_list RPAREN { Instr ($3, $5) }
  ;

  instr2_comma_list:
  INSTR2 COMMA instr2_comma_list { $1 :: $3 }
| INSTR2                         { [$1] }
  ;
   
  value_comma_list:
  INSTRUCTION DOT REGISTER COMMA value_comma_list { ($1, $3) :: $5 }
| INSTRUCTION DOT REGISTER                        { [$1, $3] }
  ;
  
  reg_comma_list:
  REGISTER COMMA reg_comma_list { $1 :: $3 }
| REGISTER                      { [$1] }
  ;
  
  const_comma_list:
  int COMMA const_comma_list { $1 :: $3 }
| int                        { [$1] }
  
  name:
  IDENT STAR { $1, true }
| IDENT      { $1, false }
  ;
    
  arg:
  ARGUMENT { $1 }
| REGISTER LPAREN INT RPAREN { 
    if $1 <> St 0 then failwith "PARSE ERROR" else
      Register (St $3)
  }
| REGISTER                             { Register $1 }
| IDENT PLUS INT { 
    ConstantBase (Const_add (Const_int $3, Const_symbol $1)) }
| IDENT MINUS INT { 
    ConstantBase (Const_add (Const_int (- $3), Const_symbol $1)) }
| IDENT    { 
    ConstantBase (Const_symbol $1) }
| IMMEDIAT int                              { Const ($2) }
| int LPAREN REGISTER RPAREN           { OffsetBase ($1, $3) }
| int LPAREN REGISTER COMMA REGISTER RPAREN { 
    OffsetBaseIndex($1, $3, $5) }
| int LPAREN REGISTER COMMA REGISTER COMMA INT RPAREN { 
    OffsetBaseIndexScale ($1, $3, $5, $7) }
| int LPAREN COMMA REGISTER COMMA INT RPAREN { 
    OffsetIndexScale ($1, $4, $6) }
| LPAREN REGISTER RPAREN {
    OffsetBase (Const_int 0, $2) }
| LPAREN REGISTER COMMA REGISTER RPAREN { 
    OffsetBaseIndex(Const_int 0, $2, $4) }
| LPAREN REGISTER COMMA REGISTER COMMA INT RPAREN { 
    OffsetBaseIndexScale(Const_int 0, $2, $4, $6) }
| LPAREN COMMA REGISTER COMMA INT RPAREN { 
    OffsetIndexScale(Const_int 0, $3, $5) }
| LABEL    { 
    ConstantBase (Const_label (label $1)) }
| STAR arg { Indirect $2 }
;
  
  int:
  const_int PLUS const_int { Const_add ($1,$3) }
| const_int MINUS const_int { Const_sub ($1,$3) }
| const_int { $1 }
  ;

  data_int:
  data_int PLUS data_int { Const_add ($1,$3) }
| sint { Const_int $1 }
| IDENT { Const_symbol $1 }
| LABEL { Const_data_label $1 }
  ;
  
  const_int:
  sint { Const_int $1 }
| IDENT { Const_symbol $1 }
| CONSTANT { $1 }
  ;

  sint:
  INT { $1 }
| MINUS INT { - $2 }
  ;
  
  reg_list:
  arg COLON reg_list { $1 :: $3 }
|           { [] }
  ;
  
  register_list:
  REGISTER COLON register_list { $1 :: $3 }
|           { [] }
  ;
 