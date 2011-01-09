%{
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
  
  open Xtypes
  open Twm_t
      
      
      %}
  %token EOF
  %token  LB RB LP RP MENUS MENU BUTTON DEFAULT_FUNCTION PLUS MINUS
  %token  ALL OR CURSORS PIXMAPS ICONS COLOR SAVECOLOR MONOCHROME FUNCTION 
  %token  ICONMGR_SHOW ICONMGR WINDOW_FUNCTION ZOOM ICONMGRS
  %token  ICONMGR_GEOMETRY ICONMGR_NOSHOW MAKE_TITLE GRAYSCALE
  %token  ICONIFY_BY_UNMAPPING DONT_ICONIFY_BY_UNMAPPING 
  %token  NO_TITLE AUTO_RAISE NO_HILITE ICON_REGION 
  %token  META SHIFT LOCK CONTROL WINDOW TITLE ICON ROOT FRAME 
  %token  COLON EQUALS SQUEEZE_TITLE DONT_SQUEEZE_TITLE
  %token  START_ICONIFIED NO_TITLE_HILITE TITLE_HILITE STICKY
  %token  MOVE RESIZE WAIT SELECT KILL LEFT_TITLEBUTTON RIGHT_TITLEBUTTON 
  %token <int>  NUMBER
  %token <Twm_t.keyword> KEYWORD
  %token <Twm_t.nkeyword> NKEYWORD
  %token <Twm_t.clkeyword> CKEYWORD
  %token <Twm_t.clkeyword> CLKEYWORD
  %token <Twm_t.fkeyword> FKEYWORD
  %token <Twm_t.fskeyword> FSKEYWORD 
  %token <Twm_t.skeyword> SKEYWORD 
  %token <Twm_t.dkeyword> DKEYWORD
  %token <Twm_t.jkeyword> JKEYWORD
  %token WINDOW_RING WARP_CURSOR ERRORTOKEN
  %token  NO_STACKMODE
  %token <string> STRING 
  
  %type <Twm_t.twm list> twmrc
    %start twmrc 
    
    %%
    twmrc      : stmts EOF { $1 }
    
    stmts      : /* Empty */ { [] }
| stmts stmt       { $2 :: $1 }
    
    
    stmt: 
  error { Error }
| noarg { $1 }
| sarg { $1 }
| narg { $1 }
| squeeze { $1 }
| ICON_REGION string DKEYWORD DKEYWORD number number
    { AddIconRegion($2, $3, $4, $5, $6) }
| ICONMGR_GEOMETRY string number   { IconMgrGeometry($2,Some $3) }
| ICONMGR_GEOMETRY string   { IconMgrGeometry($2,None) }
| ZOOM number      { ZoomCount $2 }
| ZOOM             { ZoomCount (-1) }
| PIXMAPS pixmap_list   { Pixmap_list $2 }
| CURSORS cursor_list   { Cursor_list $2 }
| ICONIFY_BY_UNMAPPING win_list   { IconifyByUnmapping $2 }
| ICONIFY_BY_UNMAPPING   { IconifyByUnmappingAll }
| LEFT_TITLEBUTTON string EQUALS action { TitleButton ($2, $4, false) }
| RIGHT_TITLEBUTTON string EQUALS action { TitleButton ($2, $4, true) }
| button string      { ButtonMenu ($1,$2) }
| button action      { ButtonAction ($1,$2) }
| string EQUALS keys COLON contextkeys COLON action   { Key($1, $3,$5,$7) }
| button EQUALS keys COLON contexts COLON action   { Button($1, $3,$5,$7) }
| DONT_ICONIFY_BY_UNMAPPING win_list { DontIconify $2 }
| ICONMGR_NOSHOW win_list    { IconManagerNoShow $2 }
| ICONMGR_NOSHOW   { IconManagerNoShowAll }
| ICONMGRS iconm_list { IconMgrs $2 }
| ICONMGR_SHOW win_list   { IconManagerShow $2 }
| NO_TITLE_HILITE win_list  { NoTitleHighlight $2 }
| NO_TITLE_HILITE   { NoTitleHighlightAll  }
| NO_HILITE win_list { NoHighlight $2 }
| NO_HILITE      { NoHighlightAll  }
| NO_STACKMODE win_list { NoStackMode $2 }
| NO_STACKMODE     { NoStackModeAll }
| NO_TITLE  win_list    { NoTitlebar $2 }
| NO_TITLE              { NoTitlebarAll }
| MAKE_TITLE win_list      { MakeTitle $2 }
| START_ICONIFIED win_list   { StartIconified $2 }
| AUTO_RAISE win_list      { AutoRaise $2 }
| MENU string LP string COLON string RP menu { 
    RootMenu($2,Some $4,Some $6,$8) }
| MENU string menu       { RootMenu($2, None, None, $3) }
| FUNCTION string fonction   { RootFunction($2, $3) }
| ICONS icon_list       { IconNames $2 }
| COLOR color_list      { ColorList $2 }
| GRAYSCALE color_list      { GrayscaleList $2 }
| SAVECOLOR  save_color_list { SaveColorList $2 }
| MONOCHROME color_list       { Monochrome $2 }
| DEFAULT_FUNCTION action { DefaultFunction $2 }
| WINDOW_FUNCTION action { WindowFunction $2 }
| WARP_CURSOR win_list      { WarpCursorList $2 }
| WARP_CURSOR      { WarpCursorAll }
| WINDOW_RING win_list      { WindowRingList $2 }
        
    noarg      : KEYWORD      { NoArg $1 }
    sarg      : SKEYWORD string   { StringArg ($1,$2) }
    narg      : NKEYWORD number   { NumberArg ($1,$2) }

  keys      : /* Empty */      { 0 }
| keys key                   { $2 lor $1 }
| keys OR keys                   { $3 lor $1 }
    
    key  : META     { mod1Mask }
| SHIFT         { shiftMask }
| LOCK          { lockMask }
| CONTROL       { controlMask }
| META number   { mod1Mask lsl ($2 - 1) }
    
    
    contexts   : /* Empty */ { [] }
| contexts context       { $2 :: $1 }
| contexts OR contexts       { $3 @ $1 }
        
    context      : WINDOW      { C_WINDOW }
| TITLE         { C_TITLE }
| ICON         { C_ICON }
| ROOT         { C_ROOT }
| FRAME         { C_FRAME }
| ICONMGR      { C_ICONMGR }
| META         { C_ICONMGR }
| ALL         { C_ALL }
        
    contextkeys   : /* Empty */ { [] }
| contextkeys contextkey      { $2 :: $1 }
| contextkeys OR contextkeys      { $3 @ $1 }
    
    
    contextkey   : WINDOW      { C_WINDOW }
| TITLE         { C_TITLE }
| ICON         { C_ICON }
| ROOT         { C_ROOT }
| FRAME         { C_FRAME }
| ICONMGR      { C_ICONMGR }
| META         { C_ICONMGR }
| ALL         { C_ALL }
| string      { C_NAME $1 }
            
    pixmap_list   : LB pixmap_entries RB { List.rev $2 }
        
    pixmap_entries   : /* Empty */ { [] }
| pixmap_entries pixmap_entry    { $2 :: $1 }
        
    pixmap_entry   : TITLE_HILITE string { $2 }        
    
    cursor_list   : LB cursor_entries RB { List.rev $2 }
    
    cursor_entries   : /* Empty */ { [] }
| cursor_entries cursor_entry    { $2 :: $1 }
    
  cursor_entry   : FRAME string string { 
    NewBitmapCursor( FrameCursor, $2, $3) }
| FRAME string         { NewFontCursor(FrameCursor, $2) }
| TITLE string string  {  NewBitmapCursor(TitleCursor, $2, $3) }
| TITLE string         { NewFontCursor(TitleCursor, $2) }
| ICON string string   { NewBitmapCursor( IconCursor, $2, $3) }
| ICON string          { NewFontCursor( IconCursor, $2) }
| ICONMGR string string {
    NewBitmapCursor(IconMgrCursor, $2, $3) }
| ICONMGR string {
    NewFontCursor( IconMgrCursor, $2) }
| BUTTON string string {
    NewBitmapCursor( ButtonCursor, $2, $3) }
| BUTTON string {
    NewFontCursor( ButtonCursor, $2) }
| MOVE string string {
    NewBitmapCursor( MoveCursor, $2, $3) }
| MOVE string {
    NewFontCursor( MoveCursor, $2) }
| RESIZE string string {
    NewBitmapCursor(ResizeCursor, $2, $3) }
| RESIZE string {
    NewFontCursor(ResizeCursor, $2) }
| WAIT string string { NewBitmapCursor(WaitCursor, $2, $3) }
| WAIT string { NewFontCursor(WaitCursor, $2) }
| MENU string string { NewBitmapCursor(MenuCursor, $2, $3) }
| MENU string { NewFontCursor(MenuCursor, $2) }
| SELECT string string { NewBitmapCursor(SelectCursor, $2, $3) }
| SELECT string { NewFontCursor(SelectCursor, $2) }
| KILL string string { NewBitmapCursor(DestroyCursor, $2, $3) }
| KILL string { NewFontCursor(DestroyCursor, $2) }
    
    
    color_list   : LB color_entries RB { List.rev $2 }
    
    
    
    color_entries   : /* Empty */ { [] }
| color_entries color_entry    { $2 :: $1 }
    
    
    color_entry   : CLKEYWORD string   {  $1,$2,[] }
| CLKEYWORD string    win_color_list { $1,$2,$3 }
| CKEYWORD string   { $1,$2,[] }
    
    save_color_list : LB s_color_entries RB { List.rev $2 }
    
    
    s_color_entries : /* Empty */  { [] }
| s_color_entries s_color_entry  { $2 :: $1 }
    
    
    s_color_entry   : string            { SaveColor $1 }
| CLKEYWORD         { SaveColorVar $1 }
    
    
    win_color_list   : LB win_color_entries RB { List.rev $2 }
    
    
    win_color_entries   : /* Empty */ { [] }
| win_color_entries win_color_entry { $2 :: $1 }
    
    
    win_color_entry   : string string      {  $1, $2 }
    
    
    squeeze      : SQUEEZE_TITLE { SqueezeTitleAll }
| SQUEEZE_TITLE  LB win_sqz_entries RB { SqueezeTitleList $3 }
| DONT_SQUEEZE_TITLE { DontSqueezeTitleAll }
| DONT_SQUEEZE_TITLE win_list { DontSqueezeTitleList $2 }
    
    
    win_sqz_entries   : /* Empty */ { [] }
| win_sqz_entries string JKEYWORD signed_number number   {
    ($2, $3, $4, $5) :: $1  }
    
    
    iconm_list   : LB iconm_entries RB { List.rev $2 }
    
    iconm_entries   : /* Empty */ { [] }
| iconm_entries iconm_entry     { $2 :: $1 }
    
    
    iconm_entry   : string string number   { $1,None,$2,$3 }
| string string string number            { $1,Some $2,$3, $4  }
    
    
    win_list   : LB win_entries RB { List.rev $2 }
    
    
    win_entries   : /* Empty */ { [] }
| win_entries win_entry       { $2 :: $1 }
    
    
    win_entry   : string      { $1 }
    
    
    icon_list   : LB icon_entries RB { List.rev $2 }
    
    
    icon_entries   : /* Empty */ { [] }
| icon_entries icon_entry      { $2 :: $1 }
    
    
    icon_entry   : string string      { $1,$2 }
    
    
    fonction   : LB function_entries RB { List.rev $2 }
    
    
    function_entries: /* Empty */ { [] }
| function_entries function_entry {  $2 :: $1 }
    
    
    function_entry   : action      { $1 }
    
    menu      : LB menu_entries RB { List.rev $2 }
    
    menu_entries   : /* Empty */ { [] }
| menu_entries menu_entry    { $2 :: $1 }
    
    menu_entry   : string action      { $1, $2, None,None }
| string LP string COLON string RP action { $1, $7, Some $3, Some $5 }
    
    
    action      : FKEYWORD   { Action $1 }
| FSKEYWORD string { ActionString ($1,$2) }
    
    
    
    signed_number   : number      { $1 }
| PLUS number      { $2 }
| MINUS number      { -($2) }
    
    
    button      : BUTTON number      { $2 }
    
    
    string      : STRING      { $1 }
    number      : NUMBER      { $1 }
    
    
    %%
  