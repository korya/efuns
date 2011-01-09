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

  open Twm_t
  open Twm_p
  
  let keywords = [
      "all",         ALL;
      "autoraise",      AUTO_RAISE;
      "autorelativeresize",   KEYWORD F_AUTORELATIVERESIZE;
      "bordercolor",      CLKEYWORD F_BORDERCOLOR;
      "bordertilebackground",   CLKEYWORD F_BORDERTILEBACKGROUND;
      "bordertileforeground",   CLKEYWORD F_BORDERTILEFOREGROUND;
      "borderwidth",      NKEYWORD F_BORDERWIDTH;
      "button",         BUTTON;
      "buttonindent",      NKEYWORD F_BUTTONINDENT;
      "c",         CONTROL;
      "center",         JKEYWORD F_CENTER;
      "clientborderwidth",   KEYWORD F_CLIENTBORDERWIDTH;
      "color",         COLOR;
      "constrainedmovetime",   NKEYWORD F_CONSTRAINEDMOVETIME;
      "control",      CONTROL;
      "cursors",      CURSORS;
      "decoratetransients",   KEYWORD F_DECORATETRANSIENTS;
      "defaultbackground",   CKEYWORD F_DEFAULTBACKGROUND;
      "defaultforeground",   CKEYWORD F_DEFAULTFOREGROUND;
      "defaultfunction",   DEFAULT_FUNCTION;
      "destroy",      KILL;
      "donticonifybyunmapping",   DONT_ICONIFY_BY_UNMAPPING;
      "dontmoveoff",      KEYWORD F_DONTMOVEOFF;
      "dontsqueezetitle",   DONT_SQUEEZE_TITLE;
      "east",         DKEYWORD F_EAST;
      "f",         FRAME;
      "f.autoraise",      FKEYWORD F_AUTORAISE;
      "f.backiconmgr",      FKEYWORD F_BACKICONMGR;
      "f.beep",         FKEYWORD F_BEEP;
      "f.bottomzoom",      FKEYWORD F_BOTTOMZOOM;
      "f.circledown",      FKEYWORD F_CIRCLEDOWN;
      "f.circleup",      FKEYWORD F_CIRCLEUP;
      "f.colormap",      FSKEYWORD F_COLORMAP;
      "f.cut",         FSKEYWORD F_CUT;
      "f.cutfile",      FKEYWORD F_CUTFILE;
      "f.deiconify",      FKEYWORD F_DEICONIFY;
      "f.delete",      FKEYWORD F_DELETE;
      "f.deltastop",      FKEYWORD F_DELTASTOP;
      "f.destroy",      FKEYWORD F_DESTROY;
      "f.downiconmgr",      FKEYWORD F_DOWNICONMGR;
      "f.exec",         FSKEYWORD F_EXEC;
      "f.file",         FSKEYWORD F_FILE;
      "f.focus",      FKEYWORD F_FOCUS;
      "f.forcemove",      FKEYWORD F_FORCEMOVE;
      "f.forwiconmgr",      FKEYWORD F_FORWICONMGR;
      "f.fullzoom",      FKEYWORD F_FULLZOOM;
      "f.function",      FSKEYWORD F_FUNCTION;
      "f.hbzoom",      FKEYWORD F_HBZOOM;
      "f.hideiconmgr",      FKEYWORD F_HIDEICONMGR;
      "f.horizoom",      FKEYWORD F_HORIZOOM;
      "f.htzoom",      FKEYWORD F_HTZOOM;
      "f.hzoom",      FKEYWORD F_HZOOM;
      "f.iconify",      FKEYWORD F_ICONIFY;
      "f.identify",      FKEYWORD F_IDENTIFY;
      "f.lefticonmgr",      FKEYWORD F_LEFTICONMGR;
      "f.leftzoom",      FKEYWORD F_LEFTZOOM;
      "f.lower",      FKEYWORD F_LOWER;
      "f.menu",         FSKEYWORD F_MENU;
      "f.move",         FKEYWORD F_MOVE;
      "f.nexticonmgr",      FKEYWORD F_NEXTICONMGR;
      "f.nop",         FKEYWORD F_NOP;
      "f.previconmgr",      FKEYWORD F_PREVICONMGR;
      "f.priority",      FSKEYWORD F_PRIORITY;
      "f.quit",         FKEYWORD F_QUIT;
      "f.raise",      FKEYWORD F_RAISE;
      "f.raiselower",      FKEYWORD F_RAISELOWER;
      "f.refresh",      FKEYWORD F_REFRESH;
      "f.resize",      FKEYWORD F_RESIZE;
      "f.restart",      FKEYWORD F_RESTART;
      "f.righticonmgr",      FKEYWORD F_RIGHTICONMGR;
      "f.rightzoom",      FKEYWORD F_RIGHTZOOM;
      "f.saveyourself",      FKEYWORD F_SAVEYOURSELF;
      "f.scrollright",    FKEYWORD F_SCROLLRIGHT; (* tvtwm *)
      "f.scrolldown",    FKEYWORD F_SCROLLDOWN; (* tvtwm *)
      "f.scrollup",    FKEYWORD F_SCROLLUP; (* tvtwm *)
      "f.scrollleft",    FKEYWORD F_SCROLLLEFT; (* tvtwm *)
      "f.scrollhome",    FKEYWORD F_SCROLLHOME; (* tvtwm *)
      "f.showiconmgr",      FKEYWORD F_SHOWICONMGR;
      "f.sorticonmgr",      FKEYWORD F_SORTICONMGR;
      "f.source",      FSKEYWORD F_SOURCE;
      "f.title",      FKEYWORD F_TITLE;
      "f.topzoom",      FKEYWORD F_TOPZOOM;
      "f.twmrc",      FKEYWORD F_TWMRC;
      "f.unfocus",      FKEYWORD F_UNFOCUS;
      "f.upiconmgr",      FKEYWORD F_UPICONMGR;
      "f.version",      FKEYWORD F_VERSION;
      "f.vlzoom",      FKEYWORD F_VLZOOM;
      "f.vrzoom",      FKEYWORD F_VRZOOM;
      "f.warpring",      FSKEYWORD F_WARPRING;
      "f.warpto",      FSKEYWORD F_WARPTO;
      "f.warptoiconmgr",   FSKEYWORD F_WARPTOICONMGR;
      "f.warptoscreen",      FSKEYWORD F_WARPTOSCREEN;
      "f.winrefresh",      FKEYWORD F_WINREFRESH;
      "f.zoom",         FKEYWORD F_ZOOM;
      "forceicons",      KEYWORD F_FORCEICONS;
      "frame",         FRAME;
      "framepadding",      NKEYWORD F_FRAMEPADDING;
      "function",      FUNCTION;
      "grayscale",      GRAYSCALE;
      "greyscale",      GRAYSCALE;
      "i",         ICON;
      "icon",         ICON;
      "iconbackground",      CLKEYWORD F_ICONBACKGROUND;
      "iconbordercolor",   CLKEYWORD F_ICONBORDERCOLOR;
      "iconborderwidth",   NKEYWORD F_ICONBORDERWIDTH;
      "icondirectory",      SKEYWORD F_ICONDIRECTORY;
      "iconfont",      SKEYWORD F_ICONFONT;
      "iconforeground",      CLKEYWORD F_ICONFOREGROUND;
      "iconifybyunmapping",   ICONIFY_BY_UNMAPPING;
      "iconmanagerbackground",   CLKEYWORD F_ICONMANAGERBACKGROUND;
      "iconmanagerdontshow",   ICONMGR_NOSHOW;
      "iconmanagerfont",   SKEYWORD F_ICONMANAGERFONT;
      "iconmanagerforeground",   CLKEYWORD F_ICONMANAGERFOREGROUND;
      "iconmanagergeometry",   ICONMGR_GEOMETRY;
      "iconmanagerhighlight",   CLKEYWORD F_ICONMANAGERHIGHLIGHT;
      "iconmanagers",      ICONMGRS;
      "iconmanagershow",   ICONMGR_SHOW;
      "iconmgr",      ICONMGR;
      "iconregion",      ICON_REGION;
      "icons",         ICONS;
      "interpolatemenucolors",   KEYWORD F_INTERPOLATEMENUCOLORS;
      "l",         LOCK;
      "left",         JKEYWORD F_LEFT;
      "lefttitlebutton",   LEFT_TITLEBUTTON;
      "lock",         LOCK;
      "m",         META;
      "maketitle",      MAKE_TITLE;
      "maxwindowsize",      SKEYWORD F_MAXWINDOWSIZE;
      "menu",         MENU;
      "menubackground",      CKEYWORD F_MENUBACKGROUND;
      "menufont",      SKEYWORD F_MENUFONT;
      "menuforeground",      CKEYWORD F_MENUFOREGROUND;
      "menushadowcolor",   CKEYWORD F_MENUSHADOWCOLOR;
      "menutitlebackground",   CKEYWORD F_MENUTITLEBACKGROUND;
      "menutitleforeground",   CKEYWORD F_MENUTITLEFOREGROUND;
      "meta",         META;
      "mod",         META; 
        "monochrome",      MONOCHROME;
      "move",         MOVE;
      "movedelta",      NKEYWORD F_MOVEDELTA;
      "nobackingstore",      KEYWORD F_NOBACKINGSTORE;
      "nocasesensitive",   KEYWORD F_NOCASESENSITIVE;
      "nodefaults",      KEYWORD F_NODEFAULTS;
      "nograbserver",      KEYWORD F_NOGRABSERVER;
      "nohighlight",      NO_HILITE;
      "noiconmanagers",      KEYWORD F_NOICONMANAGERS;
      "nomenushadows",      KEYWORD F_NOMENUSHADOWS;
      "noraiseondeiconify",   KEYWORD F_NORAISEONDEICONIFY;
      "noraiseonmove",      KEYWORD F_NORAISEONMOVE;
      "noraiseonresize",   KEYWORD F_NORAISEONRESIZE;
      "noraiseonwarp",      KEYWORD F_NORAISEONWARP;
      "north",         DKEYWORD F_NORTH;
      "nosaveunders",      KEYWORD F_NOSAVEUNDERS;
      "nostackmode",      NO_STACKMODE;
      "notitle",      NO_TITLE;
      "notitlefocus",      KEYWORD F_NOTITLEFOCUS;
      "notitlehighlight",   NO_TITLE_HILITE;
      "noversion",      KEYWORD F_NOVERSION;
      "opaquemove",      KEYWORD F_OPAQUEMOVE;
      "pannerbackground", CKEYWORD F_PANNERBACKGROUND;
      "pannerforeground", CKEYWORD F_PANNERFOREGROUND;
      "pixmaps",      PIXMAPS;
      "pointerbackground",   CKEYWORD F_POINTERBACKGROUND;
      "pointerforeground",   CKEYWORD F_POINTERFOREGROUND;
      "priority",      NKEYWORD F_NPRIORITY;
      "r",         ROOT;
      "randomplacement",   KEYWORD F_RANDOMPLACEMENT;
      "resize",         RESIZE;
      "resizefont",      SKEYWORD F_RESIZEFONT;
      "restartpreviousstate",   KEYWORD F_RESTARTPREVIOUSSTATE;
      "right",         JKEYWORD F_RIGHT;
      "righttitlebutton",   RIGHT_TITLEBUTTON;
      "root",         ROOT;
      "s",         SHIFT;
      "savecolor",              SAVECOLOR;
      "select",         SELECT;
      "shift",         SHIFT;
      "showiconmanager",   KEYWORD F_SHOWICONMANAGER;
      "showvirtualnames", KEYWORD F_SHOWVIRTUALNAMES; (* tvtwm *)
      "sorticonmanager",   KEYWORD F_SORTICONMANAGER;
      "south",         DKEYWORD F_SOUTH;
      "squeezetitle",      SQUEEZE_TITLE;
      "starticonified",      START_ICONIFIED;
      "sticky",          STICKY; (* tvtwm *)
      "t",         TITLE;
      "title",         TITLE;
      "titlebackground",   CLKEYWORD F_TITLEBACKGROUND;
      "titlebuttonborderwidth",   NKEYWORD F_TITLEBUTTONBORDERWIDTH;
      "titlefont",      SKEYWORD F_TITLEFONT;
      "titleforeground",   CLKEYWORD F_TITLEFOREGROUND;
      "titlehighlight",      TITLE_HILITE;
      "titlepadding",      NKEYWORD F_TITLEPADDING;
      "unknownicon",      SKEYWORD F_UNKNOWNICON;
      "usepposition",      SKEYWORD F_USEPPOSITION;
      "virtualforeground",  CKEYWORD F_VIRTUALFOREGROUND; (* tvtwm *)
      "virtualbackground", CKEYWORD F_VIRTUALBACKGROUND; (* tvtwm *)
      "virtualdesktop",   SKEYWORD F_VIRTUALDESKTOP; (* tvtwm *)
      "w",         WINDOW;
      "wait",         WAIT;
      "warpcursor",      WARP_CURSOR;
      "warpunmapped",      KEYWORD F_WARPUNMAPPED;
      "west",         DKEYWORD F_WEST;
      "window",         WINDOW;
      "windowfunction",      WINDOW_FUNCTION;
      "windowring",      WINDOW_RING;
      "xorvalue",      NKEYWORD F_XORVALUE;
      "zoom",         ZOOM;
    ]
  
  let keytable = Hashtbl.create 53
  let _ = List.iter (fun (kw, cle) ->
        Hashtbl.add keytable kw cle) keywords

}

let blank = [' ' '\t']+
let string = '"' [^ '"'] * '"'
let number = [ '0'-'9']+

rule twmrc = parse
    blank          { twmrc lexbuf }
  | "{"            { LB }
  | "}"            { RB }
  | "("            { LP }
  | ")"            { RP }
  | "="            { EQUALS }
  | ":"            { COLON }
  | "+"            { PLUS }
  | "-"            { MINUS }
  | "|"            { OR }
  |  ['a'-'z' 'A'-'Z' '.']+    { 
      let keyword = Lexing.lexeme lexbuf in
      let keyword = String.lowercase keyword in
      try
        Hashtbl.find keytable keyword
      with
        _ ->             let pos = Lexing.lexeme_start lexbuf in
          Printf.printf "Position: %d" pos; print_newline ();
          ERRORTOKEN }      
  | "!"             { FSKEYWORD F_EXEC }
  | "^"             { FSKEYWORD F_CUT; }  
  | string          { STRING (let s = Lexing.lexeme lexbuf in 
        String.sub s 1 (String.length s - 2)) }
  | number          { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | '#' [^'\n']*'\n'      { twmrc lexbuf }
  | ['\n' '\t' ]         { twmrc lexbuf }
  | eof             { EOF }
  | _               {             let pos = Lexing.lexeme_start lexbuf in
      Printf.printf "Position: %d" pos; print_newline ();
      ERRORTOKEN }
    
    