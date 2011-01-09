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

type keyword =
  F_AUTORELATIVERESIZE
| F_CLIENTBORDERWIDTH
| F_DECORATETRANSIENTS
| F_DONTMOVEOFF
| F_FORCEICONS
| F_INTERPOLATEMENUCOLORS
| F_NOBACKINGSTORE
| F_NOCASESENSITIVE
| F_NODEFAULTS
| F_NOGRABSERVER
| F_NOICONMANAGERS
| F_NOMENUSHADOWS
| F_NORAISEONWARP
| F_NORAISEONRESIZE
| F_NORAISEONMOVE
| F_NORAISEONDEICONIFY
| F_NOSAVEUNDERS
| F_NOTITLEFOCUS
| F_NOVERSION
| F_OPAQUEMOVE
| F_RANDOMPLACEMENT
| F_RESTARTPREVIOUSSTATE
| F_SHOWICONMANAGER
| F_SORTICONMANAGER
| F_WARPUNMAPPED
| F_SHOWVIRTUALNAMES (* Tvtwm *)
  
type clkeyword =
  F_BORDERCOLOR
| F_BORDERTILEBACKGROUND
| F_BORDERTILEFOREGROUND
| F_ICONBACKGROUND
| F_ICONBORDERCOLOR
| F_ICONFOREGROUND
| F_ICONMANAGERFOREGROUND
| F_ICONMANAGERBACKGROUND
| F_ICONMANAGERHIGHLIGHT
| F_TITLEFOREGROUND
| F_TITLEBACKGROUND
| F_DEFAULTBACKGROUND
| F_DEFAULTFOREGROUND
| F_MENUBACKGROUND
| F_MENUFOREGROUND
| F_MENUSHADOWCOLOR
| F_MENUTITLEBACKGROUND
| F_MENUTITLEFOREGROUND
| F_POINTERBACKGROUND
| F_POINTERFOREGROUND
| F_PANNERBACKGROUND
| F_PANNERFOREGROUND
| F_VIRTUALFOREGROUND
| F_VIRTUALBACKGROUND
  
type nkeyword =
  F_BORDERWIDTH
| F_BUTTONINDENT
| F_CONSTRAINEDMOVETIME
| F_FRAMEPADDING
| F_ICONBORDERWIDTH
| F_MOVEDELTA
| F_NPRIORITY
| F_TITLEBUTTONBORDERWIDTH
| F_TITLEPADDING
| F_XORVALUE
  
type jkeyword =
  F_CENTER
| F_LEFT
| F_RIGHT
  
type fkeyword =
| F_AUTORAISE
| F_BACKICONMGR
| F_BEEP
| F_BOTTOMZOOM
| F_CIRCLEDOWN
| F_CIRCLEUP
| F_CUTFILE
| F_DEICONIFY
| F_DELETE
| F_DELTASTOP
| F_DESTROY
| F_DOWNICONMGR
| F_FOCUS
| F_FORCEMOVE
| F_FORWICONMGR
| F_FULLZOOM
| F_HBZOOM
| F_HIDEICONMGR
| F_HORIZOOM
| F_HTZOOM
| F_HZOOM
| F_ICONIFY
| F_IDENTIFY
| F_LEFTICONMGR
| F_LEFTZOOM
| F_LOWER
| F_MOVE
| F_NEXTICONMGR
| F_NOP
| F_PREVICONMGR
| F_QUIT
| F_RAISE
| F_RAISELOWER
| F_REFRESH
| F_RESIZE
| F_RESTART
| F_RIGHTICONMGR
| F_RIGHTZOOM
| F_SAVEYOURSELF
| F_SHOWICONMGR
| F_SORTICONMGR
| F_TITLE
| F_TOPZOOM
| F_TWMRC
| F_UNFOCUS
| F_UPICONMGR
| F_VERSION
| F_VLZOOM
| F_VRZOOM
| F_WINREFRESH
| F_ZOOM
| F_SCROLLUP
| F_SCROLLDOWN
| F_SCROLLRIGHT
| F_SCROLLLEFT
| F_SCROLLHOME

type dkeyword =
| F_EAST
| F_NORTH
| F_SOUTH
| F_WEST
  

  
type skeyword =
| F_ICONDIRECTORY
| F_ICONFONT
| F_ICONMANAGERFONT
| F_MAXWINDOWSIZE
| F_MENUFONT
| F_RESIZEFONT
| F_TITLEFONT
| F_UNKNOWNICON
| F_USEPPOSITION
| F_VIRTUALDESKTOP
  
type fskeyword =
| F_COLORMAP
| F_CUT
| F_EXEC
| F_FILE
| F_FUNCTION
| F_MENU
| F_PRIORITY
| F_SOURCE
| F_WARPRING
| F_WARPTO
| F_WARPTOICONMGR
| F_WARPTOSCREEN

type t
type win_list  = string list
type action =
  Action of fkeyword
| ActionString of fskeyword * string
  
type color_list = (clkeyword * string * (string * string) list) list
type iconm = string * string option * string * int
type menu = string * action * string option * string option
type fonction = action list
type save_color = SaveColor of string | SaveColorVar of clkeyword
  
type context = 
| C_WINDOW
| C_TITLE
| C_ICON
| C_ROOT
| C_FRAME
| C_ICONMGR
| C_ALL
| C_NAME of string

type cursor_of =
  FrameCursor
| TitleCursor
| IconCursor
| IconMgrCursor
| ButtonCursor
| MoveCursor
| ResizeCursor
| WaitCursor
| MenuCursor
| SelectCursor
| DestroyCursor
  
  
type cursor =
  NewFontCursor of cursor_of * string
| NewBitmapCursor of cursor_of * string * string
  
type twm = 
  Error
| NoArg of keyword
| StringArg of skeyword * string
| NumberArg of nkeyword * int
| AddIconRegion of string * dkeyword * dkeyword * int * int
| IconMgrGeometry of string * int option
| ZoomCount of int
| Pixmap_list of string list
| Cursor_list of cursor list
| IconifyByUnmapping of win_list
| IconifyByUnmappingAll
| TitleButton of string * action * bool
| ButtonMenu of int * string
| ButtonAction of int * action
| Key of string * int * context list * action
| Button of int * int * context list * action
| DontIconify of win_list
| IconManagerNoShow of win_list
| IconManagerNoShowAll
| Sticky of win_list
| IconManagerShow of win_list
| IconMgrs of iconm list
| NoTitleHighlight of win_list
| NoTitleHighlightAll
| NoHighlight of win_list
| NoStackMode of win_list
| NoTitlebar of win_list
| NoHighlightAll
| NoStackModeAll
| NoTitlebarAll
| MakeTitle of win_list
| StartIconified of win_list
| AutoRaise of win_list
| RootMenu of string * string option * string option * (menu list)
| RootFunction of string * fonction
| IconNames of (string * string) list
| ColorList of color_list
| GrayscaleList of color_list
| Monochrome of color_list
| DefaultFunction of action
| WindowFunction of action
| WarpCursorList of win_list
| WarpCursorAll
| WindowRingList of win_list
| SaveColorList of save_color list
| SqueezeTitleList of (string * jkeyword * int * int) list
| SqueezeTitleAll
| DontSqueezeTitleList of win_list
| DontSqueezeTitleAll
  