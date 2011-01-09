(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* ce fichier provient de X11/keysymdef.h 
   les noms des symboles deviennent : XK_ -> xk_

*)

let xk_VoidSymbol		= 0xFFFFFF	(* void symbol *)


(*
 * TTY Functions, cleverly chosen to map to ascii, for convenience of
 * programming, but could have been arbitrary (at the cost of lookup
 * tables in client code.
 *)

let xk_BackSpace		= 0xFF08	(* back space, back char *)
let xk_Tab			= 0xFF09
let xk_Linefeed		= 0xFF0A	(* Linefeed, LF *)
let xk_Clear		= 0xFF0B
let xk_Return		= 0xFF0D	(* Return, enter *)
let xk_Pause		= 0xFF13	(* Pause, hold *)
let xk_Scroll_Lock		= 0xFF14
let xk_Sys_Req		= 0xFF15
let xk_Escape		= 0xFF1B
let xk_Delete		= 0xFFFF	(* Delete, rubout *)



(* International & multi-key character composition *)

let xk_Multi_key		= 0xFF20  (* Multi-key character compose *)

(* Japanese keyboard support *)

let xk_Kanji		= 0xFF21	(* Kanji, Kanji convert *)
let xk_Muhenkan		= 0xFF22  (* Cancel Conversion *)
let xk_Henkan_Mode		= 0xFF23  (* Start/Stop Conversion *)
let xk_Henkan		= 0xFF23  (* Alias for Henkan_Mode *)
let xk_Romaji		= 0xFF24  (* to Romaji *)
let xk_Hiragana		= 0xFF25  (* to Hiragana *)
let xk_Katakana		= 0xFF26  (* to Katakana *)
let xk_Hiragana_Katakana	= 0xFF27  (* Hiragana/Katakana toggle *)
let xk_Zenkaku		= 0xFF28  (* to Zenkaku *)
let xk_Hankaku		= 0xFF29  (* to Hankaku *)
let xk_Zenkaku_Hankaku	= 0xFF2A  (* Zenkaku/Hankaku toggle *)
let xk_Touroku		= 0xFF2B  (* Add to Dictionary *)
let xk_Massyo		= 0xFF2C  (* Delete from Dictionary *)
let xk_Kana_Lock		= 0xFF2D  (* Kana Lock *)
let xk_Kana_Shift		= 0xFF2E  (* Kana Shift *)
let xk_Eisu_Shift		= 0xFF2F  (* Alphanumeric Shift *)
let xk_Eisu_toggle		= 0xFF30  (* Alphanumeric toggle *)

(* = 0xFF31 thru = 0xFF3F are under xk_KOREAN *)

(* Cursor control & motion *)

let xk_Home			= 0xFF50
let xk_Left			= 0xFF51	(* Move left, left arrow *)
let xk_Up			= 0xFF52	(* Move up, up arrow *)
let xk_Right		= 0xFF53	(* Move right, right arrow *)
let xk_Down			= 0xFF54	(* Move down, down arrow *)
let xk_Prior		= 0xFF55	(* Prior, previous *)
let xk_Page_Up		= 0xFF55
let xk_Next			= 0xFF56	(* Next *)
let xk_Page_Down		= 0xFF56
let xk_End			= 0xFF57	(* EOL *)
let xk_Begin		= 0xFF58	(* BOL *)


(* Misc Functions *)

let xk_Select		= 0xFF60	(* Select, mark *)
let xk_Print		= 0xFF61
let xk_Execute		= 0xFF62	(* Execute, run, do *)
let xk_Insert		= 0xFF63	(* Insert, insert here *)
let xk_Undo			= 0xFF65	(* Undo, oops *)
let xk_Redo			= 0xFF66	(* redo, again *)
let xk_Menu			= 0xFF67
let xk_Find			= 0xFF68	(* Find, search *)
let xk_Cancel		= 0xFF69	(* Cancel, stop, abort, exit *)
let xk_Help			= 0xFF6A	(* Help *)
let xk_Break		= 0xFF6B
let xk_Mode_switch		= 0xFF7E	(* Character set switch *)
let xk_script_switch        = 0xFF7E  (* Alias for mode_switch *)
let xk_Num_Lock		= 0xFF7F

(* Keypad Functions, keypad numbers cleverly chosen to map to ascii *)

let xk_KP_Space		= 0xFF80	(* space *)
let xk_KP_Tab		= 0xFF89
let xk_KP_Enter		= 0xFF8D	(* enter *)
let xk_KP_F1		= 0xFF91	(* PF1, KP_A, ... *)
let xk_KP_F2		= 0xFF92
let xk_KP_F3		= 0xFF93
let xk_KP_F4		= 0xFF94
let xk_KP_Home		= 0xFF95
let xk_KP_Left		= 0xFF96
let xk_KP_Up		= 0xFF97
let xk_KP_Right		= 0xFF98
let xk_KP_Down		= 0xFF99
let xk_KP_Prior		= 0xFF9A
let xk_KP_Page_Up		= 0xFF9A
let xk_KP_Next		= 0xFF9B
let xk_KP_Page_Down		= 0xFF9B
let xk_KP_End		= 0xFF9C
let xk_KP_Begin		= 0xFF9D
let xk_KP_Insert		= 0xFF9E
let xk_KP_Delete		= 0xFF9F
let xk_KP_Equal		= 0xFFBD	(* equals *)
let xk_KP_Multiply		= 0xFFAA
let xk_KP_Add		= 0xFFAB
let xk_KP_Separator		= 0xFFAC	(* separator, often comma *)
let xk_KP_Subtract		= 0xFFAD
let xk_KP_Decimal		= 0xFFAE
let xk_KP_Divide		= 0xFFAF

let xk_KP_0			= 0xFFB0
let xk_KP_1			= 0xFFB1
let xk_KP_2			= 0xFFB2
let xk_KP_3			= 0xFFB3
let xk_KP_4			= 0xFFB4
let xk_KP_5			= 0xFFB5
let xk_KP_6			= 0xFFB6
let xk_KP_7			= 0xFFB7
let xk_KP_8			= 0xFFB8
let xk_KP_9			= 0xFFB9



(*
 * Auxilliary Functions; note the duplicate definitions for left and right
 * function keys;  Sun keyboards and a few other manufactures have such
 * function key groups on the left and/or right sides of the keyboard.
 * We've not found a keyboard with more than 35 function keys total.
 *)

let xk_F1			= 0xFFBE
let xk_F2			= 0xFFBF
let xk_F3			= 0xFFC0
let xk_F4			= 0xFFC1
let xk_F5			= 0xFFC2
let xk_F6			= 0xFFC3
let xk_F7			= 0xFFC4
let xk_F8			= 0xFFC5
let xk_F9			= 0xFFC6
let xk_F10			= 0xFFC7
let xk_F11			= 0xFFC8
let xk_L1			= 0xFFC8
let xk_F12			= 0xFFC9
let xk_L2			= 0xFFC9
let xk_F13			= 0xFFCA
let xk_L3			= 0xFFCA
let xk_F14			= 0xFFCB
let xk_L4			= 0xFFCB
let xk_F15			= 0xFFCC
let xk_L5			= 0xFFCC
let xk_F16			= 0xFFCD
let xk_L6			= 0xFFCD
let xk_F17			= 0xFFCE
let xk_L7			= 0xFFCE
let xk_F18			= 0xFFCF
let xk_L8			= 0xFFCF
let xk_F19			= 0xFFD0
let xk_L9			= 0xFFD0
let xk_F20			= 0xFFD1
let xk_L10			= 0xFFD1
let xk_F21			= 0xFFD2
let xk_R1			= 0xFFD2
let xk_F22			= 0xFFD3
let xk_R2			= 0xFFD3
let xk_F23			= 0xFFD4
let xk_R3			= 0xFFD4
let xk_F24			= 0xFFD5
let xk_R4			= 0xFFD5
let xk_F25			= 0xFFD6
let xk_R5			= 0xFFD6
let xk_F26			= 0xFFD7
let xk_R6			= 0xFFD7
let xk_F27			= 0xFFD8
let xk_R7			= 0xFFD8
let xk_F28			= 0xFFD9
let xk_R8			= 0xFFD9
let xk_F29			= 0xFFDA
let xk_R9			= 0xFFDA
let xk_F30			= 0xFFDB
let xk_R10			= 0xFFDB
let xk_F31			= 0xFFDC
let xk_R11			= 0xFFDC
let xk_F32			= 0xFFDD
let xk_R12			= 0xFFDD
let xk_F33			= 0xFFDE
let xk_R13			= 0xFFDE
let xk_F34			= 0xFFDF
let xk_R14			= 0xFFDF
let xk_F35			= 0xFFE0
let xk_R15			= 0xFFE0

(* Modifiers *)

let xk_Shift_L		= 0xFFE1	(* Left shift *)
let xk_Shift_R		= 0xFFE2	(* Right shift *)
let xk_Control_L		= 0xFFE3	(* Left control *)
let xk_Control_R		= 0xFFE4	(* Right control *)
let xk_Caps_Lock		= 0xFFE5	(* Caps lock *)
let xk_Shift_Lock		= 0xFFE6	(* Shift lock *)

let xk_Meta_L		= 0xFFE7	(* Left meta *)
let xk_Meta_R		= 0xFFE8	(* Right meta *)
let xk_Alt_L		= 0xFFE9	(* Left alt *)
let xk_Alt_R		= 0xFFEA	(* Right alt *)
let xk_Super_L		= 0xFFEB	(* Left super *)
let xk_Super_R		= 0xFFEC	(* Right super *)
let xk_Hyper_L		= 0xFFED	(* Left hyper *)
let xk_Hyper_R		= 0xFFEE	(* Right hyper *)


(*
 * ISO 9995 Function and Modifier Keys
 * Byte 3 = = 0xFE
 *)


let	xk_ISO_Lock					= 0xFE01
let	xk_ISO_Level2_Latch				= 0xFE02
let	xk_ISO_Level3_Shift				= 0xFE03
let	xk_ISO_Level3_Latch				= 0xFE04
let	xk_ISO_Level3_Lock				= 0xFE05
let	xk_ISO_Group_Shift		= 0xFF7E	(* Alias for mode_switch *)
let	xk_ISO_Group_Latch				= 0xFE06
let	xk_ISO_Group_Lock				= 0xFE07
let	xk_ISO_Next_Group				= 0xFE08
let	xk_ISO_Next_Group_Lock				= 0xFE09
let	xk_ISO_Prev_Group				= 0xFE0A
let	xk_ISO_Prev_Group_Lock				= 0xFE0B
let	xk_ISO_First_Group				= 0xFE0C
let	xk_ISO_First_Group_Lock				= 0xFE0D
let	xk_ISO_Last_Group				= 0xFE0E
let	xk_ISO_Last_Group_Lock				= 0xFE0F

let	xk_ISO_Left_Tab					= 0xFE20
let	xk_ISO_Move_Line_Up				= 0xFE21
let	xk_ISO_Move_Line_Down				= 0xFE22
let	xk_ISO_Partial_Line_Up				= 0xFE23
let	xk_ISO_Partial_Line_Down			= 0xFE24
let	xk_ISO_Partial_Space_Left			= 0xFE25
let	xk_ISO_Partial_Space_Right			= 0xFE26
let	xk_ISO_Set_Margin_Left				= 0xFE27
let	xk_ISO_Set_Margin_Right				= 0xFE28
let	xk_ISO_Release_Margin_Left			= 0xFE29
let	xk_ISO_Release_Margin_Right			= 0xFE2A
let	xk_ISO_Release_Both_Margins			= 0xFE2B
let	xk_ISO_Fast_Cursor_Left				= 0xFE2C
let	xk_ISO_Fast_Cursor_Right			= 0xFE2D
let	xk_ISO_Fast_Cursor_Up				= 0xFE2E
let	xk_ISO_Fast_Cursor_Down				= 0xFE2F
let	xk_ISO_Continuous_Underline			= 0xFE30
let	xk_ISO_Discontinuous_Underline			= 0xFE31
let	xk_ISO_Emphasize				= 0xFE32
let	xk_ISO_Center_Object				= 0xFE33
let	xk_ISO_Enter					= 0xFE34

let	xk_dead_grave					= 0xFE50
let	xk_dead_acute					= 0xFE51
let	xk_dead_circumflex				= 0xFE52
let	xk_dead_tilde					= 0xFE53
let	xk_dead_macron					= 0xFE54
let	xk_dead_breve					= 0xFE55
let	xk_dead_abovedot				= 0xFE56
let	xk_dead_diaeresis				= 0xFE57
let	xk_dead_abovering				= 0xFE58
let	xk_dead_doubleacute				= 0xFE59
let	xk_dead_caron					= 0xFE5A
let	xk_dead_cedilla					= 0xFE5B
let	xk_dead_ogonek					= 0xFE5C
let	xk_dead_iota					= 0xFE5D
let	xk_dead_voiced_sound				= 0xFE5E
let	xk_dead_semivoiced_sound			= 0xFE5F
let	xk_dead_belowdot				= 0xFE60

let	xk_First_Virtual_Screen				= 0xFED0
let	xk_Prev_Virtual_Screen				= 0xFED1
let	xk_Next_Virtual_Screen				= 0xFED2
let	xk_Last_Virtual_Screen				= 0xFED4
let	xk_Terminate_Server				= 0xFED5

let	xk_AccessX_Enable				= 0xFE70
let	xk_AccessX_Feedback_Enable			= 0xFE71
let	xk_RepeatKeys_Enable				= 0xFE72
let	xk_SlowKeys_Enable				= 0xFE73
let	xk_BounceKeys_Enable				= 0xFE74
let	xk_StickyKeys_Enable				= 0xFE75
let	xk_MouseKeys_Enable				= 0xFE76
let	xk_MouseKeys_Accel_Enable			= 0xFE77
let	xk_Overlay1_Enable				= 0xFE78
let	xk_Overlay2_Enable				= 0xFE79
let	xk_AudibleBell_Enable				= 0xFE7A

let	xk_Pointer_Left					= 0xFEE0
let	xk_Pointer_Right				= 0xFEE1
let	xk_Pointer_Up					= 0xFEE2
let	xk_Pointer_Down					= 0xFEE3
let	xk_Pointer_UpLeft				= 0xFEE4
let	xk_Pointer_UpRight				= 0xFEE5
let	xk_Pointer_DownLeft				= 0xFEE6
let	xk_Pointer_DownRight				= 0xFEE7
let	xk_Pointer_Button_Dflt				= 0xFEE8
let	xk_Pointer_Button1				= 0xFEE9
let	xk_Pointer_Button2				= 0xFEEA
let	xk_Pointer_Button3				= 0xFEEB
let	xk_Pointer_Button4				= 0xFEEC
let	xk_Pointer_Button5				= 0xFEED
let	xk_Pointer_DblClick_Dflt			= 0xFEEE
let	xk_Pointer_DblClick1				= 0xFEEF
let	xk_Pointer_DblClick2				= 0xFEF0
let	xk_Pointer_DblClick3				= 0xFEF1
let	xk_Pointer_DblClick4				= 0xFEF2
let	xk_Pointer_DblClick5				= 0xFEF3
let	xk_Pointer_Drag_Dflt				= 0xFEF4
let	xk_Pointer_Drag1				= 0xFEF5
let	xk_Pointer_Drag2				= 0xFEF6
let	xk_Pointer_Drag3				= 0xFEF7
let	xk_Pointer_Drag4				= 0xFEF8
let	xk_Pointer_Drag5				= 0xFEFD

let	xk_Pointer_EnableKeys				= 0xFEF9
let	xk_Pointer_Accelerate				= 0xFEFA
let	xk_Pointer_DfltBtnNext				= 0xFEFB
let	xk_Pointer_DfltBtnPrev				= 0xFEFC



(*
 * 3270 Terminal Keys
 * Byte 3 = = 0xFD
 *)


let xk_3270_Duplicate      = 0xFD01
let xk_3270_FieldMark      = 0xFD02
let xk_3270_Right2         = 0xFD03
let xk_3270_Left2          = 0xFD04
let xk_3270_BackTab        = 0xFD05
let xk_3270_EraseEOF       = 0xFD06
let xk_3270_EraseInput     = 0xFD07
let xk_3270_Reset          = 0xFD08
let xk_3270_Quit           = 0xFD09
let xk_3270_PA1            = 0xFD0A
let xk_3270_PA2            = 0xFD0B
let xk_3270_PA3            = 0xFD0C
let xk_3270_Test           = 0xFD0D
let xk_3270_Attn           = 0xFD0E
let xk_3270_CursorBlink    = 0xFD0F
let xk_3270_AltCursor      = 0xFD10
let xk_3270_KeyClick       = 0xFD11
let xk_3270_Jump           = 0xFD12
let xk_3270_Ident          = 0xFD13
let xk_3270_Rule           = 0xFD14
let xk_3270_Copy           = 0xFD15
let xk_3270_Play           = 0xFD16
let xk_3270_Setup          = 0xFD17
let xk_3270_Record         = 0xFD18
let xk_3270_ChangeScreen   = 0xFD19
let xk_3270_DeleteWord     = 0xFD1A
let xk_3270_ExSelect       = 0xFD1B
let xk_3270_CursorSelect   = 0xFD1C
let xk_3270_PrintScreen    = 0xFD1D
let xk_3270_Enter          = 0xFD1E


(*
 *  Latin 1
 *  Byte 3 = 0
 *)

let xk_space               = 0x020
let xk_exclam              = 0x021
let xk_quotedbl            = 0x022
let xk_numbersign          = 0x023
let xk_dollar              = 0x024
let xk_percent             = 0x025
let xk_ampersand           = 0x026
let xk_apostrophe          = 0x027
let xk_quoteright          = 0x027	(* deprecated *)
let xk_parenleft           = 0x028
let xk_parenright          = 0x029
let xk_asterisk            = 0x02a
let xk_plus                = 0x02b
let xk_comma               = 0x02c
let xk_minus               = 0x02d
let xk_period              = 0x02e
let xk_slash               = 0x02f
let xk_0                   = 0x030
let xk_1                   = 0x031
let xk_2                   = 0x032
let xk_3                   = 0x033
let xk_4                   = 0x034
let xk_5                   = 0x035
let xk_6                   = 0x036
let xk_7                   = 0x037
let xk_8                   = 0x038
let xk_9                   = 0x039
let xk_colon               = 0x03a
let xk_semicolon           = 0x03b
let xk_less                = 0x03c
let xk_equal               = 0x03d
let xk_greater             = 0x03e
let xk_question            = 0x03f
let xk_at                  = 0x040
let xk_A                   = 0x041
let xk_B                   = 0x042
let xk_C                   = 0x043
let xk_D                   = 0x044
let xk_E                   = 0x045
let xk_F                   = 0x046
let xk_G                   = 0x047
let xk_H                   = 0x048
let xk_I                   = 0x049
let xk_J                   = 0x04a
let xk_K                   = 0x04b
let xk_L                   = 0x04c
let xk_M                   = 0x04d
let xk_N                   = 0x04e
let xk_O                   = 0x04f
let xk_P                   = 0x050
let xk_Q                   = 0x051
let xk_R                   = 0x052
let xk_S                   = 0x053
let xk_T                   = 0x054
let xk_U                   = 0x055
let xk_V                   = 0x056
let xk_W                   = 0x057
let xk_X                   = 0x058
let xk_Y                   = 0x059
let xk_Z                   = 0x05a
let xk_bracketleft         = 0x05b
let xk_backslash           = 0x05c
let xk_bracketright        = 0x05d
let xk_asciicircum         = 0x05e
let xk_underscore          = 0x05f
let xk_grave               = 0x060
let xk_quoteleft           = 0x060	(* deprecated *)
let xk_a                   = 0x061
let xk_b                   = 0x062
let xk_c                   = 0x063
let xk_d                   = 0x064
let xk_e                   = 0x065
let xk_f                   = 0x066
let xk_g                   = 0x067
let xk_h                   = 0x068
let xk_i                   = 0x069
let xk_j                   = 0x06a
let xk_k                   = 0x06b
let xk_l                   = 0x06c
let xk_m                   = 0x06d
let xk_n                   = 0x06e
let xk_o                   = 0x06f
let xk_p                   = 0x070
let xk_q                   = 0x071
let xk_r                   = 0x072
let xk_s                   = 0x073
let xk_t                   = 0x074
let xk_u                   = 0x075
let xk_v                   = 0x076
let xk_w                   = 0x077
let xk_x                   = 0x078
let xk_y                   = 0x079
let xk_z                   = 0x07a
let xk_braceleft           = 0x07b
let xk_bar                 = 0x07c
let xk_braceright          = 0x07d
let xk_asciitilde          = 0x07e

let xk_nobreakspace        = 0x0a0
let xk_exclamdown          = 0x0a1
let xk_cent        	       = 0x0a2
let xk_sterling            = 0x0a3
let xk_currency            = 0x0a4
let xk_yen                 = 0x0a5
let xk_brokenbar           = 0x0a6
let xk_section             = 0x0a7
let xk_diaeresis           = 0x0a8
let xk_copyright           = 0x0a9
let xk_ordfeminine         = 0x0aa
let xk_guillemotleft       = 0x0ab	(* left angle quotation mark *)
let xk_notsign             = 0x0ac
let xk_hyphen              = 0x0ad
let xk_registered          = 0x0ae
let xk_macron              = 0x0af
let xk_degree              = 0x0b0
let xk_plusminus           = 0x0b1
let xk_twosuperior         = 0x0b2
let xk_threesuperior       = 0x0b3
let xk_acute               = 0x0b4
let xk_mu                  = 0x0b5
let xk_paragraph           = 0x0b6
let xk_periodcentered      = 0x0b7
let xk_cedilla             = 0x0b8
let xk_onesuperior         = 0x0b9
let xk_masculine           = 0x0ba
let xk_guillemotright      = 0x0bb	(* right angle quotation mark *)
let xk_onequarter          = 0x0bc
let xk_onehalf             = 0x0bd
let xk_threequarters       = 0x0be
let xk_questiondown        = 0x0bf
let xk_Agrave              = 0x0c0
let xk_Aacute              = 0x0c1
let xk_Acircumflex         = 0x0c2
let xk_Atilde              = 0x0c3
let xk_Adiaeresis          = 0x0c4
let xk_Aring               = 0x0c5
let xk_AE                  = 0x0c6
let xk_Ccedilla            = 0x0c7
let xk_Egrave              = 0x0c8
let xk_Eacute              = 0x0c9
let xk_Ecircumflex         = 0x0ca
let xk_Ediaeresis          = 0x0cb
let xk_Igrave              = 0x0cc
let xk_Iacute              = 0x0cd
let xk_Icircumflex         = 0x0ce
let xk_Idiaeresis          = 0x0cf
let xk_ETH                 = 0x0d0
let xk_Eth                 = 0x0d0	(* deprecated *)
let xk_Ntilde              = 0x0d1
let xk_Ograve              = 0x0d2
let xk_Oacute              = 0x0d3
let xk_Ocircumflex         = 0x0d4
let xk_Otilde              = 0x0d5
let xk_Odiaeresis          = 0x0d6
let xk_multiply            = 0x0d7
let xk_Ooblique            = 0x0d8
let xk_Ugrave              = 0x0d9
let xk_Uacute              = 0x0da
let xk_Ucircumflex         = 0x0db
let xk_Udiaeresis          = 0x0dc
let xk_Yacute              = 0x0dd
let xk_THORN               = 0x0de
let xk_Thorn               = 0x0de	(* deprecated *)
let xk_ssharp              = 0x0df
let xk_agrave              = 0x0e0
let xk_aacute              = 0x0e1
let xk_acircumflex         = 0x0e2
let xk_atilde              = 0x0e3
let xk_adiaeresis          = 0x0e4
let xk_aring               = 0x0e5
let xk_ae                  = 0x0e6
let xk_ccedilla            = 0x0e7
let xk_egrave              = 0x0e8
let xk_eacute              = 0x0e9
let xk_ecircumflex         = 0x0ea
let xk_ediaeresis          = 0x0eb
let xk_igrave              = 0x0ec
let xk_iacute              = 0x0ed
let xk_icircumflex         = 0x0ee
let xk_idiaeresis          = 0x0ef
let xk_eth                 = 0x0f0
let xk_ntilde              = 0x0f1
let xk_ograve              = 0x0f2
let xk_oacute              = 0x0f3
let xk_ocircumflex         = 0x0f4
let xk_otilde              = 0x0f5
let xk_odiaeresis          = 0x0f6
let xk_division            = 0x0f7
let xk_oslash              = 0x0f8
let xk_ugrave              = 0x0f9
let xk_uacute              = 0x0fa
let xk_ucircumflex         = 0x0fb
let xk_udiaeresis          = 0x0fc
let xk_yacute              = 0x0fd
let xk_thorn               = 0x0fe
let xk_ydiaeresis          = 0x0ff


(*
 *   Latin 2
 *   Byte 3 = 1
 *)


let xk_Aogonek             = 0x1a1
let xk_breve               = 0x1a2
let xk_Lstroke             = 0x1a3
let xk_Lcaron              = 0x1a5
let xk_Sacute              = 0x1a6
let xk_Scaron              = 0x1a9
let xk_Scedilla            = 0x1aa
let xk_Tcaron              = 0x1ab
let xk_Zacute              = 0x1ac
let xk_Zcaron              = 0x1ae
let xk_Zabovedot           = 0x1af
let xk_aogonek             = 0x1b1
let xk_ogonek              = 0x1b2
let xk_lstroke             = 0x1b3
let xk_lcaron              = 0x1b5
let xk_sacute              = 0x1b6
let xk_caron               = 0x1b7
let xk_scaron              = 0x1b9
let xk_scedilla            = 0x1ba
let xk_tcaron              = 0x1bb
let xk_zacute              = 0x1bc
let xk_doubleacute         = 0x1bd
let xk_zcaron              = 0x1be
let xk_zabovedot           = 0x1bf
let xk_Racute              = 0x1c0
let xk_Abreve              = 0x1c3
let xk_Lacute              = 0x1c5
let xk_Cacute              = 0x1c6
let xk_Ccaron              = 0x1c8
let xk_Eogonek             = 0x1ca
let xk_Ecaron              = 0x1cc
let xk_Dcaron              = 0x1cf
let xk_Dstroke             = 0x1d0
let xk_Nacute              = 0x1d1
let xk_Ncaron              = 0x1d2
let xk_Odoubleacute        = 0x1d5
let xk_Rcaron              = 0x1d8
let xk_Uring               = 0x1d9
let xk_Udoubleacute        = 0x1db
let xk_Tcedilla            = 0x1de
let xk_racute              = 0x1e0
let xk_abreve              = 0x1e3
let xk_lacute              = 0x1e5
let xk_cacute              = 0x1e6
let xk_ccaron              = 0x1e8
let xk_eogonek             = 0x1ea
let xk_ecaron              = 0x1ec
let xk_dcaron              = 0x1ef
let xk_dstroke             = 0x1f0
let xk_nacute              = 0x1f1
let xk_ncaron              = 0x1f2
let xk_odoubleacute        = 0x1f5
let xk_udoubleacute        = 0x1fb
let xk_rcaron              = 0x1f8
let xk_uring               = 0x1f9
let xk_tcedilla            = 0x1fe
let xk_abovedot            = 0x1ff


(*
 *   Latin 3
 *   Byte 3 = 2
 *)


let xk_Hstroke             = 0x2a1
let xk_Hcircumflex         = 0x2a6
let xk_Iabovedot           = 0x2a9
let xk_Gbreve              = 0x2ab
let xk_Jcircumflex         = 0x2ac
let xk_hstroke             = 0x2b1
let xk_hcircumflex         = 0x2b6
let xk_idotless            = 0x2b9
let xk_gbreve              = 0x2bb
let xk_jcircumflex         = 0x2bc
let xk_Cabovedot           = 0x2c5
let xk_Ccircumflex         = 0x2c6
let xk_Gabovedot           = 0x2d5
let xk_Gcircumflex         = 0x2d8
let xk_Ubreve              = 0x2dd
let xk_Scircumflex         = 0x2de
let xk_cabovedot           = 0x2e5
let xk_ccircumflex         = 0x2e6
let xk_gabovedot           = 0x2f5
let xk_gcircumflex         = 0x2f8
let xk_ubreve              = 0x2fd
let xk_scircumflex         = 0x2fe



(*
 *   Latin 4
 *   Byte 3 = 3
 *)


let xk_kra                 = 0x3a2
let xk_kappa               = 0x3a2	(* deprecated *)
let xk_Rcedilla            = 0x3a3
let xk_Itilde              = 0x3a5
let xk_Lcedilla            = 0x3a6
let xk_Emacron             = 0x3aa
let xk_Gcedilla            = 0x3ab
let xk_Tslash              = 0x3ac
let xk_rcedilla            = 0x3b3
let xk_itilde              = 0x3b5
let xk_lcedilla            = 0x3b6
let xk_emacron             = 0x3ba
let xk_gcedilla            = 0x3bb
let xk_tslash              = 0x3bc
let xk_ENG                 = 0x3bd
let xk_eng                 = 0x3bf
let xk_Amacron             = 0x3c0
let xk_Iogonek             = 0x3c7
let xk_Eabovedot           = 0x3cc
let xk_Imacron             = 0x3cf
let xk_Ncedilla            = 0x3d1
let xk_Omacron             = 0x3d2
let xk_Kcedilla            = 0x3d3
let xk_Uogonek             = 0x3d9
let xk_Utilde              = 0x3dd
let xk_Umacron             = 0x3de
let xk_amacron             = 0x3e0
let xk_iogonek             = 0x3e7
let xk_eabovedot           = 0x3ec
let xk_imacron             = 0x3ef
let xk_ncedilla            = 0x3f1
let xk_omacron             = 0x3f2
let xk_kcedilla            = 0x3f3
let xk_uogonek             = 0x3f9
let xk_utilde              = 0x3fd
let xk_umacron             = 0x3fe


(*
 * Katakana
 * Byte 3 = 4
 *)


let xk_overline				       = 0x47e
let xk_kana_fullstop                               = 0x4a1
let xk_kana_openingbracket                         = 0x4a2
let xk_kana_closingbracket                         = 0x4a3
let xk_kana_comma                                  = 0x4a4
let xk_kana_conjunctive                            = 0x4a5
let xk_kana_middledot                              = 0x4a5  (* deprecated *)
let xk_kana_WO                                     = 0x4a6
let xk_kana_a                                      = 0x4a7
let xk_kana_i                                      = 0x4a8
let xk_kana_u                                      = 0x4a9
let xk_kana_e                                      = 0x4aa
let xk_kana_o                                      = 0x4ab
let xk_kana_ya                                     = 0x4ac
let xk_kana_yu                                     = 0x4ad
let xk_kana_yo                                     = 0x4ae
let xk_kana_tsu                                    = 0x4af
let xk_kana_tu                                     = 0x4af  (* deprecated *)
let xk_prolongedsound                              = 0x4b0
let xk_kana_A                                      = 0x4b1
let xk_kana_I                                      = 0x4b2
let xk_kana_U                                      = 0x4b3
let xk_kana_E                                      = 0x4b4
let xk_kana_O                                      = 0x4b5
let xk_kana_KA                                     = 0x4b6
let xk_kana_KI                                     = 0x4b7
let xk_kana_KU                                     = 0x4b8
let xk_kana_KE                                     = 0x4b9
let xk_kana_KO                                     = 0x4ba
let xk_kana_SA                                     = 0x4bb
let xk_kana_SHI                                    = 0x4bc
let xk_kana_SU                                     = 0x4bd
let xk_kana_SE                                     = 0x4be
let xk_kana_SO                                     = 0x4bf
let xk_kana_TA                                     = 0x4c0
let xk_kana_CHI                                    = 0x4c1
let xk_kana_TI                                     = 0x4c1  (* deprecated *)
let xk_kana_TSU                                    = 0x4c2
let xk_kana_TU                                     = 0x4c2  (* deprecated *)
let xk_kana_TE                                     = 0x4c3
let xk_kana_TO                                     = 0x4c4
let xk_kana_NA                                     = 0x4c5
let xk_kana_NI                                     = 0x4c6
let xk_kana_NU                                     = 0x4c7
let xk_kana_NE                                     = 0x4c8
let xk_kana_NO                                     = 0x4c9
let xk_kana_HA                                     = 0x4ca
let xk_kana_HI                                     = 0x4cb
let xk_kana_FU                                     = 0x4cc
let xk_kana_HU                                     = 0x4cc  (* deprecated *)
let xk_kana_HE                                     = 0x4cd
let xk_kana_HO                                     = 0x4ce
let xk_kana_MA                                     = 0x4cf
let xk_kana_MI                                     = 0x4d0
let xk_kana_MU                                     = 0x4d1
let xk_kana_ME                                     = 0x4d2
let xk_kana_MO                                     = 0x4d3
let xk_kana_YA                                     = 0x4d4
let xk_kana_YU                                     = 0x4d5
let xk_kana_YO                                     = 0x4d6
let xk_kana_RA                                     = 0x4d7
let xk_kana_RI                                     = 0x4d8
let xk_kana_RU                                     = 0x4d9
let xk_kana_RE                                     = 0x4da
let xk_kana_RO                                     = 0x4db
let xk_kana_WA                                     = 0x4dc
let xk_kana_N                                      = 0x4dd
let xk_voicedsound                                 = 0x4de
let xk_semivoicedsound                             = 0x4df
let xk_kana_switch          = 0xFF7E  (* Alias for mode_switch *)


(*
 *  Arabic
 *  Byte 3 = 5
 *)


let xk_Arabic_comma                                = 0x5ac
let xk_Arabic_semicolon                            = 0x5bb
let xk_Arabic_question_mark                        = 0x5bf
let xk_Arabic_hamza                                = 0x5c1
let xk_Arabic_maddaonalef                          = 0x5c2
let xk_Arabic_hamzaonalef                          = 0x5c3
let xk_Arabic_hamzaonwaw                           = 0x5c4
let xk_Arabic_hamzaunderalef                       = 0x5c5
let xk_Arabic_hamzaonyeh                           = 0x5c6
let xk_Arabic_alef                                 = 0x5c7
let xk_Arabic_beh                                  = 0x5c8
let xk_Arabic_tehmarbuta                           = 0x5c9
let xk_Arabic_teh                                  = 0x5ca
let xk_Arabic_theh                                 = 0x5cb
let xk_Arabic_jeem                                 = 0x5cc
let xk_Arabic_hah                                  = 0x5cd
let xk_Arabic_khah                                 = 0x5ce
let xk_Arabic_dal                                  = 0x5cf
let xk_Arabic_thal                                 = 0x5d0
let xk_Arabic_ra                                   = 0x5d1
let xk_Arabic_zain                                 = 0x5d2
let xk_Arabic_seen                                 = 0x5d3
let xk_Arabic_sheen                                = 0x5d4
let xk_Arabic_sad                                  = 0x5d5
let xk_Arabic_dad                                  = 0x5d6
let xk_Arabic_tah                                  = 0x5d7
let xk_Arabic_zah                                  = 0x5d8
let xk_Arabic_ain                                  = 0x5d9
let xk_Arabic_ghain                                = 0x5da
let xk_Arabic_tatweel                              = 0x5e0
let xk_Arabic_feh                                  = 0x5e1
let xk_Arabic_qaf                                  = 0x5e2
let xk_Arabic_kaf                                  = 0x5e3
let xk_Arabic_lam                                  = 0x5e4
let xk_Arabic_meem                                 = 0x5e5
let xk_Arabic_noon                                 = 0x5e6
let xk_Arabic_ha                                   = 0x5e7
let xk_Arabic_heh                                  = 0x5e7  (* deprecated *)
let xk_Arabic_waw                                  = 0x5e8
let xk_Arabic_alefmaksura                          = 0x5e9
let xk_Arabic_yeh                                  = 0x5ea
let xk_Arabic_fathatan                             = 0x5eb
let xk_Arabic_dammatan                             = 0x5ec
let xk_Arabic_kasratan                             = 0x5ed
let xk_Arabic_fatha                                = 0x5ee
let xk_Arabic_damma                                = 0x5ef
let xk_Arabic_kasra                                = 0x5f0
let xk_Arabic_shadda                               = 0x5f1
let xk_Arabic_sukun                                = 0x5f2
let xk_Arabic_switch        = 0xFF7E  (* Alias for mode_switch *)


(*
 * Cyrillic
 * Byte 3 = 6
 *)

let xk_Serbian_dje                                 = 0x6a1
let xk_Macedonia_gje                               = 0x6a2
let xk_Cyrillic_io                                 = 0x6a3
let xk_Ukrainian_ie                                = 0x6a4
let xk_Ukranian_je                                 = 0x6a4  (* deprecated *)
let xk_Macedonia_dse                               = 0x6a5
let xk_Ukrainian_i                                 = 0x6a6
let xk_Ukranian_i                                  = 0x6a6  (* deprecated *)
let xk_Ukrainian_yi                                = 0x6a7
let xk_Ukranian_yi                                 = 0x6a7  (* deprecated *)
let xk_Cyrillic_je                                 = 0x6a8
let xk_Serbian_je                                  = 0x6a8  (* deprecated *)
let xk_Cyrillic_lje                                = 0x6a9
let xk_Serbian_lje                                 = 0x6a9  (* deprecated *)
let xk_Cyrillic_nje                                = 0x6aa
let xk_Serbian_nje                                 = 0x6aa  (* deprecated *)
let xk_Serbian_tshe                                = 0x6ab
let xk_Macedonia_kje                               = 0x6ac
let xk_Byelorussian_shortu                         = 0x6ae
let xk_Cyrillic_dzhe                               = 0x6af
let xk_Serbian_dze                                 = 0x6af  (* deprecated *)
let xk_numerosign                                  = 0x6b0
let xk_Serbian_DJE                                 = 0x6b1
let xk_Macedonia_GJE                               = 0x6b2
let xk_Cyrillic_IO                                 = 0x6b3
let xk_Ukrainian_IE                                = 0x6b4
let xk_Ukranian_JE                                 = 0x6b4  (* deprecated *)
let xk_Macedonia_DSE                               = 0x6b5
let xk_Ukrainian_I                                 = 0x6b6
let xk_Ukranian_I                                  = 0x6b6  (* deprecated *)
let xk_Ukrainian_YI                                = 0x6b7
let xk_Ukranian_YI                                 = 0x6b7  (* deprecated *)
let xk_Cyrillic_JE                                 = 0x6b8
let xk_Serbian_JE                                  = 0x6b8  (* deprecated *)
let xk_Cyrillic_LJE                                = 0x6b9
let xk_Serbian_LJE                                 = 0x6b9  (* deprecated *)
let xk_Cyrillic_NJE                                = 0x6ba
let xk_Serbian_NJE                                 = 0x6ba  (* deprecated *)
let xk_Serbian_TSHE                                = 0x6bb
let xk_Macedonia_KJE                               = 0x6bc
let xk_Byelorussian_SHORTU                         = 0x6be
let xk_Cyrillic_DZHE                               = 0x6bf
let xk_Serbian_DZE                                 = 0x6bf  (* deprecated *)
let xk_Cyrillic_yu                                 = 0x6c0
let xk_Cyrillic_a                                  = 0x6c1
let xk_Cyrillic_be                                 = 0x6c2
let xk_Cyrillic_tse                                = 0x6c3
let xk_Cyrillic_de                                 = 0x6c4
let xk_Cyrillic_ie                                 = 0x6c5
let xk_Cyrillic_ef                                 = 0x6c6
let xk_Cyrillic_ghe                                = 0x6c7
let xk_Cyrillic_ha                                 = 0x6c8
let xk_Cyrillic_i                                  = 0x6c9
let xk_Cyrillic_shorti                             = 0x6ca
let xk_Cyrillic_ka                                 = 0x6cb
let xk_Cyrillic_el                                 = 0x6cc
let xk_Cyrillic_em                                 = 0x6cd
let xk_Cyrillic_en                                 = 0x6ce
let xk_Cyrillic_o                                  = 0x6cf
let xk_Cyrillic_pe                                 = 0x6d0
let xk_Cyrillic_ya                                 = 0x6d1
let xk_Cyrillic_er                                 = 0x6d2
let xk_Cyrillic_es                                 = 0x6d3
let xk_Cyrillic_te                                 = 0x6d4
let xk_Cyrillic_u                                  = 0x6d5
let xk_Cyrillic_zhe                                = 0x6d6
let xk_Cyrillic_ve                                 = 0x6d7
let xk_Cyrillic_softsign                           = 0x6d8
let xk_Cyrillic_yeru                               = 0x6d9
let xk_Cyrillic_ze                                 = 0x6da
let xk_Cyrillic_sha                                = 0x6db
let xk_Cyrillic_e                                  = 0x6dc
let xk_Cyrillic_shcha                              = 0x6dd
let xk_Cyrillic_che                                = 0x6de
let xk_Cyrillic_hardsign                           = 0x6df
let xk_Cyrillic_YU                                 = 0x6e0
let xk_Cyrillic_A                                  = 0x6e1
let xk_Cyrillic_BE                                 = 0x6e2
let xk_Cyrillic_TSE                                = 0x6e3
let xk_Cyrillic_DE                                 = 0x6e4
let xk_Cyrillic_IE                                 = 0x6e5
let xk_Cyrillic_EF                                 = 0x6e6
let xk_Cyrillic_GHE                                = 0x6e7
let xk_Cyrillic_HA                                 = 0x6e8
let xk_Cyrillic_I                                  = 0x6e9
let xk_Cyrillic_SHORTI                             = 0x6ea
let xk_Cyrillic_KA                                 = 0x6eb
let xk_Cyrillic_EL                                 = 0x6ec
let xk_Cyrillic_EM                                 = 0x6ed
let xk_Cyrillic_EN                                 = 0x6ee
let xk_Cyrillic_O                                  = 0x6ef
let xk_Cyrillic_PE                                 = 0x6f0
let xk_Cyrillic_YA                                 = 0x6f1
let xk_Cyrillic_ER                                 = 0x6f2
let xk_Cyrillic_ES                                 = 0x6f3
let xk_Cyrillic_TE                                 = 0x6f4
let xk_Cyrillic_U                                  = 0x6f5
let xk_Cyrillic_ZHE                                = 0x6f6
let xk_Cyrillic_VE                                 = 0x6f7
let xk_Cyrillic_SOFTSIGN                           = 0x6f8
let xk_Cyrillic_YERU                               = 0x6f9
let xk_Cyrillic_ZE                                 = 0x6fa
let xk_Cyrillic_SHA                                = 0x6fb
let xk_Cyrillic_E                                  = 0x6fc
let xk_Cyrillic_SHCHA                              = 0x6fd
let xk_Cyrillic_CHE                                = 0x6fe
let xk_Cyrillic_HARDSIGN                           = 0x6ff


(*
 * Greek
 * Byte 3 = 7
 *)


let xk_Greek_ALPHAaccent                           = 0x7a1
let xk_Greek_EPSILONaccent                         = 0x7a2
let xk_Greek_ETAaccent                             = 0x7a3
let xk_Greek_IOTAaccent                            = 0x7a4
let xk_Greek_IOTAdiaeresis                         = 0x7a5
let xk_Greek_OMICRONaccent                         = 0x7a7
let xk_Greek_UPSILONaccent                         = 0x7a8
let xk_Greek_UPSILONdieresis                       = 0x7a9
let xk_Greek_OMEGAaccent                           = 0x7ab
let xk_Greek_accentdieresis                        = 0x7ae
let xk_Greek_horizbar                              = 0x7af
let xk_Greek_alphaaccent                           = 0x7b1
let xk_Greek_epsilonaccent                         = 0x7b2
let xk_Greek_etaaccent                             = 0x7b3
let xk_Greek_iotaaccent                            = 0x7b4
let xk_Greek_iotadieresis                          = 0x7b5
let xk_Greek_iotaaccentdieresis                    = 0x7b6
let xk_Greek_omicronaccent                         = 0x7b7
let xk_Greek_upsilonaccent                         = 0x7b8
let xk_Greek_upsilondieresis                       = 0x7b9
let xk_Greek_upsilonaccentdieresis                 = 0x7ba
let xk_Greek_omegaaccent                           = 0x7bb
let xk_Greek_ALPHA                                 = 0x7c1
let xk_Greek_BETA                                  = 0x7c2
let xk_Greek_GAMMA                                 = 0x7c3
let xk_Greek_DELTA                                 = 0x7c4
let xk_Greek_EPSILON                               = 0x7c5
let xk_Greek_ZETA                                  = 0x7c6
let xk_Greek_ETA                                   = 0x7c7
let xk_Greek_THETA                                 = 0x7c8
let xk_Greek_IOTA                                  = 0x7c9
let xk_Greek_KAPPA                                 = 0x7ca
let xk_Greek_LAMDA                                 = 0x7cb
let xk_Greek_LAMBDA                                = 0x7cb
let xk_Greek_MU                                    = 0x7cc
let xk_Greek_NU                                    = 0x7cd
let xk_Greek_XI                                    = 0x7ce
let xk_Greek_OMICRON                               = 0x7cf
let xk_Greek_PI                                    = 0x7d0
let xk_Greek_RHO                                   = 0x7d1
let xk_Greek_SIGMA                                 = 0x7d2
let xk_Greek_TAU                                   = 0x7d4
let xk_Greek_UPSILON                               = 0x7d5
let xk_Greek_PHI                                   = 0x7d6
let xk_Greek_CHI                                   = 0x7d7
let xk_Greek_PSI                                   = 0x7d8
let xk_Greek_OMEGA                                 = 0x7d9
let xk_Greek_alpha                                 = 0x7e1
let xk_Greek_beta                                  = 0x7e2
let xk_Greek_gamma                                 = 0x7e3
let xk_Greek_delta                                 = 0x7e4
let xk_Greek_epsilon                               = 0x7e5
let xk_Greek_zeta                                  = 0x7e6
let xk_Greek_eta                                   = 0x7e7
let xk_Greek_theta                                 = 0x7e8
let xk_Greek_iota                                  = 0x7e9
let xk_Greek_kappa                                 = 0x7ea
let xk_Greek_lamda                                 = 0x7eb
let xk_Greek_lambda                                = 0x7eb
let xk_Greek_mu                                    = 0x7ec
let xk_Greek_nu                                    = 0x7ed
let xk_Greek_xi                                    = 0x7ee
let xk_Greek_omicron                               = 0x7ef
let xk_Greek_pi                                    = 0x7f0
let xk_Greek_rho                                   = 0x7f1
let xk_Greek_sigma                                 = 0x7f2
let xk_Greek_finalsmallsigma                       = 0x7f3
let xk_Greek_tau                                   = 0x7f4
let xk_Greek_upsilon                               = 0x7f5
let xk_Greek_phi                                   = 0x7f6
let xk_Greek_chi                                   = 0x7f7
let xk_Greek_psi                                   = 0x7f8
let xk_Greek_omega                                 = 0x7f9
let xk_Greek_switch         = 0xFF7E  (* Alias for mode_switch *)


(*
 * Technical
 * Byte 3 = 8
 *)


let xk_leftradical                                 = 0x8a1
let xk_topleftradical                              = 0x8a2
let xk_horizconnector                              = 0x8a3
let xk_topintegral                                 = 0x8a4
let xk_botintegral                                 = 0x8a5
let xk_vertconnector                               = 0x8a6
let xk_topleftsqbracket                            = 0x8a7
let xk_botleftsqbracket                            = 0x8a8
let xk_toprightsqbracket                           = 0x8a9
let xk_botrightsqbracket                           = 0x8aa
let xk_topleftparens                               = 0x8ab
let xk_botleftparens                               = 0x8ac
let xk_toprightparens                              = 0x8ad
let xk_botrightparens                              = 0x8ae
let xk_leftmiddlecurlybrace                        = 0x8af
let xk_rightmiddlecurlybrace                       = 0x8b0
let xk_topleftsummation                            = 0x8b1
let xk_botleftsummation                            = 0x8b2
let xk_topvertsummationconnector                   = 0x8b3
let xk_botvertsummationconnector                   = 0x8b4
let xk_toprightsummation                           = 0x8b5
let xk_botrightsummation                           = 0x8b6
let xk_rightmiddlesummation                        = 0x8b7
let xk_lessthanequal                               = 0x8bc
let xk_notequal                                    = 0x8bd
let xk_greaterthanequal                            = 0x8be
let xk_integral                                    = 0x8bf
let xk_therefore                                   = 0x8c0
let xk_variation                                   = 0x8c1
let xk_infinity                                    = 0x8c2
let xk_nabla                                       = 0x8c5
let xk_approximate                                 = 0x8c8
let xk_similarequal                                = 0x8c9
let xk_ifonlyif                                    = 0x8cd
let xk_implies                                     = 0x8ce
let xk_identical                                   = 0x8cf
let xk_radical                                     = 0x8d6
let xk_includedin                                  = 0x8da
let xk_includes                                    = 0x8db
let xk_intersection                                = 0x8dc
let xk_union                                       = 0x8dd
let xk_logicaland                                  = 0x8de
let xk_logicalor                                   = 0x8df
let xk_partialderivative                           = 0x8ef
let xk_function                                    = 0x8f6
let xk_leftarrow                                   = 0x8fb
let xk_uparrow                                     = 0x8fc
let xk_rightarrow                                  = 0x8fd
let xk_downarrow                                   = 0x8fe


(*
 *  Special
 *  Byte 3 = 9
 *)


let xk_blank                                       = 0x9df
let xk_soliddiamond                                = 0x9e0
let xk_checkerboard                                = 0x9e1
let xk_ht                                          = 0x9e2
let xk_ff                                          = 0x9e3
let xk_cr                                          = 0x9e4
let xk_lf                                          = 0x9e5
let xk_nl                                          = 0x9e8
let xk_vt                                          = 0x9e9
let xk_lowrightcorner                              = 0x9ea
let xk_uprightcorner                               = 0x9eb
let xk_upleftcorner                                = 0x9ec
let xk_lowleftcorner                               = 0x9ed
let xk_crossinglines                               = 0x9ee
let xk_horizlinescan1                              = 0x9ef
let xk_horizlinescan3                              = 0x9f0
let xk_horizlinescan5                              = 0x9f1
let xk_horizlinescan7                              = 0x9f2
let xk_horizlinescan9                              = 0x9f3
let xk_leftt                                       = 0x9f4
let xk_rightt                                      = 0x9f5
let xk_bott                                        = 0x9f6
let xk_topt                                        = 0x9f7
let xk_vertbar                                     = 0x9f8


(*
 *  Publishing
 *  Byte 3 = a
 *)


let xk_emspace                                     = 0xaa1
let xk_enspace                                     = 0xaa2
let xk_em3space                                    = 0xaa3
let xk_em4space                                    = 0xaa4
let xk_digitspace                                  = 0xaa5
let xk_punctspace                                  = 0xaa6
let xk_thinspace                                   = 0xaa7
let xk_hairspace                                   = 0xaa8
let xk_emdash                                      = 0xaa9
let xk_endash                                      = 0xaaa
let xk_signifblank                                 = 0xaac
let xk_ellipsis                                    = 0xaae
let xk_doubbaselinedot                             = 0xaaf
let xk_onethird                                    = 0xab0
let xk_twothirds                                   = 0xab1
let xk_onefifth                                    = 0xab2
let xk_twofifths                                   = 0xab3
let xk_threefifths                                 = 0xab4
let xk_fourfifths                                  = 0xab5
let xk_onesixth                                    = 0xab6
let xk_fivesixths                                  = 0xab7
let xk_careof                                      = 0xab8
let xk_figdash                                     = 0xabb
let xk_leftanglebracket                            = 0xabc
let xk_decimalpoint                                = 0xabd
let xk_rightanglebracket                           = 0xabe
let xk_marker                                      = 0xabf
let xk_oneeighth                                   = 0xac3
let xk_threeeighths                                = 0xac4
let xk_fiveeighths                                 = 0xac5
let xk_seveneighths                                = 0xac6
let xk_trademark                                   = 0xac9
let xk_signaturemark                               = 0xaca
let xk_trademarkincircle                           = 0xacb
let xk_leftopentriangle                            = 0xacc
let xk_rightopentriangle                           = 0xacd
let xk_emopencircle                                = 0xace
let xk_emopenrectangle                             = 0xacf
let xk_leftsinglequotemark                         = 0xad0
let xk_rightsinglequotemark                        = 0xad1
let xk_leftdoublequotemark                         = 0xad2
let xk_rightdoublequotemark                        = 0xad3
let xk_prescription                                = 0xad4
let xk_minutes                                     = 0xad6
let xk_seconds                                     = 0xad7
let xk_latincross                                  = 0xad9
let xk_hexagram                                    = 0xada
let xk_filledrectbullet                            = 0xadb
let xk_filledlefttribullet                         = 0xadc
let xk_filledrighttribullet                        = 0xadd
let xk_emfilledcircle                              = 0xade
let xk_emfilledrect                                = 0xadf
let xk_enopencircbullet                            = 0xae0
let xk_enopensquarebullet                          = 0xae1
let xk_openrectbullet                              = 0xae2
let xk_opentribulletup                             = 0xae3
let xk_opentribulletdown                           = 0xae4
let xk_openstar                                    = 0xae5
let xk_enfilledcircbullet                          = 0xae6
let xk_enfilledsqbullet                            = 0xae7
let xk_filledtribulletup                           = 0xae8
let xk_filledtribulletdown                         = 0xae9
let xk_leftpointer                                 = 0xaea
let xk_rightpointer                                = 0xaeb
let xk_club                                        = 0xaec
let xk_diamond                                     = 0xaed
let xk_heart                                       = 0xaee
let xk_maltesecross                                = 0xaf0
let xk_dagger                                      = 0xaf1
let xk_doubledagger                                = 0xaf2
let xk_checkmark                                   = 0xaf3
let xk_ballotcross                                 = 0xaf4
let xk_musicalsharp                                = 0xaf5
let xk_musicalflat                                 = 0xaf6
let xk_malesymbol                                  = 0xaf7
let xk_femalesymbol                                = 0xaf8
let xk_telephone                                   = 0xaf9
let xk_telephonerecorder                           = 0xafa
let xk_phonographcopyright                         = 0xafb
let xk_caret                                       = 0xafc
let xk_singlelowquotemark                          = 0xafd
let xk_doublelowquotemark                          = 0xafe
let xk_cursor                                      = 0xaff


(*
 *  APL
 *  Byte 3 = b
 *)


let xk_leftcaret                                   = 0xba3
let xk_rightcaret                                  = 0xba6
let xk_downcaret                                   = 0xba8
let xk_upcaret                                     = 0xba9
let xk_overbar                                     = 0xbc0
let xk_downtack                                    = 0xbc2
let xk_upshoe                                      = 0xbc3
let xk_downstile                                   = 0xbc4
let xk_underbar                                    = 0xbc6
let xk_jot                                         = 0xbca
let xk_quad                                        = 0xbcc
let xk_uptack                                      = 0xbce
let xk_circle                                      = 0xbcf
let xk_upstile                                     = 0xbd3
let xk_downshoe                                    = 0xbd6
let xk_rightshoe                                   = 0xbd8
let xk_leftshoe                                    = 0xbda
let xk_lefttack                                    = 0xbdc
let xk_righttack                                   = 0xbfc


(*
 * Hebrew
 * Byte 3 = c
 *)


let xk_hebrew_doublelowline                        = 0xcdf
let xk_hebrew_aleph                                = 0xce0
let xk_hebrew_bet                                  = 0xce1
let xk_hebrew_beth                                 = 0xce1  (* deprecated *)
let xk_hebrew_gimel                                = 0xce2
let xk_hebrew_gimmel                               = 0xce2  (* deprecated *)
let xk_hebrew_dalet                                = 0xce3
let xk_hebrew_daleth                               = 0xce3  (* deprecated *)
let xk_hebrew_he                                   = 0xce4
let xk_hebrew_waw                                  = 0xce5
let xk_hebrew_zain                                 = 0xce6
let xk_hebrew_zayin                                = 0xce6  (* deprecated *)
let xk_hebrew_chet                                 = 0xce7
let xk_hebrew_het                                  = 0xce7  (* deprecated *)
let xk_hebrew_tet                                  = 0xce8
let xk_hebrew_teth                                 = 0xce8  (* deprecated *)
let xk_hebrew_yod                                  = 0xce9
let xk_hebrew_finalkaph                            = 0xcea
let xk_hebrew_kaph                                 = 0xceb
let xk_hebrew_lamed                                = 0xcec
let xk_hebrew_finalmem                             = 0xced
let xk_hebrew_mem                                  = 0xcee
let xk_hebrew_finalnun                             = 0xcef
let xk_hebrew_nun                                  = 0xcf0
let xk_hebrew_samech                               = 0xcf1
let xk_hebrew_samekh                               = 0xcf1  (* deprecated *)
let xk_hebrew_ayin                                 = 0xcf2
let xk_hebrew_finalpe                              = 0xcf3
let xk_hebrew_pe                                   = 0xcf4
let xk_hebrew_finalzade                            = 0xcf5
let xk_hebrew_finalzadi                            = 0xcf5  (* deprecated *)
let xk_hebrew_zade                                 = 0xcf6
let xk_hebrew_zadi                                 = 0xcf6  (* deprecated *)
let xk_hebrew_qoph                                 = 0xcf7
let xk_hebrew_kuf                                  = 0xcf7  (* deprecated *)
let xk_hebrew_resh                                 = 0xcf8
let xk_hebrew_shin                                 = 0xcf9
let xk_hebrew_taw                                  = 0xcfa
let xk_hebrew_taf                                  = 0xcfa  (* deprecated *)
let xk_Hebrew_switch        = 0xFF7E  (* Alias for mode_switch *)


(*
 * Thai
 * Byte 3 = d
 *)


let xk_Thai_kokai					= 0xda1
let xk_Thai_khokhai					= 0xda2
let xk_Thai_khokhuat				= 0xda3
let xk_Thai_khokhwai				= 0xda4
let xk_Thai_khokhon					= 0xda5
let xk_Thai_khorakhang			        = 0xda6  
let xk_Thai_ngongu					= 0xda7  
let xk_Thai_chochan					= 0xda8  
let xk_Thai_choching				= 0xda9   
let xk_Thai_chochang				= 0xdaa  
let xk_Thai_soso					= 0xdab
let xk_Thai_chochoe					= 0xdac
let xk_Thai_yoying					= 0xdad
let xk_Thai_dochada					= 0xdae
let xk_Thai_topatak					= 0xdaf
let xk_Thai_thothan					= 0xdb0
let xk_Thai_thonangmontho			        = 0xdb1
let xk_Thai_thophuthao			        = 0xdb2
let xk_Thai_nonen					= 0xdb3
let xk_Thai_dodek					= 0xdb4
let xk_Thai_totao					= 0xdb5
let xk_Thai_thothung				= 0xdb6
let xk_Thai_thothahan				= 0xdb7
let xk_Thai_thothong	 			= 0xdb8
let xk_Thai_nonu					= 0xdb9
let xk_Thai_bobaimai				= 0xdba
let xk_Thai_popla					= 0xdbb
let xk_Thai_phophung				= 0xdbc
let xk_Thai_fofa					= 0xdbd
let xk_Thai_phophan					= 0xdbe
let xk_Thai_fofan					= 0xdbf
let xk_Thai_phosamphao			        = 0xdc0
let xk_Thai_moma					= 0xdc1
let xk_Thai_yoyak					= 0xdc2
let xk_Thai_rorua					= 0xdc3
let xk_Thai_ru					= 0xdc4
let xk_Thai_loling					= 0xdc5
let xk_Thai_lu					= 0xdc6
let xk_Thai_wowaen					= 0xdc7
let xk_Thai_sosala					= 0xdc8
let xk_Thai_sorusi					= 0xdc9
let xk_Thai_sosua					= 0xdca
let xk_Thai_hohip					= 0xdcb
let xk_Thai_lochula					= 0xdcc
let xk_Thai_oang					= 0xdcd
let xk_Thai_honokhuk				= 0xdce
let xk_Thai_paiyannoi				= 0xdcf
let xk_Thai_saraa					= 0xdd0
let xk_Thai_maihanakat				= 0xdd1
let xk_Thai_saraaa					= 0xdd2
let xk_Thai_saraam					= 0xdd3
let xk_Thai_sarai					= 0xdd4   
let xk_Thai_saraii					= 0xdd5   
let xk_Thai_saraue					= 0xdd6    
let xk_Thai_sarauee					= 0xdd7    
let xk_Thai_sarau					= 0xdd8    
let xk_Thai_sarauu					= 0xdd9   
let xk_Thai_phinthu					= 0xdda
let xk_Thai_maihanakat_maitho   			= 0xdde
let xk_Thai_baht					= 0xddf
let xk_Thai_sarae					= 0xde0    
let xk_Thai_saraae					= 0xde1
let xk_Thai_sarao					= 0xde2
let xk_Thai_saraaimaimuan				= 0xde3   
let xk_Thai_saraaimaimalai				= 0xde4  
let xk_Thai_lakkhangyao				= 0xde5
let xk_Thai_maiyamok				= 0xde6
let xk_Thai_maitaikhu				= 0xde7
let xk_Thai_maiek					= 0xde8   
let xk_Thai_maitho					= 0xde9
let xk_Thai_maitri					= 0xdea
let xk_Thai_maichattawa				= 0xdeb
let xk_Thai_thanthakhat				= 0xdec
let xk_Thai_nikhahit				= 0xded
let xk_Thai_leksun					= 0xdf0 
let xk_Thai_leknung					= 0xdf1  
let xk_Thai_leksong					= 0xdf2 
let xk_Thai_leksam					= 0xdf3
let xk_Thai_leksi					= 0xdf4  
let xk_Thai_lekha					= 0xdf5  
let xk_Thai_lekhok					= 0xdf6  
let xk_Thai_lekchet					= 0xdf7  
let xk_Thai_lekpaet					= 0xdf8  
let xk_Thai_lekkao					= 0xdf9 


(*
 *   Korean
 *   Byte 3 = e
 *)



let xk_Hangul		= 0xff31    (* Hangul start/stop(toggle) *)
let xk_Hangul_Start		= 0xff32    (* Hangul start *)
let xk_Hangul_End		= 0xff33    (* Hangul end, English start *)
let xk_Hangul_Hanja		= 0xff34    (* Start Hangul->Hanja Conversion *)
let xk_Hangul_Jamo		= 0xff35    (* Hangul Jamo mode *)
let xk_Hangul_Romaja	= 0xff36    (* Hangul Romaja mode *)
let xk_Hangul_Codeinput	= 0xff37    (* Hangul code input mode *)
let xk_Hangul_Jeonja	= 0xff38    (* Jeonja mode *)
let xk_Hangul_Banja		= 0xff39    (* Banja mode *)
let xk_Hangul_PreHanja	= 0xff3a    (* Pre Hanja conversion *)
let xk_Hangul_PostHanja	= 0xff3b    (* Post Hanja conversion *)
let xk_Hangul_SingleCandidate	= 0xff3c    (* Single candidate *)
let xk_Hangul_MultipleCandidate	= 0xff3d    (* Multiple candidate *)
let xk_Hangul_PreviousCandidate	= 0xff3e    (* Previous candidate *)
let xk_Hangul_Special	= 0xff3f    (* Special symbols *)
let xk_Hangul_switch	= 0xFF7E    (* Alias for mode_switch *)

(* Hangul Consonant Characters *)
let xk_Hangul_Kiyeog				= 0xea1
let xk_Hangul_SsangKiyeog				= 0xea2
let xk_Hangul_KiyeogSios				= 0xea3
let xk_Hangul_Nieun					= 0xea4
let xk_Hangul_NieunJieuj				= 0xea5
let xk_Hangul_NieunHieuh				= 0xea6
let xk_Hangul_Dikeud				= 0xea7
let xk_Hangul_SsangDikeud				= 0xea8
let xk_Hangul_Rieul					= 0xea9
let xk_Hangul_RieulKiyeog				= 0xeaa
let xk_Hangul_RieulMieum				= 0xeab
let xk_Hangul_RieulPieub				= 0xeac
let xk_Hangul_RieulSios				= 0xead
let xk_Hangul_RieulTieut				= 0xeae
let xk_Hangul_RieulPhieuf				= 0xeaf
let xk_Hangul_RieulHieuh				= 0xeb0
let xk_Hangul_Mieum					= 0xeb1
let xk_Hangul_Pieub					= 0xeb2
let xk_Hangul_SsangPieub				= 0xeb3
let xk_Hangul_PieubSios				= 0xeb4
let xk_Hangul_Sios					= 0xeb5
let xk_Hangul_SsangSios				= 0xeb6
let xk_Hangul_Ieung					= 0xeb7
let xk_Hangul_Jieuj					= 0xeb8
let xk_Hangul_SsangJieuj				= 0xeb9
let xk_Hangul_Cieuc					= 0xeba
let xk_Hangul_Khieuq				= 0xebb
let xk_Hangul_Tieut					= 0xebc
let xk_Hangul_Phieuf				= 0xebd
let xk_Hangul_Hieuh					= 0xebe

(* Hangul Vowel Characters *)
let xk_Hangul_A					= 0xebf
let xk_Hangul_AE					= 0xec0
let xk_Hangul_YA					= 0xec1
let xk_Hangul_YAE					= 0xec2
let xk_Hangul_EO					= 0xec3
let xk_Hangul_E					= 0xec4
let xk_Hangul_YEO					= 0xec5
let xk_Hangul_YE					= 0xec6
let xk_Hangul_O					= 0xec7
let xk_Hangul_WA					= 0xec8
let xk_Hangul_WAE					= 0xec9
let xk_Hangul_OE					= 0xeca
let xk_Hangul_YO					= 0xecb
let xk_Hangul_U					= 0xecc
let xk_Hangul_WEO					= 0xecd
let xk_Hangul_WE					= 0xece
let xk_Hangul_WI					= 0xecf
let xk_Hangul_YU					= 0xed0
let xk_Hangul_EU					= 0xed1
let xk_Hangul_YI					= 0xed2
let xk_Hangul_I					= 0xed3

(* Hangul syllable-final (JongSeong) Characters *)
let xk_Hangul_J_Kiyeog				= 0xed4
let xk_Hangul_J_SsangKiyeog				= 0xed5
let xk_Hangul_J_KiyeogSios				= 0xed6
let xk_Hangul_J_Nieun				= 0xed7
let xk_Hangul_J_NieunJieuj				= 0xed8
let xk_Hangul_J_NieunHieuh				= 0xed9
let xk_Hangul_J_Dikeud				= 0xeda
let xk_Hangul_J_Rieul				= 0xedb
let xk_Hangul_J_RieulKiyeog				= 0xedc
let xk_Hangul_J_RieulMieum				= 0xedd
let xk_Hangul_J_RieulPieub				= 0xede
let xk_Hangul_J_RieulSios				= 0xedf
let xk_Hangul_J_RieulTieut				= 0xee0
let xk_Hangul_J_RieulPhieuf				= 0xee1
let xk_Hangul_J_RieulHieuh				= 0xee2
let xk_Hangul_J_Mieum				= 0xee3
let xk_Hangul_J_Pieub				= 0xee4
let xk_Hangul_J_PieubSios				= 0xee5
let xk_Hangul_J_Sios				= 0xee6
let xk_Hangul_J_SsangSios				= 0xee7
let xk_Hangul_J_Ieung				= 0xee8
let xk_Hangul_J_Jieuj				= 0xee9
let xk_Hangul_J_Cieuc				= 0xeea
let xk_Hangul_J_Khieuq				= 0xeeb
let xk_Hangul_J_Tieut				= 0xeec
let xk_Hangul_J_Phieuf				= 0xeed
let xk_Hangul_J_Hieuh				= 0xeee

(* Ancient Hangul Consonant Characters *)
let xk_Hangul_RieulYeorinHieuh			= 0xeef
let xk_Hangul_SunkyeongeumMieum			= 0xef0
let xk_Hangul_SunkyeongeumPieub			= 0xef1
let xk_Hangul_PanSios				= 0xef2
let xk_Hangul_KkogjiDalrinIeung			= 0xef3
let xk_Hangul_SunkyeongeumPhieuf			= 0xef4
let xk_Hangul_YeorinHieuh				= 0xef5

(* Ancient Hangul Vowel Characters *)
let xk_Hangul_AraeA					= 0xef6
let xk_Hangul_AraeAE				= 0xef7

(* Ancient Hangul syllable-final (JongSeong) Characters *)
let xk_Hangul_J_PanSios				= 0xef8
let xk_Hangul_J_KkogjiDalrinIeung			= 0xef9
let xk_Hangul_J_YeorinHieuh				= 0xefa

(* Korean currency symbol *)
let xk_Korean_Won					= 0xeff


