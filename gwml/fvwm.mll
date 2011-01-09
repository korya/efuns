{
(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

  open Gwml_args
  open Options
  open Xtypes
  open Xlib
  open Gwml  
  open Stdconfig
  open User
  open Wob
  open Parameters  
  open Modules
  open Unix


  let add_to_path option dir =
    let list = !!option in
    if not (List.mem dir list) then option =:= dir :: list
        
let fvwm_file = define_option ["fvwm_file"] 
    "<fvwm_file> is the name of your fvwm file, used to find the fvwm menu. " 
  filename_option
  ".fvwm2rc"
let fvwm_main_menu = define_option ["fvwm_main_menu"] 
  "<fvwm_main_menu> is the name of the fvwm menu loaded from the fvwm file <fvwm_file>." string_option
  "Red_Hat_Linux"

type action =
  RaiseLower
| Iconify
| Move
| Resize
| WindowList
| Module of string * (string list)
| Maximize of int * int
| FvwmFunction of string
| Popup of string
| Prev of action
| Next of action
| Focus
| FvwmMenu of string * action
| Delete
| Destroy
| Stick
| Exec of string
| Raise
| Lower
| Title
| Refresh
| Nop
| Scroll of int * int
| Restart of string
| Quit
| RecDef of func
| CursorMove of int * int
| Desk of int * int    
| Close
| SetMask of int
| SendWindowList
| SendConfigInfo
| GotoPage of int * int
  
type token =
    
    (* Fvwm 2 *)
  Style of string * (Parameters.t list)
| FvwmAuto of int
| WindowFont of string
| IconFont of string (* + Twmrc *)
| MenuFont of string (* + Twmrc *)
| DefaultColors of string * string * string * string
| HilightColors of string * string
| StickyColors of string * string
| FvwmKey of string * string * string * action
| Mouse of int * string * string * action
| ModulePath of string
| PixmapPath of string
| IconPath of string
| OpaqueMoveSize of int
| EdgeResistance of int * int
| EdgeScroll of int * int
| DeskTopSize of string
| DeskTopScale of int
| AddToFunc of string * (string * action) list
| AddToMenu of string * (string * action) list

| MWMDecorHints
| MWMHintOverride
| MWMFunctionHints
| GlobalDecoration of Parameters.t
    
  (* Fvwm 1 *)
| StdForeColor of string
| StdBackColor of string
| HiForeColor of string
| HiBackColor of string
| PagerForeColor of string
| PagerBackColor of string
| StickyForeColor of string
| StickyBackColor of string
| MenuForeColor of string
| MenuBackColor of string
| MenuStippleColor of string
| ButtonStyle of int * string
| IconBox of int * int * int * int
| AutoRaise of int    
| BoundaryWidth of int
| NoBoundaryWidth of int
| StubbornIconPlacement    
| StubbornIcons
| SuppressIcons
| StickyIcons
| Pager of int * int
    
  (* common *)
| Error  of string * int    
| EOF  

  
  
  
let rec action lexbuf name get_name get_int get_line =
  try
    match String.lowercase name with
      "raiselower" -> RaiseLower
    | "iconify" -> Iconify
    | "move" -> Move
    | "resize" -> Resize
    | "windowlist" -> WindowList
    | "module" -> let f1 = get_name lexbuf in
        let rec iter list = 
          try
            let n = get_name lexbuf in
            if n = "" then list else
              iter (n :: list)
          with
            _ -> list
        in
        Module (f1,List.rev (iter []))
    | "maximize" -> 
        let f1 = get_int lexbuf in
        let f2 = get_int lexbuf in Maximize (f1,f2)
    | "function" -> FvwmFunction  (get_name lexbuf)
    | "popup" -> Popup (get_name lexbuf)
    | "prev" -> Prev (action lexbuf (get_name lexbuf)
          get_name get_int get_line)
    | "next" -> Next (action lexbuf (get_name lexbuf)
          get_name get_int get_line)
    | "focus" -> Focus
    | "menu" -> let name =get_name lexbuf in 
        FvwmMenu (name,action lexbuf (get_name lexbuf)
          get_name get_int get_line)
    | "delete" -> Delete
    | "exec" -> Exec (get_line lexbuf)
    | "raise" -> Raise
    | "lower" -> Lower
    | "title" -> Title
    | "nop"  -> Nop
    | "refresh" -> Refresh
    | "destroy" -> Destroy
    | "stick" -> Stick
    | "scroll" -> 
        let c1 = get_int lexbuf in
        let c2 = get_int lexbuf in
        Scroll (c1,c2)
    | "restart" -> Restart (get_line lexbuf)
    | "quit" -> Quit
    | "cursormove" -> 
        let f1 = get_int lexbuf in
        let f2 = get_int lexbuf in CursorMove (f1,f2)          
    | "gotopage" -> 
        let f1 = get_int lexbuf in
        let f2 = get_int lexbuf in GotoPage (f1,f2)          
    | "desk" -> 
        let f1 = get_int lexbuf in
        let f2 = if f1 = 0 then get_int lexbuf else 0
        in Desk (f1,f2)          
    
    | "close" -> Close
    | "set_mask" -> SetMask (get_int lexbuf)
    | "send_windowlist" -> SendWindowList
    | "send_configinfo" -> SendConfigInfo
    | _ -> 
        if String.sub name 0 4 = "Fvwm" then 
        (* This must be a module name *)
          begin
            let rec iter list = 
              try
                let n = get_name lexbuf in
                if n = "" then list else
                  iter (n :: list)
              with
                _ -> list
            in
            Module (name,List.rev (iter []))
          end
        else
          begin
            Log.printf "No action [%s]" name;
            Log.printf " at %d\n" (Lexing.lexeme_start lexbuf);
            FvwmFunction  name
          end
  with
    _ -> 
      Log.printf "Error at %d\n" (Lexing.lexeme_start lexbuf);
      Nop
      
      
    
}

let blank = [' ' '\009' '\t' ] | ('\\' '\n')
let return = ['\010' '\012' '\013']
let name = ['a'-'z' 'A'-'Z']+

rule fvwm = parse
    '#' [^ '\n'] * '\n'        { fvwm lexbuf }
  | '*'  {  let line = get_line lexbuf in
      config_info := ("*" ^ line) :: !config_info;
      fvwm lexbuf }  
  | return
  | blank       { fvwm lexbuf }
  | name        { 
      try
        let name = Lexing.lexeme lexbuf in
        match String.lowercase name with
          "style" -> 
            let name = get_name lexbuf in
            style_args lexbuf name []
        | "fvwmauto" ->
            let time = get_int lexbuf in FvwmAuto time
        | "windowfont" -> 
            let font = get_name lexbuf in WindowFont font
        | "font" -> 
            let font = get_name lexbuf in MenuFont font
        | "iconfont" -> 
            let font = get_name lexbuf in IconFont font
        | "menufont" -> 
            let font = get_name lexbuf in MenuFont font
        | "defaultcolors" ->
            let c1 = get_name lexbuf in
            let c2 = get_name lexbuf in
            let c3 = get_name lexbuf in
            let c4 = get_name lexbuf in
            DefaultColors (c1,c2,c3,c4)
        | "hilightcolors" ->
            let c1 = get_name lexbuf in
            let c2 = get_name lexbuf in
            HilightColors (c1,c2)
        | "stickycolors" ->
            let c1 = get_name lexbuf in
            let c2 = get_name lexbuf in
            StickyColors (c1,c2)
        | "key" -> 
            let key = get_name lexbuf in
            let where = get_name lexbuf in
            let modifiers = get_name lexbuf in
            let action = action lexbuf (get_name lexbuf)
              get_name get_int get_line in
            let _ = get_line lexbuf in (* remove end of line *)
            FvwmKey (key,where,modifiers,action)
        | "mouse" -> 
            let button = get_int lexbuf in
            let where = get_name lexbuf in
            let modifiers = get_name lexbuf in
            let action = action lexbuf (get_name lexbuf)
              get_name get_int get_line in
            let _ = get_line lexbuf in  (* remove end of line *)
            Mouse (button,where,modifiers,action)            
        | "modulepath" -> 
            let path =  get_name lexbuf in 
            ModulePath path
        | "pixmappath" -> 
            let path =  get_name lexbuf in 
            PixmapPath path
        | "iconpath" -> 
            let path =  get_name lexbuf in 
            IconPath path
        | "opaquemovesize" -> OpaqueMoveSize (get_int lexbuf)
        | "edgescroll" -> let c1 = get_int lexbuf in
            let c2 = get_int lexbuf in
            EdgeScroll(c1,c2)
        | "edgeresistance" -> let c1 = get_int lexbuf in
            let c2 = get_int lexbuf in
            EdgeResistance(c1,c2)
        | "desktopsize" -> let size = get_name lexbuf in DeskTopSize size
        | "desktopscale" -> let size = get_int lexbuf in DeskTopScale size
        | "addtofunc" -> let func = get_name lexbuf in
            let rec addtofunc lexbuf list = 
              let c = get_name lexbuf in
              let a = action lexbuf (get_name lexbuf) 
                get_name get_int get_line in
              get_addto_next lexbuf ((c,a)::list) addtofunc
            in
            let funs = addtofunc lexbuf [] in
            AddToFunc (func, List.rev funs)
        | "addtomenu" -> let menu = get_name lexbuf in
            let rec addtomenu lexbuf list = 
              let c = get_name lexbuf in              
              if c = "" then
                get_addto_next lexbuf list addtomenu
              else
              let a = action lexbuf (get_name lexbuf)
                get_name get_int get_line in
              get_addto_next lexbuf ((c,a)::list) addtomenu
            in
            let list = addtomenu lexbuf [] in
            AddToMenu (menu, List.rev list)
        | "stdforecolor" -> let color = get_name lexbuf in StdForeColor color
        | "stdbackcolor" -> let color = get_name lexbuf in StdBackColor color
        | "hiforecolor" -> let color = get_name lexbuf in HiForeColor color
        | "hibackcolor" -> let color = get_name lexbuf in HiBackColor color
        | "pagerforecolor" -> 
            let color = get_name lexbuf in PagerForeColor color
        | "pagerbackcolor" -> 
            let color = get_name lexbuf in PagerBackColor color
        | "stickyforecolor" -> 
            let color = get_name lexbuf in StickyForeColor color
        | "stickybackcolor" -> 
            let color = get_name lexbuf in StickyBackColor color
        | "menuforecolor" -> let color = get_name lexbuf in MenuForeColor color
        | "menubackcolor" -> let color = get_name lexbuf in MenuBackColor color
        | "menustipplecolor" -> 
            let color = get_name lexbuf in MenuStippleColor color
        | "randomplacement" -> GlobalDecoration RandomPlacement
        | "smartplacement" -> GlobalDecoration SmartPlacement
        | "stubbornplacement" -> GlobalDecoration StubbornPlacement
        | "nopposition" -> GlobalDecoration NoPPosition
        | "sloppyfocus" -> GlobalDecoration SloppyFocus
        | "mwmhintoverride" -> MWMHintOverride
        | "mwmdecorhints" -> MWMDecorHints
        | "mwmfunctionhints" -> MWMFunctionHints
        | "iconbox" -> 
            let x = get_int lexbuf in
            let y = get_int lexbuf in
            let xx = get_int lexbuf in
            let yy = get_int lexbuf in
            IconBox (x,y,xx,yy)
        | "buttonstyle" -> let b = get_int lexbuf in
            ButtonStyle (b,get_line lexbuf)
        | "popup" -> 
            let name = get_name lexbuf in
            get_popup lexbuf name []
        | "function" -> let name = get_name lexbuf in
            get_fun lexbuf name []
        | "boundarywidth" -> BoundaryWidth (get_int lexbuf)
        | "noboundarywidth" -> NoBoundaryWidth (get_int lexbuf)
        | "autoraise" -> AutoRaise (get_int lexbuf)
        | "stubbornicons" -> StubbornIcons
        | "stubborniconplacement" -> StubbornIconPlacement
        | "suppressicons" -> SuppressIcons
        | "stickyicons" -> StickyIcons
        | "opaquemove" -> OpaqueMoveSize (get_int lexbuf)
        | "decoratetransients" -> GlobalDecoration DecorateTransient
        | "pager" -> let x = get_int lexbuf in let y = get_int lexbuf in
            Pager(x,y)
        | _ -> end_of_line lexbuf (
              Error (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf))
      
      with
        _ ->     end_of_line lexbuf (
            Error (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf))
    }
  | eof  { EOF }
  | _    { end_of_line lexbuf (
        Error (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)) }

and get_popup = parse
    blank | '\n' { get_popup lexbuf }
  | name { fun name list -> 
        let code = Lexing.lexeme lexbuf in
        if String.lowercase code = "endpopup" then AddToMenu (name, List.rev list)
        else
        let nom = get_name lexbuf in
        let action = action lexbuf code get_name get_int get_line in
        get_popup lexbuf name ((nom, action):: list)
    } 
  | '#' [^ '\n'] * '\n'        { get_popup lexbuf }
  | _  { Log.printf "Error at %d\n" (Lexing.lexeme_start lexbuf);
      raise Exit }

and get_fun = parse
    blank | '\n' { get_fun lexbuf }
  | name { fun name list -> 
        let code = Lexing.lexeme lexbuf in
        if String.lowercase code = "endfunction" then
          AddToFunc (name, List.rev list)
        else
        let nom = get_name lexbuf in
        let action = action lexbuf code get_name get_int get_line in
        get_fun lexbuf name ((nom, action):: list)
    } 
  | '#' [^ '\n'] * '\n'        { get_fun lexbuf }    
  | _  { raise Exit }
    
and get_addto_next = parse
    blank         { get_addto_next lexbuf }
  | [^ '\n'] * '\n' '+'      { fun list addto -> addto lexbuf list }
  | [^ '\n'] * '\n' blank * '#' [^ '\n'] * { get_addto_next lexbuf }
  | '\n'          { fun list addto -> list }
  | _ { raise Exit }

and get_line = parse
    blank+            { get_line lexbuf }
  | [^ ' ' '\t' '\n'] [^ '\n'] *  { 
      let s = Lexing.lexeme lexbuf in
      let len = String.length s in
      if len > 0 && s.[len-1] = '\\' then
        begin
          take_char lexbuf;
          s.[len-1] <- ' ';
          s ^ (get_line lexbuf)
        end
      else s
      }
  | ""                { "" }
    
and take_char = parse
    _  { () }
    
and end_of_line = parse
    [^ '\n']  * '\n' { function token -> token }

and get_name = parse
    '"' [^ '"']* '"' { let class_name = Lexing.lexeme lexbuf in
    let class_name = String.sub class_name 1 (String.length class_name - 2) in
    class_name }
  | ['-' 'a'-'z' 'A'-'Z' '0'-'9' '*' '/' '_' '.' ':' '#']* { Lexing.lexeme lexbuf }
  | blank  { get_name lexbuf }

and get_color = parse
    '"' [^ '"']* '"' { let class_name = Lexing.lexeme lexbuf in
    let class_name = String.sub class_name 1 (String.length class_name - 2) in
    class_name }
  | [ 'a'-'z' 'A'-'Z' '0'-'9' '#']* { Lexing.lexeme lexbuf }
  | '/'
  | blank  { get_color lexbuf }
(*  | _ { raise Exit } *)

and style_args = parse
    blank { style_args lexbuf }
  | name  { fun class_name args ->
        try
          let name = Lexing.lexeme lexbuf in
          let arg = 
            match String.lowercase name with
              "borderwidth" -> let arg = get_int lexbuf in BorderWidth arg
            | "handlewidth" -> let arg = get_int lexbuf in HandleWidth arg
            | "notitle" -> NoTitle
            | "sticky" -> Sticky
            | "windowlistskip" -> WindowListSkip
            | "staysontop" -> StaysOnTop
            | "clicktofocus" -> ClickToFocus
            | "nohandles" -> NoHandles
            | "startsondesk" -> StartsOnDesk (get_int lexbuf)
            | "titleicon" -> let arg = get_name lexbuf in TitleIcon arg
            | "nopposition" -> NoPPosition
            | "smartplacement" -> SmartPlacement
            | "randomplacement" -> RandomPlacement
            | "sloppyfocus" -> SloppyFocus
            | "noicon" -> NoIcon
            | "mwmdecor" -> MWMDecor
            | "mwmfunctions" -> MWMFunctions
            | "hintoverride" -> HintOverride
            | "decoratetransient" -> DecorateTransient
            | "circulateskip" -> CirculateSkip
            | "forecolor" -> ForeColor (get_name lexbuf)
            | "backcolor" -> BackColor (get_name lexbuf)
            | "miniicon" -> TitleIcon (get_name lexbuf)
            | "icon" -> Icon (get_name lexbuf)
            | "nobutton" -> NoButton (get_int lexbuf)
            | "starticonic" -> StartIconic
            | "color" -> 
                let c1 = get_color lexbuf in
                let c2 = get_color lexbuf in Color (c1,c2)
                
            | _ -> raise Not_found
          in
          style_next lexbuf class_name (arg :: args)
        with
          _ -> end_of_line lexbuf (
              Error ("Error in Style args", Lexing.lexeme_start lexbuf))
    }
  | _    { fun _ _ -> end_of_line lexbuf (
          Error (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)) }

and get_int = parse
    blank { get_int lexbuf }
  | '-' { - (get_int lexbuf) }
  | '+' { get_int lexbuf }
  | ['0'-'9'] + { int_of_string (Lexing.lexeme lexbuf) }
  | _  { 
      Log.printf "Error at %d in get_int\n" (Lexing.lexeme_start lexbuf);
      raise Exit
        }

and style_next = parse
    blank { style_next lexbuf }
  | ','   { style_args lexbuf }
  | '\n'  { fun class_name args -> Style (class_name, args) }
  | '#' [^'\n']*'\n'  { fun class_name args -> Style (class_name, args) }
  | _     { fun _ _ ->  end_of_line lexbuf (
          Error (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf))
    }

{

let fvwm1_config_file = ref "none"
let fvwm2_config_file = ref "none"
let afterstep_config_file = ref "none"

let load proto file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  (match proto with
      Fvwm1 ->   fvwm1_config_file := file;
    | Fvwm2 ->   fvwm2_config_file := file;
    | Afterstep -> afterstep_config_file := file
        );
  let rec iter list =
    let token = fvwm lexbuf in
    match token with
      EOF -> list
    | Error (s,pos) -> 
        Printf.printf "Error on token %s at %d" s pos; print_newline ();
        iter list
    | _ -> iter (token :: list)
  in
  let list = iter [] in
  close_in ic;
  (file,list)


  
  
exception Found of int
let get_pixmap name =
  let len = String.length name in  
  if len>2 && name.[len-1] = '%' then
    try
      for i = 0 to len - 3 do
        if name.[i] = '%' then raise (Found i)
      done;
      name, []
    with
      Found i ->
        String.sub name 0 i, 
        [ItemPixmap (FromFile (String.sub name (i+1) (len - i -2)), true)]
  else name, []

let _ =
  Utils.set_signal Sys.sigpipe  (Sys.Signal_handle (fun _ -> ()))
    
let fvwm_menu name () = 
  try Hashtbl.find menus_table ("fvwmrc:"^ name) with _ -> 
      Log.printf "Fwvm: No menu <%s>\n" name;
      []
let fvwm_function s w = 
  try (Hashtbl.find funs_table ("fvwmrc:"^s)) w with _ -> ()

let afterstep_share = ref "/usr/local/share/afterstep"
  
let config_file proto w =
  match proto with
    Fvwm1 -> !fvwm1_config_file
  | Fvwm2 -> !fvwm2_config_file      
  | Afterstep ->
      if !afterstep_config_file = "none" then
        let file = Printf.sprintf "base.%dbpp" w.w_screen.s_scr.scr_root_depth
        in
        Filename.concat !afterstep_share file
      else
        !afterstep_config_file
      
let rec fvwm_action proto (cond,action) w =
  let s = w.w_screen in
  let click = Wob.click s in
  let condition = match cond with
    | "M" when click <> DeltaMove -> false
    | "C" when click <> Simple -> false
    | "D" when click <> Double -> false
    | "Motion" when click <> DeltaMove -> false
    | "Click" when click <> Simple -> false
    | "DoubleClick" when click <> Double -> false
    | _ -> true  
  in
  if condition then
    match action with
      RaiseLower -> 
        X.configureWindow display (User.client_top w).w_window [CWStackMode Opposite]
    | Iconify -> Wob.send (client_top w) (WobIconifyRequest true)
    | Move -> User.move (client_top w) true
    | Resize -> resize (client_top w) true
    | WindowList -> let _ = popup_menu w false (winmenu w) in ()        
    | Module (name, args) -> 
        execute_fvwm_module proto name w C_NO_CONTEXT args
    | Maximize (dx,dy) -> 
        let w = client_top w in
        let old_percent_x = !!x_maximize_use in
        let old_percent_y = !!y_maximize_use in
        x_maximize_use =:= dx; 
        maximize_x w;
        y_maximize_use =:= dy;
        maximize_y w;
        x_maximize_use =:= old_percent_x;
        y_maximize_use =:= old_percent_y
    | FvwmFunction name -> fvwm_function name w
    | Popup name -> let _ = popup_menu w false (fvwm_menu name ()) in ()
    | Prev action ->  Printf.printf "Prev not implemented"; print_newline ()
    | Next action ->  Printf.printf "Next not implemented"; print_newline ()
    | Focus ->  Printf.printf "Focus not implemented"; print_newline ()
    | FvwmMenu (string,action) -> 
        let _ = popup_menu w false (fvwm_menu string ()) in ()
    | Delete -> Wob.send w.w_top WobDeleteWindow
    | Destroy ->  X.killClient display (window_to_id 
            ((client_top w).w_oo#client.c_window))
    | Stick ->  Printf.printf "Stick not implemented"; print_newline ()
    | Exec string -> 
        commandw string ()
    | Raise -> Wob.send (client_top w) WobRaiseWindow
    | Lower -> lowerWindow display (client_top w).w_window
    | Title ->  ()
    | Refresh ->  Printf.printf "Refresh not implemented"; print_newline ()
    | Nop ->  ()
    | Scroll (dx,dy) ->  
        let rw = w.w_top.w_parent in
        let rg = rw.w_geometry in      
        !virtual_manager#move w (-dx * rg.width / 100) (-dy * rg.height / 100)
    | Restart string ->
        Wob.restore_clients ();
        Utils.set_signal Sys.sigalrm Sys.Signal_ignore;
        Utils.set_signal Sys.sigvtalrm Sys.Signal_ignore; 
        if string = "" then
          Unix.execvp "/bin/sh" [| "/bin/sh"; "-c"; string |]
        else
          Unix.execvp !restart_cmd.(0) !restart_cmd
    | Quit -> Wob.exit_gwml ()
    | RecDef f -> f w
    | CursorMove (dx,dy) ->
        let rw = w.w_top.w_parent in
        let rg = rw.w_geometry in
        let qp = X.queryPointer display rw.w_window in
        X.warpPointer display rw.w_window 0 0 rg.width rg.height
          rw.w_window (qp.qp_root_x + dx) (qp.qp_root_y + dy)
    | GotoPage (x,y) ->
        Printf.printf "GotoPage %d %d" x y; print_newline ();
        let sw = w.w_top.w_parent in
        let s = sw.w_screen in
        let scr = s.s_scr in
        let width = scr.scr_width in
        let height = scr.scr_height in
        let mng = !Stdconfig.virtual_manager in
        let (x0,y0) = mng#current_position w in
        Printf.printf "Current pos: %d %d" x0 y0; print_newline ();
        let _ = mng#move w (x0-x*width) (-(y*height-y0)) in
        let (x0,y0) = mng#current_position w in
        Printf.printf "New pos: %d %d" x0 y0; print_newline ();
    | Desk (dx,dy) -> 
    (* No workspaces *) ()
        (*
        let x,y = !virtual_manager#current_position w in
        let sw = w.w_top.w_parent in
        let sg = sw.w_geometry in
        !virtual_manager#move w (dx * sg.width - x) (dy * sg.height - y)
    *)
    | Close -> Wob.send  (client_top w) WobDeleteWindow      
    | SetMask m -> ()
    | SendWindowList -> ()
    | SendConfigInfo -> ()

and
  read_module_command m w =
  try
    if m.pipe_on then 
      begin 
      (* read next entry *)
        m.pipe_on <- false;
        let count = Unix.read m.read_pipe buffer 0 int in
        if count < int || 
          (buffer.[0] = '\000' &&
            buffer.[1] = '\000' &&
            buffer.[2] = '\000' &&
            buffer.[3] = '\000')
        then raise End_of_file else raise Exit;
      end  
    else m.pipe_on <- true;
    let count = Unix.read m.read_pipe buffer 0 long in
    if count < long then raise End_of_file;
    let win = getLong 0 in
    let count = Unix.read m.read_pipe buffer 0 int in
    if count < int then raise End_of_file;
    let size = getInt 0 in
    if size > 255 then raise End_of_file;
    let count = Unix.read m.read_pipe buffer 0 size in
    if count < size then raise End_of_file;
    let s = String.sub buffer 0 size in
    (* try to understand what the module said *)
(*    
    Printf.printf "Received <%s> from module %s" s m.name; 
    print_newline ();
    *)
    Printf.printf "Module cmd: <%s>" s; print_newline ();
    let lexbuf = Lexing.from_string s in
    let action = 
      match m.proto with
        Afterstep -> 
          let name = String.lowercase (get_name lexbuf) in
          if name = "exec" then (let _ = get_name lexbuf in ());
          action lexbuf name get_name get_int get_line
      | _ -> action lexbuf (get_name lexbuf) get_name get_int get_line 
    in
    begin
      match action with
        SetMask mask -> m.module_mask <- mask;
      | SendWindowList -> send_WindowList m w;
      | SendConfigInfo -> send_ConfigInfo m w
      | _ -> 
          try
            let w =
              try
                let (c,w) = Wintbl.find w.w_screen.s_clients 
                    (id_to_window win) in w
              with Not_found -> w.w_top.w_parent in
            fvwm_action m.proto ("", action) w
          with
            e -> Printf.printf "Exception %s in action for module %s"
                (Utils.printexn e) m.name; print_newline ()
    end;
  with
  | Exit -> ()
  | e ->
      Printf.printf "Error %s in module %s" (Utils.printexn e) m.name;
      print_newline ();
      kill_module m

and execute_fvwm_module proto name w context args =
  let name = Utils.find_in_path !module_path name in
  let (fvwm_to_app0,fvwm_to_app1) = pipe () in
  let (app_to_fvwm0,app_to_fvwm1) = 
    try pipe () with e -> close fvwm_to_app0; close fvwm_to_app1; raise e in
  let win = try
      let (c,w) = Wintbl.find w.w_screen.s_clients w.w_window in 
      window_to_id c.c_window with
      _ -> 0 in
  let pid = fork () in
  if pid > 0 then (* still in GwML *)
    begin
      close app_to_fvwm1;
      close fvwm_to_app0;
      set_nonblock fvwm_to_app1;
      set_nonblock app_to_fvwm0;
      let m = 
        {
          name = name;
          proto = proto;
          write_pipe = fvwm_to_app1;
          read_pipe = app_to_fvwm0;
          pipe_mask = all;
          pipe_queue = [];
          pipe_on = false;
          module_mask = 0xffffffff;
        } in
      fvwm_modules :=  m :: !fvwm_modules;
(*      set_nonblock fvwm_to_app1; *)
      set_close_on_exec fvwm_to_app1;
      set_close_on_exec app_to_fvwm0;
      Concur.Thread.add_reader app_to_fvwm0 (fun _ ->
          read_module_command m w)
    end
  else
  if pid = 0 then
    begin
      close app_to_fvwm0;
      close fvwm_to_app1;      
      let args = [ name; 
          string_of_file_descr app_to_fvwm1;
          string_of_file_descr fvwm_to_app0;
          config_file proto w;
          Printf.sprintf "%x" win;
          Printf.sprintf "%x" (int_of_context context) ] 
          @ args
      in 
      let args = Array.of_list args in
      (* for now, no args (diff between fvwm 1 and 2) *)
      execvp args.(0) args;
      close app_to_fvwm1;
      close fvwm_to_app0;
    end
  else
    begin
      close app_to_fvwm1;
      close app_to_fvwm0;
      close fvwm_to_app0;
      close fvwm_to_app1;
      failwith  "Fork failed"  
    end

        
let fvwm_func proto list w =
  let rec iter list =
    match list with
      [] -> ()
    | f :: tail -> fvwm_action proto f w; iter tail
  in
  iter list

let fvwm_RootKeys = ref []
let fvwm_WindowKeys = ref []
let fvwm_TitleKeys = ref ([]: bindings)



let modifiers mods =
  let m = ref 0 in
  for i = 0 to String.length mods - 1 do
    m := !m lor (match mods.[i] with
        'S' -> shiftMask
      | 'C' -> controlMask
      | 'M' -> mod1Mask
      | 'A' -> anyModifier
      | _ -> 0)
  done; !m

let fvwm_Button1Keys = ref []
let fvwm_Button2Keys = ref []
let fvwm_Button4Keys = ref []
let fvwm_Button6Keys = ref []
let fvwm_SidesKeys = ref []

let install_binding proto where key action grab =
  let key = key, (fvwm_action proto action), grab in
  for i = 0 to String.length where - 1 do
    match where.[i] with
      'R' -> fvwm_RootKeys := key :: !fvwm_RootKeys
    | 'W' -> fvwm_WindowKeys := key :: !fvwm_RootKeys
    | '1' -> fvwm_Button1Keys := key :: !fvwm_Button1Keys
    | '2' -> fvwm_Button2Keys := key :: !fvwm_Button2Keys
    | '4' -> fvwm_Button4Keys := key :: !fvwm_Button4Keys
    | '6' -> fvwm_Button6Keys := key :: !fvwm_Button6Keys
    | 'T' -> fvwm_TitleKeys := key :: !fvwm_TitleKeys
    | 'S' -> fvwm_SidesKeys := key :: !fvwm_SidesKeys
    | 'A' ->
        fvwm_RootKeys := key :: !fvwm_RootKeys;
        fvwm_WindowKeys := key :: !fvwm_RootKeys;
        fvwm_TitleKeys := key :: !fvwm_TitleKeys
    | _ -> ()
  done
  
  
type takewhat = TakeFeel | TakeTheme | TakeKeys
  
let use proto takewhat (file,args) =
  let taketheme = List.memq TakeTheme takewhat in
  let takefeel = List.memq TakeFeel takewhat in  
  let takekeys = List.memq TakeKeys takewhat in
  List.iter (fun arg ->
      match arg with
        Style (regexp, decorations) -> 
          if takefeel then begin
              Printf.printf "ADD STYLE FOR <%s>" regexp; print_newline ();
              add_std_options regexp decorations
            end
      | FvwmAuto time -> 
          if takefeel then begin
              auto_raise =:= true; 
              auto_raise_delay =:= float_of_int time /. 1000.
            end
      | WindowFont font -> 
          if taketheme then window_font =:= font
      | IconFont font -> if taketheme then icon_font =:= font
      | MenuFont font -> if taketheme then menu_font =:= font
      | DefaultColors (win_fg, win_bg, title_fg, title_bg) ->
          if taketheme then begin
              Printf.printf "Done"; print_newline ();
              window_background =:= win_bg;
              window_foreground =:= win_fg;
              menu_background =:= title_bg;
              menu_foreground =:= title_fg;
              title_background =:= title_bg;
              title_foreground =:= title_fg
            end
      | StdForeColor fg -> 
          if taketheme then begin
              window_foreground =:= fg;
              title_foreground =:= fg
            end
      | StdBackColor bg -> 
          if taketheme then begin
              window_background =:= bg;
              title_background =:= bg
            end
      | MenuForeColor fg -> if taketheme then menu_foreground =:= fg
      | MenuBackColor bg -> if taketheme then menu_background =:= bg
      | HiForeColor fg -> if taketheme then active_foreground =:= fg
      | HiBackColor fg -> if taketheme then active_background =:= fg
      | HilightColors (act_fg, act_bg) ->
          if taketheme then begin
              active_foreground =:= act_fg;
              active_background =:= act_bg;
            end
      | StickyColors (sticky_fg, sticky_bg) ->
          if taketheme then begin
              sticky_foreground =:= sticky_fg;
              sticky_background =:= sticky_bg;
            end
      | FvwmKey (key,where,mods,action) -> 
          if takekeys then 
            let m = modifiers mods in
            (try
                install_binding proto where (Key(keysym key, m))
                ("",action) (if m land anyModifier = 0 then true else false)
              with _ -> ())
      | Mouse (button, where, mods, action) -> 
          if takekeys then
            let m = modifiers mods in
            (try
                install_binding proto where (Button (button, m))
                ("",action) (if m land anyModifier = 0 then true else false)
              with _ -> ())
      | ModulePath path -> 
          let list = Utils.string_to_path path in
          module_path := list @ !module_path
      | PixmapPath path -> let list = Utils.string_to_path path in
          List.iter (add_to_path icon_path) list
      | IconPath path -> let list = Utils.string_to_path path in
          List.iter (add_to_path icon_path) list
      | OpaqueMoveSize size -> 
          if takefeel then opaque_move_size =:= size          
      | EdgeResistance (scrolling,moving) ->
          if takefeel then begin
              edge_scrolling_resist =:= scrolling;
              edge_moving_resist =:= float_of_int moving
            end
      | EdgeScroll (hperc,vperc) ->
          if takefeel then begin
              edge_scrolling_horiz =:= hperc;
              edge_scrolling_vertic =:= vperc;
            end
      | DeskTopSize geom -> ()
      | AddToFunc (func, actions) -> 
          let name = "fvwmrc:" ^ func in
          let func = 
            try
              let func = Hashtbl.find funs_table name in
              Hashtbl.remove funs_table name;
              func
            with
              Not_found -> (fun _ -> ())
          in
          Hashtbl.add funs_table name 
            (fun w -> fvwm_func proto (actions @ ["",RecDef func]) w)
      | AddToMenu (menu, actions) -> 
          let name = "fvwmrc:" ^ menu in
          let menu = 
            try 
              let menu = Hashtbl.find menus_table name in
              Hashtbl.remove menus_table name;
              menu
            with
              Not_found -> []
          in
          let actions = List.map (fun (string, action) ->
                let string, pixmap = get_pixmap string in
                match action with
                  FvwmMenu (name, action) -> string,
                    (ItemPixmap (FromFunction ("fvwm_menu_pixmap", 
                          createMenuIcon),false))::
                    pixmap, 
                    Menu (fvwm_menu name)
                | Popup name -> string, 
                    (ItemPixmap (FromFunction ("fvwm_menu_pixmap", 
                          createMenuIcon),false))::
                    pixmap, Menu (fvwm_menu name)
                | FvwmFunction name -> string, pixmap, Function (fvwm_function name)
                | Title -> string, (
                      (ItemForeground (fun () -> !!title_foreground)) ::
                      (ItemBackground (fun () -> !!title_background)) ::
                      pixmap),
                    Function (fvwm_action proto ("",action))
                | _ -> string, pixmap, Function (fvwm_action proto ("",action))
            ) actions in
          Hashtbl.add menus_table name (actions@menu)              
      | _ -> ()        
  ) args;
  ()

(* open Stddeco *)
  
(* XPM *)
 (* File generated from fvwm_close.xpm *)
let close_pixmap = 
  FromData ("fvwm95_close",
    (13,11,
      ([|Xtypes.RGB(0,0,0)
          ;Xtypes.RGB(49152,49152,49152)
        |]),
      (
        [| [|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;0;0;1;1;1;1;0;0;1;1|]
          ;[|1;1;1;1;0;0;1;1;0;0;1;1;1|]
          ;[|1;1;1;1;1;0;0;0;0;1;1;1;1|]
          ;[|1;1;1;1;1;1;0;0;1;1;1;1;1|]
          ;[|1;1;1;1;1;0;0;0;0;1;1;1;1|]
          ;[|1;1;1;1;0;0;1;1;0;0;1;1;1|]
          ;[|1;1;1;0;0;1;1;1;1;0;0;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
        |])))

  (* XPM *)
 (* File generated from fvwm_maximize.xpm *)
let maximize_pixmap = 
  FromData("fvwm95_maximize",
    (13,11,
      ([|Xtypes.RGB(0,0,0)
          ;Xtypes.RGB(49152,49152,49152)
        |]),
      (
        [| [|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;0;0;0;0;0;0;0;0;0;1;1|]
          ;[|1;1;0;0;0;0;0;0;0;0;0;1;1|]
          ;[|1;1;0;1;1;1;1;1;1;1;0;1;1|]
          ;[|1;1;0;1;1;1;1;1;1;1;0;1;1|]
          ;[|1;1;0;1;1;1;1;1;1;1;0;1;1|]
          ;[|1;1;0;1;1;1;1;1;1;1;0;1;1|]
          ;[|1;1;0;1;1;1;1;1;1;1;0;1;1|]
          ;[|1;1;0;1;1;1;1;1;1;1;0;1;1|]
          ;[|1;1;0;0;0;0;0;0;0;0;0;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
        |])))

  (* XPM *)
 (* File generated from fvwm_minimize.xpm *)
let minimize_pixmap = 
  FromData ("fvwm95_minimize",
    (13,11,
      ([|Xtypes.RGB(0,0,0)
          ;Xtypes.RGB(49152,49152,49152)
        |]),
      (
        [| [|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;0;0;0;0;0;0;1;1;1;1|]
          ;[|1;1;1;0;0;0;0;0;0;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
        |])))

  (* XPM *)
 (* File generated from fvwm_sysmenu.xpm *)
let sysmenu_pixmap = 
  FromData ("fvwm95_sysmenu",
    (13,11,
      ([|Xtypes.RGB(0,0,0)
          ;Xtypes.RGB(49152,49152,49152)
        |]),
      (
        [| [|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;0;0;0;0;0;1;1;1;1|]
          ;[|1;1;1;1;0;0;0;0;0;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
        |])))

  (* XPM *)
 (* File generated from fvwm_normalize.xpm *)
let normalize_pixmap = 
  FromData("fvwm95_normalize",
    (13,11,
      ([|Xtypes.RGB(0,0,0)
          ;Xtypes.RGB(49152,49152,49152)
        |]),
      (
        [| [|1;1;1;1;1;1;1;1;1;1;1;1;1|]
          ;[|1;1;1;1;0;0;0;0;0;0;1;1;1|]
          ;[|1;1;1;1;0;0;0;0;0;0;1;1;1|]
          ;[|1;1;1;1;0;1;1;1;1;0;1;1;1|]
          ;[|1;1;0;0;0;0;0;0;1;0;1;1;1|]
          ;[|1;1;0;0;0;0;0;0;1;0;1;1;1|]
          ;[|1;1;0;1;1;1;1;0;0;0;1;1;1|]
          ;[|1;1;0;1;1;1;1;0;1;1;1;1;1|]
          ;[|1;1;0;1;1;1;1;0;1;1;1;1;1|]
          ;[|1;1;0;0;0;0;0;0;1;1;1;1;1|]
          ;[|1;1;1;1;1;1;1;1;1;1;1;1;1|]
        |])))

let old_maximize_size = Wobenv.new_var ()  

let windows_init desc =
  desc#set_background "CadetBlue"

let windows_getsize desc = 
  let w = desc#wob in
  let g = w.w_geometry in
  g.x <- 0;
  g.y <- 0;
  g.width <- 14;
  g.height <- 18

let onclick desc f e =
  match e with
    WobButtonPress _ -> f desc#wob
  | _ -> ()


let move_resize w x y width height =
  let tw = w.w_top in
  let g = tw.w_geometry in
  g.x <-x ;
  g.y <- y;
  g.width <- width - 2 * g.border;
  g.height <- height - 2 * g.border;
  Wob.send_one tw WobMove;
  Wob.send_one tw (WobResize false)

let full_maximize w =
  let tw = w.w_top in
  let g = tw.w_geometry in
  let sw= tw.w_parent in
  let sg = sw.w_geometry in
  try
    let size = Wob.getenv tw old_maximize_size in
    match size with
      None -> Wob.setenv tw old_maximize_size 
          (Some (g.x,g.y,g.width,g.height));
        Wob.send w (WobMessage "full_screen");
        move_resize tw 0 0 sg.width sg.height
    | Some (x,y,dx,dy) ->
        Wob.setenv tw old_maximize_size None;
        Wob.send w (WobMessage "normal_size");
        move_resize tw x y dx dy
  with
    Not_found ->
      Wob.setenv tw old_maximize_size 
        (Some (g.x,g.y,g.width,g.height));
      Wob.send w (WobMessage "full_screen");
      move_resize tw 0 0 sg.width sg.height

      
let handle_width = ref 3
let fvwm_version = ref 95  
  

let handle_hihook desc e =
  match e with
    WobClientFocus t -> 
      if t then begin
          desc#set_foreground !!active_foreground;
          desc#set_background !!active_background
        end
      else
        begin
          desc#set_foreground !!window_foreground;
          desc#set_background !!window_background          
        end
  | _ -> ()          
  
let title_hihook desc e =
  match e with
    WobClientFocus t -> 
      if t then begin
          desc#set_foreground !!active_foreground;
          desc#set_background !!active_background
            end
      else
        begin
          desc#set_foreground !!title_foreground;
          desc#set_background !!title_background          
        end
  | _ -> ()          
  
let handle cursor =
  let w = Null.make_wob () in
  w#set_min_width !handle_width;
  w#set_min_height !handle_width;
  w#set_background !!window_background;
  w#set_cursor cursor;
  w#set_actions !fvwm_SidesKeys;
  w#set_mask (ButtonPressMask :: w#mask);
  w#add_hook (handle_hihook w);
  w

let fvwm_window sw c =
  let label = Label.make c.c_name in
  label#add_hook (Stddeco.name_update c label);
  label#set_min_height 17;
  label#set_font !!window_font;
  label#set_background !!title_background;    
  label#set_foreground !!title_foreground;
  label#set_justified Center;
  label#add_hook (title_hihook label);
  label#set_extensible_width 3;

  let middle = Null.make_wob () in
  middle#set_background !!title_background;

  let sysmenu = 
    Pixmap.make sw (if !fvwm_version = 95 then
        sysmenu_pixmap
      else
      try FromFile (get_title_icon c) with _ -> sysmenu_pixmap)
  in
    
  let button1 = Pixmap.make sw minimize_pixmap in
  let button2 = Pixmap.make sw maximize_pixmap in
  let button3 = Pixmap.make sw close_pixmap in
  
  sysmenu#set_background !!title_background;  
  button1#set_background !!title_background;  
  button2#set_background !!title_background;
  button3#set_background !!title_background;

  button2#add_hook (fun e ->
      match e with
        WobMessage "full_screen" -> button2#set_pixmap normalize_pixmap
      | WobMessage "normal_size" -> button2#set_pixmap maximize_pixmap
      | _ -> ());
  
  sysmenu#set_borderwidth 1;
  button1#set_borderwidth 1;
  button2#set_borderwidth 1;
  button3#set_borderwidth 1;

  label#set_mask (ButtonPressMask :: label#mask);
  sysmenu#set_mask (ButtonPressMask :: sysmenu#mask);
  middle#set_mask (ButtonPressMask :: middle#mask);
  button1#set_mask (ButtonPressMask :: button1#mask);
  button2#set_mask (ButtonPressMask :: button2#mask);
  button3#set_mask (ButtonPressMask :: button3#mask);

  sysmenu#set_actions !fvwm_Button1Keys;
  label#set_actions (!fvwm_TitleKeys);
  middle#set_actions (!fvwm_TitleKeys);
  button1#set_actions !fvwm_Button6Keys;  
  button2#set_actions !fvwm_Button4Keys;  
  button3#set_actions !fvwm_Button2Keys;  
  
  let bar = Bar.make Horizontal [| Wob.desc sysmenu;
      (*Wob.desc middle;*) Wob.desc label; 
      Wob.desc button1; Wob.desc button2; Wob.desc button3 |] in

  let handle1 = handle (FontCursor XC.xc_top_side) in
  let handle2 = handle (FontCursor XC.xc_left_side) in
  let handle3 = handle (FontCursor XC.xc_right_side) in
  let handle4 = handle (FontCursor XC.xc_bottom_side) in

  Client.window_borderwidth =:= 0;
  let top = Bar.make Vertical [| handle1; Wob.desc bar |] in
  ([Stddeco.c_hook c; icon_manager_hook c], 
    Some handle2, Some handle3, Some (Wob.desc top) , Some handle4)

let install_bindings () =
  Printf.printf "Fvwm.install_bindings: disabled ... ";
  print_newline ()
  (* 
  title_actions := !fvwm_TitleKeys;
  screen_actions := !fvwm_RootKeys;
  window_actions := !fvwm_WindowKeys
*)

(* With Afterstep, we need to create a AF_UNIX server ! 
NAME in _AS_MODULE_SOCKET
  *)
let afterstep_dir = 
  Filename.concat Utils.homedir "GNUstep/Library/AfterStep/non-configurable"

let afterstep_property = "_AS_MODULE_SOCKET"
  
let socket_name = Filename.concat afterstep_dir 
    (Printf.sprintf "connect.%s:0" !dpyname)

let module_accept s w=
  let fd,_ = accept s in
  Unix.set_close_on_exec fd;
  Printf.printf "Accepting connection"; print_newline ();
  let m = 
    {
      name = "Afterstep module";
      proto = Afterstep;
      write_pipe = fd;
      read_pipe = fd;
      pipe_mask = all;
      pipe_queue = [];
      pipe_on = false;
      module_mask = 0xffffffff;
    } in
  fvwm_modules :=  m :: !fvwm_modules;
  Concur.Thread.add_reader fd (fun _ ->
      read_module_command m w)

let reset () = 
  Hashtbl.clear menus_table;
  Hashtbl.clear funs_table
  
let module_listen w =
  Log.catch "Fvwm.module_listen: %s" (fun _ ->
      let s = Unix.socket PF_UNIX SOCK_STREAM 0 in
      if Sys.file_exists socket_name then Unix.unlink socket_name;
      Unix.bind s (ADDR_UNIX socket_name);
      Unix.listen s 254;
      Unix.set_nonblock s;
      Unix.set_close_on_exec s;
      Concur.Thread.add_reader s (fun _ -> module_accept s w);
      let atom = X.internAtom display afterstep_property false in
      X.changeProperty display w.w_top.w_parent.w_window 
        PropModeReplace atom XA.xa_string 1 socket_name
  )
}
