open Xtypes
  open Options
open WX_text
open Stdconfig
open Gwml
open WX_types
open WX_filesel
  
let wx_display = new WX_display.t ""
let _ =
  wx_display#broken (fun _ -> ())
  
let wx_root = new WX_root.t wx_display 0

let maxlen = 30
  
let last_focus = ref (new WX_ledit.t wx_root#container "" [])
let min_width = 50

let rstring s = RealString ([], s)
let null = rstring "    "

let boolean_selector text option =
  let record = {
      WX_selector.labels = [| "True"; "False" |];
      WX_selector.valides = [||];
      WX_selector.current = (if !!option then 0 else 1);
      WX_selector.change_hook = []
    }
  in
  record.WX_selector.change_hook <- [fun _ -> 
      option =:= (record.WX_selector.current = 0)];
  Widget [| (new WX_selector.t text#container wx_root record [
        MinWidth min_width])#contained |]

  
let help_button text option =
  let button = new WX_button.t text#container [] in
  let label = new WX_label.t button#container "Help" [] in
  button#container_add label#contained;
  button#set_action (fun _ ->
      let dialog = new WX_dialog.t wx_root (get_help option) [] in
      dialog#add_button "OK" (fun _ -> dialog#destroy);
      dialog#show);
  Widget [| button#contained |]

let boolean_option text option =
  let name = shortname option in
  let name = name ^ (String.make (
        max 0 (maxlen - String.length name)) ' ') ^ ": "in
  [| null; rstring name; boolean_selector text option; null; help_button text option |]
  
let generic_option text option to_string from_string min_width =
  let name = shortname option in
  let name = name ^ (String.make (
        max 0 (maxlen - String.length name)) ' ') ^ ": "in  
  let ledit = new WX_ledit.t text#container (to_string !!option) 
    [MinWidth min_width] in
  ledit#configure [Bindings [
      FocusIn, (fun _ -> last_focus := ledit);
      ButtonPress, (fun _ -> ledit#focus);
    ]];
  ledit#set_justification Left;
  ledit#add_subject (fun _ ->
      try
        option =:= from_string ledit#string;
        ledit#configure [Background !default_background]
      with _ ->
          ledit#configure [Background "red"]
  );
  [| null; rstring name; Widget [| ledit#contained; |]; null; help_button text option |]
  
let add_to_option text option to_string from_string min_width =
  let name = shortname option in
  let name = name ^ (String.make (
        max 0 (maxlen - String.length name)) ' ') ^ ": "in  
  let ledit = new WX_ledit.t text#container ""
    [MinWidth min_width] in
  ledit#configure [Bindings [
      FocusIn, (fun _ -> last_focus := ledit);
      ButtonPress, (fun _ -> ledit#focus);
    ]];
  ledit#set_justification Left;
  let button = new WX_button.with_label text#container "Add" [] in
  button#set_action (fun _ ->
      option =:= (from_string ledit#string) :: !!option;
      ledit#set_string "");
  [| null; rstring name; Widget [| ledit#contained |]; Widget [|button#contained|]; 
    null; help_button text option |]

let int_option text option =
  generic_option text option string_of_int int_of_string min_width

let float_option text option =
  generic_option text option string_of_float float_of_string min_width

  (*
let list_option text option =
  let name = shortname option in
  let name = name ^ (String.make (
        max 0 (maxlen - String.length name)) ' ') ^ ": "in
  let vbar = new WX_bar.v text#container [] in
  vbar#container_add_s (List.map (fun s ->
        let label = new WX_label.t vbar#container s [] in
        label#contained
    ) !!option);
  [| null; rstring name; Widget [| vbar#contained |]; null; help_button text option |]
    *)

let path_option text option =
  add_to_option text option  Utils.string_to_filename Utils.filename_to_string 200
  
let id x = x
  
let string_option text option =
  generic_option text option id id min_width
  
let string s = [| RealString ([], s) |]
  
let configurator w =
  let scr = w.w_screen.s_scr in
  let wx_appli = new WX_appli.t wx_root  
      [
      MaxWidth scr.scr_width; MaxHeight (scr.scr_height-30);
      ] in
  wx_appli#configure [Bindings [
      FocusIn, (fun _ -> 
          if (X.getInputFocus wx_appli#display).gif_win == wx_appli#window then
            !last_focus#focus);
      ButtonPress, (fun _ -> !last_focus#focus);
      Key (XK.xk_q,0), (fun () -> 
          wx_appli#destroy;
          if !Gwml_args.batch_mode then exit 0)
    ]];
  let attributes = [Relief ReliefRaised; Foreground "red"; Background "yellow"]
  in
  let hbar = new WX_bar.h wx_appli#container [] in
  let adx = new WX_adjust.t () in
  let ady = new WX_adjust.t () in
  let viewport = new WX_viewport.t hbar#container adx ady 
    [MinHeight 50; MinWidth 50;ExpandX true; ExpandY true] in
  let scale = new WX_scrollbar.v hbar#container ady [Background "black"] in
  let text = new WX_text.with_widgets viewport#container [||] 
      ((MinWidth 400) :: (MinHeight 400) :: (ExpandX true) :: (
        ExpandY true) :: (IpadX 5) :: attributes) in
  viewport#container_add text#contained;
  hbar#container_add_s [viewport#contained; scale#contained];
  let file_menu = [|
      "Save", (fun _ -> Options.save ());
      "Quit without Saving", (fun _ -> wx_appli#destroy;
          if !Gwml_args.batch_mode then exit 0;
          );
    |]
  in
  let simple_text = [|
      string "GwML Configurator";
      string "";
      string "Boolean options:";
      string "";
      boolean_option text auto_raise;
      boolean_option text Sound.use_sound;
      boolean_option text Sound.use_local_player;
      boolean_option text debug; 
      boolean_option text do_animation;
      boolean_option text pan_on_click;
      boolean_option text opaque_move;
      boolean_option text confine_move;
      boolean_option text group_iconification;
      string "";
      string "Integer options:";
      string "";
      int_option text Gradients.gradient_levels;
      int_option text x_maximize_use;
      int_option text y_maximize_use;
      int_option text Client.window_borderwidth;
      string "";
      string "Float options:";
      string "";
      float_option text Wob.double_click_delay;
      string "";
      string "Decoration options:";
      string "";
      string "  Title options:";
      string "";
      string_option text title_background;
      string_option text title_foreground;
      string_option text title_font;
      string "";
      string "  Active Title options (focused):";
      string "";
      string_option text active_background;
      string_option text active_foreground;
      string_option text active_font;
      string "";
      path_option text graphics_path;
    |] in
  text#set_widgets simple_text;
  wx_appli#container_add hbar#contained;
  wx_appli#setWM_CLASS "GwML_aux" "configurator";
  wx_appli#setWM_NAME "GwML configurator";
  wx_appli#add_menu "File" file_menu;
  wx_appli#add_separator;
  wx_appli#show;
  Printf.printf "Configurator started";
  print_newline ()
  
let _ =
  define_action "configurator" (Function configurator)