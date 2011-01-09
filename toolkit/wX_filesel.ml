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
open WX_types
  
type info = {
    filter : string; (* a directory + a regexp *)
    current_selection : string;
    predicat : (info -> bool);
    mutable action : (string -> unit);
    mutable cancel : (unit -> unit);
  }


class t root info attributes =
  let top = new WX_wmtop.t root (
      (MinWidth 300)::(MinHeight 300):: attributes) in
  let vbar = new WX_bar.v top#container [ExpandX true; ExpandY true; IpadY 10] in
  let label_filer = new WX_label.t vbar#container "Filter" [] in
  let ledit_filter = new WX_ledit.t vbar#container info.filter [ExpandX true;IpadY 4] in
  let label_dir = new WX_label.t vbar#container "Directories" [] in
  
  let hbar_files = new WX_bar.h vbar#container [ExpandY true; ExpandX true; MaxHeight 200] in
  let adx = new WX_adjust.t () in
  let ady_dir = new WX_adjust.t () in
  let dir_port = new WX_viewport.t hbar_files#container adx ady_dir [] in
  let dir_bar = new WX_bar.v dir_port#container [ExpandX true] in
  let dir_scroll = new WX_scrollbar.v hbar_files#container ady_dir [] in
  let adx = new WX_adjust.t () in
  let ady_file = new WX_adjust.t () in
  let file_port = new WX_viewport.t hbar_files#container adx ady_file [] in
  let file_scroll = new WX_scrollbar.v hbar_files#container ady_file [] in
  let file_bar = new WX_bar.v file_port#container [ExpandX true] in
  
  let label_sel = new WX_label.t vbar#container "Selection" [] in
  let ledit_sel = new WX_ledit.t vbar#container info.current_selection [ExpandX true; IpadY 4] in
  
  let hbar_buttons = new WX_bar.h vbar#container [ExpandX true; IpadY 10] in
  let ok_button = new WX_button.t hbar_buttons#container [] in
  let filter_button = new WX_button.t hbar_buttons#container [] in
  let cancel_button = new WX_button.t hbar_buttons#container [] in
  let ok_label = new WX_label.t ok_button#container "OK" [MinWidth 100; IpadY 5] in
  let filter_label = new WX_label.t filter_button#container "Filter" [MinWidth 100; IpadY 5] in
  let cancel_label = new WX_label.t cancel_button#container "Cancel" [MinWidth 100; IpadY 5] in
  let last_focus = ref ledit_sel in  
  let _ =
    top#setWM_NAME "File selection dialog";
    top#configure [Bindings [
        FocusIn, (fun _ -> 
            if (X.getInputFocus top#display).gif_win == top#window then
              !last_focus#focus);
        ButtonPress, (fun _ -> !last_focus#focus);    
        ]];
    ledit_sel#configure [Bindings [
        FocusIn, (fun _ -> last_focus := ledit_sel);
        ButtonPress, (fun _ -> ledit_sel#focus);
        ]];
    ledit_filter#configure [Bindings [
        FocusIn, (fun _ -> last_focus := ledit_filter);
        ButtonPress, (fun _ -> ledit_filter#focus);        
        ]];
    ok_label#set_justification WX_types.Center;
    filter_label#set_justification WX_types.Center;
    cancel_label#set_justification WX_types.Center;
    top#container_add vbar#contained;
    vbar#container_add_s [
      label_filer#contained; ledit_filter#contained; label_dir#contained;
      hbar_files#contained; label_sel#contained; ledit_sel#contained;
      hbar_buttons#contained];
    hbar_buttons#container_add_s [
      ok_button#contained; 
      (new WX_port.t hbar_buttons#container [ExpandX true])#contained;
      filter_button#contained; 
      (new WX_port.t hbar_buttons#container [ExpandX true])#contained;
      cancel_button#contained];
    ok_button#container_add ok_label#contained;
    filter_button#container_add filter_label#contained;
    cancel_button#container_add cancel_label#contained;
    hbar_files#container_add_s [
      dir_port#contained; dir_scroll#contained;
      file_port#contained; file_scroll#contained];
    file_port#container_add file_bar#contained;
    dir_port#container_add dir_bar#contained;
    ok_button#set_action (fun () -> 
        if info.predicat info then
          begin
            top#hide;
            info.action ledit_sel#string;
          end
    );
    cancel_button#set_action (fun () -> top#hide; info.cancel ());
  in
  let rec filter () =
    let info_filter = ledit_filter#string in
    let dirname = Filename.dirname info_filter in
    let basename = Filename.basename info_filter in
    let list = Utils.list_dir dirname in
    let regexp = Utils.glob_to_regexp basename in
    let regexp = Str.regexp regexp in
    let (dirs,files) = List.fold_left (fun (dirs,files) name ->
          try
            let stat = Unix.stat (Filename.concat dirname name) in
            match stat.Unix.st_kind with
              Unix.S_DIR -> name :: dirs, files
            | _ ->         
                dirs,
                if Str.string_match regexp name 0 then name :: files else files
          with _ ->
              dirs,
              if Str.string_match regexp name 0 then name :: files else files
      ) ([],[]) list in
    let dirs = Array.of_list (List.map (fun name ->
            let button = new WX_button.t dir_bar#container [ExpandX true] in
            let label = new WX_label.t button#container name [ExpandX true] in
            button#container_add label#contained;
            button#set_action (fun _ ->
                if not (name = "." || (name = ".." && dirname = "/")) then
                  begin
                    let dirname = 
                      if name = ".." then Filename.dirname dirname
                      else Filename.concat dirname name
                    in
                    try
                      Unix.access dirname [Unix.X_OK;Unix.R_OK];
                      ledit_filter#set_string (
                        Filename.concat dirname  basename);
                      filter ();
                    with e -> 
                        label#set_string (name ^ " (denied)")
                  end
            );
            button#contained
        ) (Sort.list (<) dirs)) in
    let files = Array.of_list (List.map (fun name ->
            let button = new WX_button.t file_bar#container [ExpandX true] in
            let label = new WX_label.t button#container name [ExpandX true] in
            button#container_add label#contained;
            button#set_action (fun _ ->
                ledit_sel#set_string (Filename.concat dirname name));
            button#contained
        ) (Sort.list (<) files)) in
    Utils.catchexn "dir_bar" (fun _ -> dir_bar#set_items dirs);
    Utils.catchexn "file_bar" (fun _ -> file_bar#set_items files);
    ady_dir#set_pos 0 1;
    ady_file#set_pos 0 1;
  in
  let _ = 
    filter ();
    filter_button#set_action (fun () -> filter ());
    ledit_filter#configure [Bindings [Key(XK.xk_Return, 0), filter]];
  in
  object (self)
    inherit WX_deleg.wx_object (top :> WX_object.t)
    inherit WX_deleg.wmtop top
end