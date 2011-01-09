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

module type Make_sig = 
  sig
    type text
    and line
    and 'a tree_desc =
      { mutable tree_nlines: int;
        mutable tree_width: int;
        mutable tree_height: int;
        mutable tree_parts: 'a array;
        mutable tree_up: tree tree_desc;
        mutable tree_pos: int;
        mutable tree_modified: bool;
        mutable line_height: int;
        mutable line_width: int;
        mutable tree_text: text }
    and tree = | Parts of tree tree_desc | Lines of line tree_desc
    and redraw = | NoRedraw | Redraw of int * int * int * int | TotalRedraw
    val modify : 'a tree_desc -> unit
    
    class t :
      WX_types.container ->
      tree tree_desc ->
      WX_types.base_attributes list ->
      object
        inherit WX_object.t
        
        method set_text : tree tree_desc -> unit
          val mutable redraw_area : redraw
          val mutable text : tree tree_desc
          val mutable widgets : WX_types.contained list
          val mutable font : WX_types.font
        end
      val make_text : text -> line array -> tree tree_desc
end


  
type item =
  String of item_attr list * int
| RealString of item_attr list * string
| Widget of contained array

and item_attr =
  Font of font
| Foreground of color
| Background of color
  
module type Text =
  sig
      type line 
      and text
      val representation : text -> line -> string * int
      val items : text -> line -> item array
  end


module Make(Text: Text)  = struct
    
    open Text
    
    type text = Text.text
    type line = Text.line
    
    type 'a tree_desc = {
        mutable tree_nlines : int;
        mutable tree_width : int;
        mutable tree_height : int;
        mutable tree_parts : 'a array;
        mutable tree_up : tree tree_desc;
        mutable tree_pos : int;
        mutable tree_modified : bool;
        mutable line_height : int;
        mutable line_width : int;    
        mutable tree_text : text;
      }
    
    and tree =
      Parts of tree tree_desc
    | Lines of line tree_desc
    
    type redraw =
      NoRedraw
    | Redraw of int * int * int * int
    | TotalRedraw
    
    let rec get_font font attrs =
      match attrs with
        [] -> font
      | (Font font) :: _ -> font
      | _ :: tail -> get_font font tail

    let rec get_attrs ((fg,bg,font) as attrs) list =
      match list with
        [] -> attrs
      | a :: tail ->
          get_attrs (match a with
              Foreground fg -> (fg.c_pixel,bg,font)
            | Background bg -> (fg,bg.c_pixel,font)
            | Font font -> (fg,bg,font)
          ) tail
          
    let modify tree =  
      let rec modify2 tree = 
        if not tree.tree_modified then 
          (tree.tree_modified <- true; modify2 tree.tree_up)
      in
      if not tree.tree_modified then
        (tree.tree_modified <- true; modify2 tree.tree_up)
    
    class t parent (stext : tree tree_desc) attributes =
      object (self)
        val mutable font = parent#default_font
            
            inherit WX_object.t parent attributes as super
        
        val mutable text =  stext
        val mutable redraw_area = TotalRedraw
        val mutable widgets = []  
        initializer self#set_text stext
        
        method size_request =     
          let sz = szhints in
          if not w.w_size_modified  || sz.comp_timestamp = s.s_timestamp then sz else 
            begin
              widgets <- [];
              sz.comp_timestamp <- s.s_timestamp;
              let rec size_request text i y =
                try
                  match text.tree_parts.(i)  with
                    Parts t -> parts_size t y
                  | Lines t -> lines_size t y
                with Not_found ->
                    size_request text i y
              
              and parts_size text y =
                let parts = text.tree_parts in
                let dx = ref 0 in
                let dy = ref 0 in
                let y = ref y in
                let dn = ref 0 in
                for i = 0 to Array.length parts - 1 do
                  let (w,h,n) = size_request text i !y in
                  dx := max !dx w;
                  dy := !dy + h;
                  y := !y + h;
                  dn := !dn + n;
                done;
                let (dx,dy, nlines) = !dx, !dy, !dn in
                text.tree_width <- dx;
                text.tree_height <- dy;
                text.tree_nlines <- nlines;
                dx, dy, nlines
              
              and lines_size text y =
                let lines = text.tree_parts in
                let width = ref 0 in
                let n = Array.length lines in
                for i = 0 to n - 1 do
                  let _ = representation text.tree_text lines.(i) in
                  let line = items text.tree_text lines.(i) in
                  let ni = Array.length line in
                  let dx = ref 0 in
                  let dy = ref 0 in
                  let pos = ref 0 in
                  for j = 0 to ni - 1 do
                    match line.(j) with
                      String (attr, len) ->
                        let font = get_font font attr in
                        dx := !dx + (len * font.font_width);
                        dy := max !dy font.font_height
                    | RealString (attr, str) ->
                        let font = get_font font attr in
                        dx := !dx + (String.length str * font.font_width);
                        dy := max !dy font.font_height
                    | Widget w ->
(* Multiple displays of a single text is not implememeted *)
                        let w = w.(0) in
                        let sz = w#size_request in
                        if not (List.memq w widgets) then
                          widgets <- w :: widgets;
                        dy := max !dy sz.requested_height;
                        dx := !dx + sz.requested_width;
                  done;
                  if !dy <> text.line_height then
                    if n = 1 || i = 0 then text.line_height <- !dy else
(* Problem, this widget requires a greater height than expected.
              Cut the line accordingly *)
                      begin
                        let before = Array.sub lines 0 i in
                        let after = Array.sub lines i (n-i) in
                        let rec newtree =
                          {
                            tree_nlines = text.tree_nlines;
                            tree_width = 0;
                            tree_height = 0;
                            tree_parts = [| Lines {
                                tree_nlines = i;
                                tree_width = 0;
                                tree_height = 0;
                                tree_parts = before;
                                tree_up = newtree;
                                tree_pos = 0;
                                tree_modified = true;
                                line_height = 0;
                                line_width = 0;
                                tree_text = text.tree_text;      
                              };
                              Lines {
                                tree_nlines = n-i;
                                tree_width = 0;
                                tree_height = 0;
                                tree_parts = after;
                                tree_up = newtree;
                                tree_pos = 1;
                                tree_modified = true;
                                line_height = 0;
                                line_width = 0;
                                tree_text = text.tree_text;      
                              }|];
                            tree_up = text.tree_up;
                            tree_pos = text.tree_pos;
                            tree_modified = true;
                            line_height = 0;
                            line_width = 0;
                            tree_text = text.tree_text;
                          }
                        in
                        text.tree_up.tree_parts.(text.tree_pos) <- Parts newtree;
                        modify text;
                        raise Not_found
                      end;
                    width := max !width !dx;
                done;
                text.line_width <- !width;
                let couple = !width, n * text.line_height, n in
                let (dx,dy, nlines) = couple in
                text.tree_width <- dx;
                text.tree_height <- dy;
                text.tree_nlines <- nlines;
                couple
              in
              let (dx,dy,_) = parts_size text 0 in
              sz.requested_width <- dx + 2 * w.w_ipad_x;
              sz.requested_height <- dy + 2 * w.w_ipad_y;
              w.w_size_timestamp <- s.s_timestamp;
              sz
            end
        
        method size_allocate x y dx dy =
          let g = w.w_geometry in
          let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
          w.w_size_modified <- false;
          super#size_allocate x y dx dy;
          if modified then
            begin
              let rec iter text y =
                match text with
                  Parts text -> iter_parts text y
                | Lines text -> iter_lines text y 
              
              and iter_parts text y =
                let parts = text.tree_parts in
                let y = ref y in
                for i = 0 to Array.length parts - 1 do
                  let height = iter parts.(i) !y in
                  y := !y + height
                done;
                text.tree_height
              
              and iter_lines text y =
                let lines = text.tree_parts in              
                let n = Array.length lines in
                for i = 0 to n - 1 do
                  let _ = representation text.tree_text lines.(i) in
                  let line =  items text.tree_text lines.(i) in
                  let ni = Array.length line in
                  let dx = ref w.w_ipad_x in
                  for j = 0 to ni - 1 do
                    match line.(j) with
                      String (attr, len) ->
                        let font = get_font font attr in
                        dx := !dx + (len * font.font_width)
                    | RealString (attr, str) ->
                        let font = get_font font attr in
                        dx := !dx + (String.length str * font.font_width)
                    | Widget w ->
                        let w = w.(0) in
                        let sz = w#size_request in
                        w#size_allocate !dx (y+ i * text.line_height) 
                        sz.requested_width sz.requested_height;
                        dx := !dx + sz.requested_width
                  done
                done;
                text.tree_height              
              in
              let _ = iter_parts text w.w_ipad_y in ()
            end
        
        
        method wait_refresh clear x y dx dy =
          if x=0 && y=0 && dx=0 && dy=0 then redraw_area <- TotalRedraw
          else
            redraw_area <- 
              (match redraw_area with
                NoRedraw -> Redraw (x,y,x+dx,y+dy)
              | Redraw (x',y',xx,yy) -> Redraw (min x x', min y y',
                    max xx (x+dx),  max yy (y+dy))
              | TotalRedraw -> TotalRedraw);
          super#wait_refresh clear x y dx dy;

(* Direct call to #refresh is not allowed. Use #wait_refresh instead *)
        
        method refresh =
          if s.s_timestamp > w.w_refresh_timestamp && not (w.w_window == noWindow) then
            begin
(* clear the area which will be redrawn *)
              w.w_refresh_timestamp <- s.s_timestamp;
              let g = w.w_geometry in
              self#draw_relief;
              let (x,y,xx,yy) = 
                match redraw_area with
                  NoRedraw -> 0,0,0,0
                | Redraw (x,y,xx,yy) ->
                    X.clearArea s.s_display w.w_window x y (xx-x) (yy-y) false;
                    x,y,xx,yy
                | TotalRedraw ->
                    X.clearArea s.s_display w.w_window 0 0 0 0 false;
                    g.x, g.y, g.x + g.width, g.y + g.height
              in
              redraw_area <- NoRedraw;
(* Now, redraw what must be redrawn *)
              let attrs_all = 
                ( w.w_foreground.c_pixel,
                  w.w_background.c_pixel,
                  font) in                        
              
              let rec draw num text cury =
                match text with
                  Parts tree -> draw_parts num tree cury 
                | Lines tree -> draw_lines num tree cury
              
              and draw_parts num text cury =
                if cury + text.tree_height < y then text.tree_height else
                if cury > yy then raise Exit else
(* We must draw parts of this text *)            
                let parts = text.tree_parts in
                let y = ref cury in
                let n = Array.length parts in
                for i = 0 to (n-1) do
                  let text = parts.(i) in
                  let h = draw (num+1) text !y in
                  y := !y + h
                done;
                text.tree_height
              
              and draw_lines num text cury =
                if cury + text.tree_height < y then text.tree_height else
                if cury > yy then raise Exit else
(* We must draw parts of this text *)            
                let lines = text.tree_parts in
                let from_line =
                  if cury < y then (y - cury) / text.line_height else 0 in
                let to_line =
                  if cury + text.tree_height > yy then
                    (yy - cury) / text.line_height
                  else (Array.length lines - 1)
                in
(* Draw each line *)
                for n = from_line to to_line do
                  let repr, pos = representation text.tree_text lines.(n) in
                  let line = items text.tree_text lines.(n) in
                  let x = ref w.w_ipad_x in
                  let nn = Array.length line in
                  for i = 0 to nn - 1 do
                    let item = line.(i) in
                    match item with
                      Widget w -> 
                        let w = w.(0) in
                        x := !x + w#width
(* do not redraw widget since they receive their own expose events *)
                    | String (attrs, len) ->
                        let fg,bg,font = get_attrs attrs_all attrs in
                        let gc = GCCache.get3 s.s_gcs fg bg font.font_id in 
(* don't care about attributes for now *)
                        Xlib.drawSubString s.s_display w.w_window gc
                          !x (cury+ text.line_height * n + font.font_ascent)
                        repr pos len;
                        x := !x + len * font.font_width
                    | RealString (attrs, str) ->
                        let fg,bg,font = get_attrs attrs_all attrs in
                        let gc = GCCache.get3 s.s_gcs fg bg font.font_id in 
                        Xlib.drawString s.s_display w.w_window gc
                          !x (cury+ text.line_height * n + font.font_ascent)
                        str;
                        x := !x + String.length str * font.font_width
                  done
                done;
                text.tree_height
              in
        (* We start with y = 0 *)
              (try let _ = draw_parts 1 text w.w_ipad_y in () with Exit -> ());
            end;
    

(* 't' is a simple text (an array of lines (array) of text items (array) *)  
        method set_text stext =
          text <- stext; 
          w.w_size_timestamp <- -1;
          self#wait_resize;
          self#wait_refresh false 0 0 0 0
        
        method iter f = List.iter f widgets
        
        method name = "text"
  end
  
  
let make_text text lines = 
  let nlines = Array.length lines in
  let rec tree = {
      tree_nlines = nlines;
      tree_width = 0;
      tree_height = 0;
      tree_parts = [| Lines {
          tree_nlines = nlines;
          tree_width = 0;
          tree_height = 0;
          tree_parts = lines;
          tree_up = tree;
          tree_pos = 0;
          tree_modified = true;
          line_height = 0;
          line_width = 0;
          tree_text = text;      
        }|];
      tree_up = tree;
      tree_pos = 0;
      tree_modified = true;
      line_height = 0;
      line_width = 0;
      tree_text = text;
    }
  in tree
  
end

  
module WidgetText = struct
    type text = unit
    type line = item array
    let representation _ _ = "",0
    let items _ items = items
  end
  
module WidgetTree = Make(WidgetText)
  
class with_widgets parent text attributes =
  let tree = WidgetTree.make_text () text in
  object
  inherit WidgetTree.t parent tree attributes as super
  method set_widgets text = super#set_text (WidgetTree.make_text () text)
end

module SimpleText = struct
    type text = string
    type line = int * (item array)
    let representation s (start,_) = s,start
    let items _ (_,items) = items
  end
  
module SimpleTree = Make(SimpleText)
  
let make_simple_text s = 
  let len = String.length s in
  let rec iter start t =
    try
      let fin = String.index_from s start '\n' in
      iter (fin+1) ((start, [|String ([], fin - start)|]) :: t)
    with
      _ ->
        (start, [|String ([], len - start)|]) :: t
  in
  let lines = Array.of_list (List.rev (iter 0 [])) in
  SimpleTree.make_text s lines

let file_to_stext filename =   
  let ic = open_in filename in
  let s = Utils.read_string ic in
  let _ = close_in ic in
  make_simple_text s


class t a b c = object
  inherit SimpleTree.t a b c
end
  
class of_file parent filename attributes =
  
  object(self)
  
  inherit SimpleTree.t parent (file_to_stext filename) attributes
  
  method set_file filename =
    self#set_text (file_to_stext filename)
end


class of_string parent lines attributes =
  
  object(self)
  
  inherit SimpleTree.t parent (make_simple_text lines) attributes
  
  method set_lines lines =
    self#set_text (make_simple_text lines)
end

