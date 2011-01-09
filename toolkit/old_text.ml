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

type simple_text = tree_item array array
  
and text = {
    mutable tree_nlines : int;
    mutable tree_width : int;
    mutable tree_height : int;
    mutable tree_desc : tree_desc;
    mutable tree_up : text option;
    mutable tree_attrs : line_attr list;
  }
  
and tree_desc =
  Parts of text array
| Text of tree_attr * simple_text
  
and tree_attr = {
    mutable line_height : int;
    mutable line_width : int;
  }
  
and tree_item =
  String of line_attr list * string
| Widget of contained
  
and line_attr =
  LineFont of font
| LineForeground of color
| LineBackground of color
| LineJustified of justified
  
type redraw =
  NoRedraw
| Redraw of int * int * int * int
| TotalRedraw

let make_text (t: simple_text) = 
  {
    tree_nlines = Array.length t;
    tree_width = 0;
    tree_height = 0;
    tree_desc = Text ({ line_height = 0;  line_width = 0;}, t);
    tree_up = None;
    tree_attrs = [];
  }

let make_simple_text s = 
  let len = String.length s in
  let rec iter start t =
    try
      let fin = String.index_from s start '\n' in
      iter (fin+1) ([|String ([], 
            String.sub s start (fin - start))|] :: t)
    with
      _ ->
        [|String ([], 
            if len = start then "" else String.sub s start (len - start))|]
          :: t
  
  in
  Array.of_list (List.rev (iter 0 []))

class t parent stext attributes =
  object (self)
  val mutable font = parent#default_font
      
      inherit WX_object.t parent attributes as super
  
  val mutable text = {
      tree_nlines = 0;
      tree_width = 0;
      tree_height = 0;
      tree_desc = Parts [||];
      tree_up = None;
      tree_attrs = [];
    }  
  
  val mutable redraw_area = TotalRedraw
  val mutable widgets = []  
  initializer self#set_text stext
  
  method size_request =     
    let sz = szhints in
    if not w.w_size_modified  || sz.comp_timestamp = s.s_timestamp then sz else 
      begin
        widgets <- [];
        sz.comp_timestamp <- s.s_timestamp;
        let rec size_request text y =
          let couple =
            match text.tree_desc with
              Parts parts ->
                let dx = ref 0 in
                let dy = ref 0 in
                let y = ref y in
                let dn = ref 0 in
                for i = 0 to Array.length parts - 1 do
                  let (w,h,n) = size_request parts.(i) !y in
                  dx := max !dx w;
                  dy := !dy + h;
                  y := !y + h;
                  dn := !dn + n;
                done;
                !dx, !dy, !dn
            | Text (attr,lines) ->
                let width = ref 0 in
                let n = Array.length lines in
                try
                  for i = 0 to n - 1 do
                    let line = lines.(i) in
                    let ni = Array.length line in
                    let dx = ref 0 in
                    let dy = ref 0 in
                    for j = 0 to ni - 1 do
                      match line.(j) with
                        String (attr, str) ->
                          dx := !dx + (String.length str * font.font_width);
                          dy := max !dy font.font_height
                      | Widget w ->
                          let sz = w#size_request in
                          if not (List.memq w widgets) then
                            widgets <- w :: widgets;
                          dy := max !dy sz.requested_height;
                          dx := !dx + sz.requested_width;
                    done;
                    if !dy <> attr.line_height then
                      if n = 1 || i = 0 then attr.line_height <- !dy else
    (* Problem, this widget requires a greater height than expected.
     Cut the line accordingly *)
                        begin
                          let before = Array.sub lines 0 i in
                          let before = make_text before in
                          let after = Array.sub lines i (n-i) in
                          let after = make_text after in
                          text.tree_desc <- Parts [| before; after|];
                          raise Not_found
                        end;
                      width := max !width !dx;
                  done;
                  attr.line_width <- !width;
                  !width, n * attr.line_height, n
                with Not_found ->
                    size_request text y
          in
          let (dx,dy, nlines) = couple in
          text.tree_width <- dx;
          text.tree_height <- dy;
          text.tree_nlines <- nlines;
          couple
        in
        let (dx,dy,_) = size_request text 0 in
        sz.requested_width <- dx;
        sz.requested_height <- dy;
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
          match text.tree_desc with
            Parts parts ->
              let y = ref y in
              for i = 0 to Array.length parts - 1 do
                iter parts.(i) !y;
                y := !y + parts.(i).tree_height
              done
          | Text (attr,lines) ->
              let dx = ref 0 in
              let n = Array.length lines in
              for i = 0 to n - 1 do
                let line = lines.(i) in
                let ni = Array.length line in
                let dx = ref 0 in
                for j = 0 to ni - 1 do
                  match line.(j) with
                    String (attr, str) ->
                      dx := !dx + (String.length str * font.font_width)
                  | Widget w ->
                      let sz = w#size_request in
                      w#size_allocate !dx (y+ i * attr.line_height) 
                      sz.requested_width sz.requested_height;
                      dx := !dx + sz.requested_width
                done
              done;
      in
      iter text 0
    end
      
     
  method wait_refresh x y dx dy =
    if x=0 && y=0 && dx=0 && dy=0 then redraw_area <- TotalRedraw
    else
      redraw_area <- 
        (match redraw_area with
          NoRedraw -> Redraw (x,y,x+dx,y+dy)
        | Redraw (x',y',xx,yy) -> Redraw (min x x', min y y',
              max xx (x+dx),  max yy (y+dy))
        | TotalRedraw -> TotalRedraw);
    super#wait_refresh x y dx dy;

    (* Direct call to #refresh is not allowed. Use #wait_refresh instead *)
  
  method refresh =
    if s.s_timestamp > w.w_refresh_timestamp && w.w_window <> noWindow then
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
        let qf = font.font_info in
        let gc = X.createGC s.s_display s.s_screen.scr_root 
            [GCforeground (self#color_make w.w_foreground true).c_pixel;
            GCbackground (self#color_make w.w_background true).c_pixel; 
            GCfont font.font_id] in
        let rec draw num text cury =
          if cury + text.tree_height < y then text.tree_height else
          if cury > yy then raise Exit else
          (* We must draw parts of this text *)            
          match text.tree_desc with
            Parts parts ->
              let y = ref cury in
              let n = Array.length parts in
              for i = 0 to (n-1) do
                let text = parts.(i) in
                let h = draw (num+1) text !y in
                y := !y + h
              done;
              text.tree_height
          | Text (attr, lines) ->
              let from_line =
                if cury < y then (y - cury) / attr.line_height else 0 in
              let to_line =
                if cury + text.tree_height > yy then
                  (yy - cury) / attr.line_height
                else (Array.length lines - 1)
              in
              (* Draw each line *)
              for n = from_line to to_line do
                let line = lines.(n) in
                let x = ref 0 in
                let nn = Array.length line in
                for i = 0 to nn - 1 do
                  let item = line.(i) in
                  match item with
                    Widget w -> x := !x + w#width
     (* do not redraw widget since they receive their own expose events *)
                  | String (attrs, str) ->
                  (* don't care about attributes for now *)
                      Xlib.drawString s.s_display w.w_window gc
                        !x (cury+ attr.line_height * n + font.font_ascent)
                      str;
                      x := !x + (String.length str) * font.font_width
                done
              done;
              text.tree_height
        in
        (* We start with y = 0 *)
        (try let _ = draw 1 text 0 in () with Exit -> ());
        X.freeGC s.s_display gc;      
      end;
   
(* 't' is a simple text (an array of lines (array) of text items (array) *)  
  method set_text stext =
    text <- make_text stext; 
    w.w_size_timestamp <- -1;
    self#wait_resize;
    self#wait_refresh 0 0 0 0
   
  method iter f = List.iter f widgets

  method name = "text"
end

let file_to_stext filename =   
  let ic = open_in filename in
  let s = Utils.read_string ic in
  let _ = close_in ic in
  make_simple_text s
  
class of_file parent filename attributes =

  object(self)
  
  inherit t parent (file_to_stext filename) attributes
  
  method set_file filename =
    self#set_text (file_to_stext filename)
end


class of_string parent lines attributes =

  object(self)
  
  inherit t parent (make_simple_text lines) attributes
  
  method set_lines lines =
    self#set_text (make_simple_text lines)
end
