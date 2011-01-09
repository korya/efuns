open Xtypes

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id: xGraphics.mli,v 1.4 1999/09/20 13:52:07 lefessan Exp $ *)

(* Module [Graphics]: machine-independent graphics primitives *)

exception Graphic_failure of string
        (* Raised by the functions below when they encounter an error. *)

(*** Initializations *)

val open_graph: string -> int -> int -> unit
(* Xlib: the first argument is the display name, the second one is the width
  and the third one is the height. *)
        (* Show the graphics window or switch the screen to graphic mode.
           The graphics window is cleared and the current point is set
           to (0, 0). The string argument is used to pass optional
           information on the desired graphics mode, the graphics window
           size, and so on. Its interpretation is implementation-dependent.
           If the empty string is given, a sensible default is selected. *)
val close_graph: unit -> unit
        (* Delete the graphics window or switch the screen back to
           text mode. *)
val clear_graph : unit -> unit
        (* Erase the graphics window. *)
val size_x : unit -> int
val size_y : unit -> int
        (* Return the size of the graphics window. Coordinates of the screen
           pixels range over [0 .. size_x()-1] and [0 .. size_y()-1].
           Drawings outside of this rectangle are clipped, without causing
           an error. The origin (0,0) is at the lower left corner. *)

(*** Colors *)

type color = int
        (* A color is specified by its R, G, B components. Each component
           is in the range [0..255]. The three components are packed in
           an [int]: [0xRRGGBB], where [RR] are the two hexadecimal digits for
           the red component, [GG] for the green component, [BB] for the
           blue component. *)

val rgb: int -> int -> int -> color
        (* [rgb r g b] returns the integer encoding the color with red
           component [r], green component [g], and blue component [b].
           [r], [g] and [b] are in the range [0..255]. *)

val set_color : color -> unit
        (* Set the current drawing color. *)

val black : color
val white : color
val red : color
val green : color
val blue : color
val yellow : color
val cyan : color
val magenta : color
        (* Some predefined colors. *)

val background: color
val foreground: color
        (* Default background and foreground colors (usually, either black
           foreground on a white background or white foreground on a
           black background).
           [clear_graph] fills the screen with the [background] color.
           The initial drawing color is [foreground]. *)

(*** Point and line drawing *)

val plot : int -> int -> unit
        (* Plot the given point with the current drawing color. *)
val point_color : int -> int -> color
        (* Return the color of the given point. *)
val moveto : int -> int -> unit
        (* Position the current point. *)
val current_point : unit -> int * int
        (* Return the position of the current point. *)
val lineto : int -> int -> unit
        (* Draw a line with endpoints the current point and the given point,
           and move the current point to the given point. *)
val draw_arc : int -> int -> int -> int -> int -> int -> unit
              
        (* [draw_arc x y rx ry a1 a2] draws an elliptical arc with center
           [x,y], horizontal radius [rx], vertical radius [ry], from angle
           [a1] to angle [a2] (in degrees). The current point is unchanged. *)
val draw_ellipse : int -> int -> int -> int -> unit
        (* [draw_ellipse x y rx ry] draws an ellipse with center
           [x,y], horizontal radius [rx] and vertical radius [ry].
           The current point is unchanged.  *)
val draw_circle : int -> int -> int -> unit
        (* [draw_circle x y r] draws a circle with center [x,y] and
           radius [r]. The current point is unchanged. *)
val set_line_width : int -> unit
        (* Set the width of points and lines drawn with the functions above.
           Under X Windows, [set_line_width 0] selects a width of 1 pixel
           and a faster, but less precise drawing algorithm than the one
           used when [set_line_width 1] is specified. *)

(*** Text drawing *)

val draw_char : char -> unit
val draw_string : string -> unit
        (* Draw a character or a character string with lower left corner
           at current position. After drawing, the current position is set
           to the lower right corner of the text drawn. *)
val set_font : string -> unit

val set_text_size : int -> unit

        (* Set the font and character size used for drawing text.
           The interpretation of the arguments to [set_font] and
           [set_text_size] is implementation-dependent. *)
val text_size : string -> int * int
        (* Return the dimensions of the given text, if it were drawn with
           the current font and size. *)

(*** Filling *)

val fill_rect : int -> int -> int -> int -> unit
        (* [fill_rect x y w h] fills the rectangle with lower left corner
           at [x,y], width [w] and height [h], with the current color. *)
val fill_poly : (int * int) array -> unit
        (* Fill the given polygon with the current color. The array
           contains the coordinates of the vertices of the polygon. *)
val fill_arc : int -> int -> int -> int -> int -> int -> unit
              
        (* Fill an elliptical pie slice with the current color. The
           parameters are the same as for [draw_arc]. *)
val fill_ellipse : int -> int -> int -> int -> unit
        (* Fill an ellipse with the current color. The
           parameters are the same as for [draw_ellipse]. *)
val fill_circle : int -> int -> int -> unit
        (* Fill a circle with the current color. The
           parameters are the same as for [draw_circle]. *)

(*** Images *)

type image
        (* The abstract type for images, in internal representation.
           Externally, images are represented as matrices of colors. *)

val transp : color
        (* In matrices of colors, this color represent a ``transparent''
           point: when drawing the corresponding image, all pixels on the
           screen corresponding to a transparent pixel in the image will
           not be modified, while other points will be set to the color
           of the corresponding point in the image. This allows superimposing
           an image over an existing background. *)

val make_image : color array array -> image
        (* Convert the given color matrix to an image.
           Each sub-array represents one horizontal line. All sub-arrays
           must have the same length; otherwise, exception [Graphic_failure]
           is raised. *)
val dump_image : image -> color array array
        (* Convert an image to a color matrix. *)
val draw_image : image -> int -> int -> unit
        (* Draw the given image with lower left corner at the given point. *)
val get_image : int -> int -> int -> int -> image
        (* Capture the contents of a rectangle on the screen as an image.
           The parameters are the same as for [fill_rect]. *)
val create_image : int -> int -> image
        (* [create_image w h] returns a new image [w] pixels wide and [h]
           pixels tall, to be used in conjunction with [blit_image].
           The initial image contents are random, except that no point
           is transparent. *)
val blit_image : image -> int -> int -> unit
        (* [blit_image img x y] copies screen pixels into the image [img],
           modifying [img] in-place. The pixels copied are those inside the
           rectangle with lower left corner at [x,y], and width and height
           equal to those of the image. Pixels that were transparent in
           [img] are left unchanged. *)

(*** Mouse and keyboard events *)

type status =
  { mouse_x : int;              (* X coordinate of the mouse *)
    mouse_y : int;              (* Y coordinate of the mouse *)
    button : bool;              (* true if a mouse button is pressed *)
    mutable keypressed : bool;          (* true if a key has been pressed *)
    mutable key : char }                (* the character for the key pressed *)
        (* To report events. *)

type event =
    Button_down                 (* A mouse button is pressed *)
  | Button_up                   (* A mouse button is released *)
  | Key_pressed                 (* A key is pressed *)
  | Mouse_motion                (* The mouse is moved *)
  | Poll                        (* Don't wait; return immediately *)
        (* To specify events to wait for. *)

val wait_next_event : event list -> status
        (* Wait until one of the events specified in the given event list
           occurs, and return the status of the mouse and keyboard at
           that time. If [Poll] is given in the event list, return immediately
           with the current status. If the mouse cursor is outside of the
           graphics window, the [mouse_x] and [mouse_y] fields of the event are
           outside the range [0..size_x()-1, 0..size_y()-1]. Keypresses
           are queued, and dequeued one by one when the [Key_pressed]
           event is specified. *)

(*** Mouse and keyboard polling *)

val mouse_pos : unit -> int * int
        (* Return the position of the mouse cursor, relative to the
           graphics window. If the mouse cursor is outside of the graphics
           window, [mouse_pos()] returns a point outside of the range
           [0..size_x()-1, 0..size_y()-1]. *)
val button_down : unit -> bool
        (* Return [true] if the mouse button is pressed, [false] otherwise. *)
val read_key : unit -> char
        (* Wait for a key to be pressed, and return the corresponding
           character. Keypresses are queued. *)
val key_pressed : unit -> bool
        (* Return [true] if a keypress is available; that is, if [read_key]
           would not block. *)

(*** Sound *)

val sound : int -> int -> unit
        (* [sound freq dur] plays a sound at frequency [freq] (in hertz)
           for a duration [dur] (in milliseconds). *)

(***********************************************************************)
(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
  
type view = {
    display : display;
    screen : screen;
    white : pixel;
    black : pixel;
    root : window;
    gc : gc;    
    window : window;
    mutable store : pixmap;
    cmap : colormap;
    mutable size_x : int;
    mutable size_y : int;
    mutable point_x : int;
    mutable point_y : int;
    mutable color : int;
    mutable pixel : pixel;
    mutable font : font;
    mutable descent : int;
    mutable ascent : int;
    mutable qf : queryFontRep;
    colors : (int,pixel) Hashtbl.t;
    pixels : (pixel,  int) Hashtbl.t;    
    depth : int;
    mutable s_in_events : (event * status) list;
    mutable s_out_events : (event * status) list;
    mutable s_keys : char list
  }

val getview : unit -> view
val open_graph_display : Xtypes.display -> int -> int -> unit



type update_style =
    ImmediateDraw
  | FlushAll
  | FlushClipped

val update : unit -> unit
val set_update_style : update_style -> unit

val create_image_from_xpm: string -> image
val create_image_from_ppm: string -> image
val create_image_from_xpm_data: int *int * colordef array * int array array -> image
val default_event_mask : eventMask list
val default_font : string
val view: view option ref
val window_handler : view -> Xtypes.xevent -> unit