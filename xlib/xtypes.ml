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

type rect = { 
    mutable xx: int; 
    mutable yy: int; 
    mutable dx : int; 
    mutable dy : int }
  
type geometry = {
    mutable x : int;
    mutable y : int;
    mutable width : int;
    mutable height : int;
    mutable border : int;
  }
  
type buffer = string
type waClass = CopyClassFromParent | InputOutput | InputOnly
type bitGravity =
  ForgetGravity    (* unmapGravity *)
| NorthWestGravity
| NorthGravity
| NorthEastGravity
| WestGravity
| CenterGravity
| EastGravity
| SouthWestGravity
| SouthGravity
| SouthEastGravity
| StaticGravity
type visualClass =
  StaticGray
| GrayScale
| StaticColor
| PseudoColor
| TrueColor
| DirectColor
type objet = Cursor | Tile | Stipple
type backingStore = NotUseful | WhenMapped | Always
type mapState = IsUnmapped | IsUnviewable | IsViewable
type stackMode = Above | Below | TopIf | BottomIf | Opposite
type wmState = WithdrawnState | NormalState | UndefinedState | IconicState
type gxFunction =
  GXclear
| GXand
| GXandReverse
| GXcopy
| GXandInverted
| GXnoop
| GXxor
| GXor
| GXnor
| GXequiv
| GXinvert
| GXorReverse
| GXcopyInverted
| GXorInverted
| GXnand
| GXset
type lineStyle = LineSolid | LineOnOffDash | LineDoubleDash
type capStyle = CapNotLast | CapButt | CapRound | CapProjecting
type joinStyle = JoinMiter | JoinRound | JoinBevel
type fillStyle = FillSolid | FillTiled | FillStippled | FillOpaqueStippled
type fillRule = EvenOddRule | WindingRule
type arcMode = ArcChord | ArcPieSlice
type subwindowMode = ClipByChildren | IncludeInferiors
type ledMode = LedOff | LedOn
type autoRepeatMode = RepeatOff | RepeatOn | RepeatDefault
type notifyDetail =
  NotifyAncestor
| NotifyVirtual
| NotifyInferior
| NotifyNonlinear
| NotifyNonlinearVirtual
| NotifyPointer
| NotifyPointerRoot
| NotifyDetailNone
type notifyMode =
  NotifyNormal
| NotifyGrab
| NotifyUngrab
| NotifyWhileGrabbed
type motionMode = MotionNormal | MotionHint
type copyRequest = NotCopy | CopyAre | CopyPlane
type visibilityState =
  VisibilityUnobscured
| VisibilityPartiallyObscured
| VisibilityFullyObscured
type queueMode = QueuedAlready | QueuedAfterReading | QueuedAfterFlush
type grabStatus =
  Success
| AlreadyGrabbed
| InvalidTime
| NotViewable
| Frozen
type atomState = PropertyNewValue | PropertyDelete
type saveSetOp = InsertResource | DeleteResource
type placeMode = PlaceOnTop | PlaceOnBottom
type propertyState = PropertyNewValue | PropertyDelete
type colormapState = ColormapUninstalled | ColormapInstalled
type messageEvent =
  MessString of string
| MessShorts of int array
| MessLongs of int array
type eventType =
  KeyPress
| KeyRelease
| ButtonPress
| ButtonRelease
| MotionNotify
| EnterNotify
| LeaveNotify
| FocusIn
| FocusOut
| KeymapNotify
| Expose
| GraphicsExpose
| NoExpose
| VisibilityNotify
| CreateNotify
| DestroyNotify
| UnmapNotify
| MapNotify
| MapRequest
| ReparentNotify
| ConfigureNotify
| ConfigureRequest
| GravityNotify
| ResizeRequest
| CirculateNotify
| CirculateRequest
| PropertyNotify
| SelectionClear
| SelectionRequest
| SelectionNotify
| ColormapNotify
| ClientMessage
| MappingNotify
type propMode = PropModeReplace | PropModePrepend | PropModeAppend
type propFormat = Format8 | Format16 | Format32
type grabMode = GrabModeSync | GrabModeAsync
type eventMode =
  AsyncPointer
| SyncPointer
| ReplayPointer
| AsyncKeyboard
| SyncKeyboard
| ReplayKeyboard
| AsyncBoth
| SyncBoth
type direction = RaiseLowest | LowerHighest
type allocCells = AllocNone | AllocAll
type revertMode = RevertToNone | RevertToPointerRoot | RevertToParent
type statusType =
  BadStatus
| BitmapOpenFailed
| BitmapFileInvalid
| BitmapNoMemory
| XpmColorError
| UnknownError
| XpmOpenFailed
| XpmFileInvalid
| XpmNoMemory
| XpmColorFailed
type coordMode = Origin | Previous
type closeDownMode = DestroyAll | RetainPermanent | RetainTemporary
type shapeMode = Complex | Nonconvex | Convex
type mappingRequest = MapModifier | MapKeyboard | MapPointer
type modifiers = int
type button = int
type errorCode =
  Success
| BadRequest
| BadValue
| BadWindow
| BadPixmap
| BadAtom
| BadCursor
| BadFont
| BadMatch
| BadDrawable
| BadAccess
| BadAlloc
| BadColor
| BadGC
| BadIDChoice
| BadName
| BadLength
| BadImplementation
type eventMask =
  KeyPressMask
| KeyReleaseMask
| ButtonPressMask
| ButtonReleaseMask
| EnterWindowMask
| LeaveWindowMask
| PointerMotionMask
| PointerMotionHintMask
| Button1MotionMask
| Button2MotionMask
| Button3MotionMask
| Button4MotionMask
| Button5MotionMask
| ButtonMotionMask
| KeymapStateMask
| ExposureMask
| VisibilityChangeMask
| StructureNotifyMask
| ResizeRedirectMask
| SubstructureNotifyMask
| SubstructureRedirectMask
| FocusChangeMask
| PropertyChangeMask
| ColormapChangeMask
| OwnerGrabButtonMask
type gcMask =
  GCFonction
| GCPlaneMask
| GCForeground
| GCBackground
| GCLineWidth
| GCLineStyle
| GCCapStyle
| GCJoinStyle
| GCFillStyle
| GCFillRule
| GCTile
| GCStipple
| GCTileStipXOrigin
| GCTileStipYOrigin
| GCFont
| GCSubwindowMode
| GCGraphicsExposures
| GCClipXOrigin
| GCClipYOrigin
| GCClipMask
| GCDashOffset
| GCDashList
| GCArcMode (* ??? *)

type saverState = ResetScreenSaver | ActivateScreenSaver
type saverMode = SaverNo | SaverYes | SaverDefault
type imageFormat = Bitmap | XYPixmap | ZPixmap
type keyMode = AutoRepeatModeOff | AutoRepeatModeOn | AutoRepeatModeDefault
type hostMode = InsertHost | DeleteHost
type hostDomaine = Internet | DECnet | Chaos
type accessControl = ControlDisable | ControlEnable
type clipOrder = UnSorted | YSorted | YXSorted | YXBanded
type byteOrder = LeastSigniFirst | MostSigniFirst
type fontDirection = LeftToRight | RightToLeft
type colorMask = DoRed | DoGreen | DoBlue
type requestOpcode =
  UnusedOpcode
| X_CreateWindow
| X_ChangeWindowAttributes
| X_GetWindowAttributes
| X_DestroyWindow
| X_DestroySubwindows
| X_ChangeSaveSet
| X_ReparentWindow
| X_MapWindow
| X_MapSubwindows
| X_UnmapWindow
| X_UnmapSubwindows
| X_ConfigureWindow
| X_CirculateWindow
| X_GetGeometry
| X_QueryTree
| X_InternAtom
| X_GetAtomName
| X_ChangeProperty
| X_DeleteProperty
| X_GetProperty
| X_ListProperties
| X_SetSelectionOwner
| X_GetSelectionOwner
| X_ConvertSelection
| X_SendEvent
| X_GrabPointer
| X_UngrabPointer
| X_GrabButton
| X_UngrabButton
| X_ChangeActivePointerGrab
| X_GrabKeyboard
| X_UngrabKeyboard
| X_GrabKey
| X_UngrabKey
| X_AllowEvents
| X_GrabServer
| X_UngrabServer
| X_QueryPointer
| X_GetMotionEvents
| X_TranslateCoordinates
| X_WarpPointer
| X_SetInputFocus
| X_GetInputFocus
| X_QueryKeymap
| X_OpenFont
| X_CloseFont
| X_QueryFont
| X_QueryTextExtents
| X_ListFonts
| X_ListFontsWithInfo
| X_SetFontPath
| X_GetFontPath
| X_CreatePixmap
| X_FreePixmap
| X_CreateGC
| X_ChangeGC
| X_CopyGC
| X_SetDashes
| X_SetClipRectangles
| X_FreeGC
| X_ClearArea
| X_CopyArea
| X_CopyPlane
| X_PolyPoint
| X_PolyLine
| X_PolySegment
| X_PolyRectangle
| X_PolyArc
| X_FillPoly
| X_PolyFillRectangle
| X_PolyFillArc
| X_PutImage
| X_GetImage
| X_PolyText8
| X_PolyText16
| X_ImageText8
| X_ImageText16
| X_CreateColormap
| X_FreeColormap
| X_CopyColormapAndFree
| X_InstallColormap
| X_UninstallColormap
| X_ListInstalledColormaps
| X_AllocColor
| X_AllocNamedColor
| X_AllocColorCells
| X_AllocColorPlanes
| X_FreeColors
| X_StoreColors
| X_StoreNamedColor
| X_QueryColors
| X_LookupColor
| X_CreateCursor
| X_CreateGlyphCursor
| X_FreeCursor
| X_RecolorCursor
| X_QueryBestSize
| X_QueryExtension
| X_ListExtensions
| X_ChangeKeyboardMapping
| X_GetKeyboardMapping
| X_ChangeKeyboardControl
| X_GetKeyboardControl
| X_Bell
| X_ChangePointerControl
| X_GetPointerControl
| X_SetScreenSaver
| X_GetScreenSaver
| X_ChangeHosts
| X_ListHosts
| X_SetAccessControl
| X_SetCloseDownMode
| X_KillClient
| X_RotateProperties
| X_ForceScreenSaver
| X_SetPointerMapping
| X_GetPointerMapping
| X_SetModifierMapping
| X_GetModifierMapping
| X_NoOperation

type font = int
type visual = int
type pixmap = int
type window = int
type colormap = int
type pixel = int
type colordef = Color of string | RGB of int * int * int | NoColor
type cursor = int
type atom = int
type gc = int
type keySym = int
type keyCode = int
type coord = int
type size = int
type red = int
type green = int
type blue = int
type image = string
type time = int * int
type keycode = int
type textItem =
  Text8 of int * string * int * int 
| Text16 of int array * int 
| ShiftFont of font



module Xkey = struct type t = {
        detail : keycode;
        time : time;
        root : window;
        event : window;
        child : window;
        x_event : coord;
        y_event : coord;
        x_root : coord;
        y_root : coord;
        state : modifiers;
        same_screen : bool
      }
  end
;;

module Xbutton = struct type t = {
        detail : button;
        time : time;
        root : window;
        event : window;
        child : window;
        x_event : coord;
        y_event : coord;
        x_root : coord;
        y_root : coord;
        state : modifiers;
        same_screen : bool
      }
  end
;;

module Xmotion = struct type t = {
        detail : motionMode;
        time : time;
        root : window;
        event : window;
        child : window;
        x_event : coord;
        y_event : coord;
        x_root : coord;
        y_root : coord;
        state : modifiers;
        same_screen : bool
      }
  end
;;

module Xcrossing = struct type t = {
        detail : notifyDetail;
        time : time;
        root : window;
        event : window;
        child : window;
        x_event : coord;
        y_event : coord;
        x_root : coord;
        y_root : coord;
        state : modifiers;
        same_screen : bool;
        mode : notifyMode;
        focus : bool
      }
  end
;;

module Xfocus = struct type t = {
        event : window;
        detail : notifyDetail;
        mode : notifyMode
      }
  end
;;

module Xkeymap = struct type t = {
        keys : string
      }
  end
;;

module Xexpose = struct type t = {
        window : window;
        x : coord;
        y : coord;
        width : size;
        height : size;
        count : int
      }
  end
;;

module Xgraphicsexpose = struct type t = {
        drawable : window;
        x : coord;
        y : coord;
        width : size;
        height : size;
        minor_opcode : int;
        count : int;
        major_opcode : int
      }
  end
;;

module Xnoexpose = struct type t = {
        drawable : window;
        minor_opcode : int;
        major_opcode : int
      }
  end
;;

module Xvisibility  = struct type t = {
        window : window ;
        state : visibilityState
      }
  end
;;

module Xcreatewindow  = struct type t = {
        parent : window;
        window : window;
        x : coord;
        y : coord;
        width : size;
        height : size;
        border_width : int;
        override_redirect : bool
      }
  end
;;

module Xdestroywindow = struct type t = {
        event : window;
        window : window
      }
  end
;;


module Xunmap = struct type t = {
        event : window;
        window : window;
        from_configure : bool
      }
  end
;;

module Xmap = struct type t = {
        event : window;
        window : window;
        override_redirect : bool
      }
  end
;;

module Xmaprequest = struct type t = {
        parent : window;
        window : window
      }
  end
;;

module Xreparent = struct type t = {
        event : window;
        window : window;
        parent : window;
        x : coord;
        y : coord;
        override_redirect : bool
      }
  end
;;

module Xconfigure = struct type t = {
        event : window;
        window : window;
        above_sibling : window;
        x : coord;
        y : coord;
        width : size;
        height : size;
        border_width : int;
        override_redirect : bool
      }
  end
;;

module Xconfigurerequest = struct type t = {
        stack_mode : stackMode option;
        parent : window;
        window : window;
        sibling : window option;
        x : coord option;
        y : coord option;
        width : size option;
        height : size option;
        border_width : int option
      }
  end
;;


module Xgravity = struct type t = {
        event : window;
        window : window;
        x : coord;
        y : coord
      }
  end
;;

module Xresizerequest = struct type t = {
        window : window;
        width : size;
        height : size
      }
  end
;;

module Xcirculate = struct type t = {
        event : window;
        window : window;
        place : placeMode
      }
  end
;;

module Xcirculaterequest = struct type t = {
        parent : window;
        window : window;
        place : placeMode
      }
  end
;;

module Xproperty = struct type t = {
        window : window;
        atom : atom;
        time : time;
        state : propertyState
      }
  end
;;

module Xselectionclear = struct type t = {
        time : time;
        owner : atom;
        selection : atom
      }
  end
;;

module Xselectionrequest = struct type t = {
        time : time;
        owner : window;
        requestor : window;
        selection : atom;
        target : atom;
        property : atom
      }
  end
;;


module Xselection = struct type t = {
        time : time;
        requestor : window;
        selection : atom;
        target : atom;
        property : atom
      }
  end
;;

module Xcolormap = struct type t = {
        window : window;
        colormap : colormap;
        newp : bool;
        state : colormapState
      }
  end
;;

module Xclient = struct type t = {
        format : int;
        window : window;
        datatype : atom;
        data : string
      }
  end
;;

module Xmapping = struct type t = {
        request : mappingRequest;
        first_keycode : keycode;
        count : int
      }
  end
;;

type event =
  KeyPressEvent of Xkey.t
| KeyReleaseEvent of Xkey.t
| ButtonPressEvent of Xbutton.t
| ButtonReleaseEvent of Xbutton.t
| MotionNotifyEvent of Xmotion.t
| EnterNotifyEvent of Xcrossing.t
| LeaveNotifyEvent of Xcrossing.t
| FocusInEvent of Xfocus.t
| FocusOutEvent of Xfocus.t
| KeymapNotifyEvent of Xkeymap.t
| ExposeEvent of Xexpose.t
| GraphicsExposeEvent of Xgraphicsexpose.t
| NoExposeEvent of Xnoexpose.t
| VisibilityNotifyEvent of Xvisibility.t
| CreateNotifyEvent of Xcreatewindow.t
| DestroyNotifyEvent of Xdestroywindow.t
| UnmapNotifyEvent of Xunmap.t
| MapNotifyEvent of Xmap.t
| MapRequestEvent of Xmaprequest.t
| ReparentNotifyEvent of Xreparent.t
| ConfigureNotifyEvent of Xconfigure.t
| ConfigureRequestEvent of Xconfigurerequest.t
| GravityNotifyEvent of Xgravity.t
| ResizeRequestEvent of Xresizerequest.t
| CirculateNotifyEvent of Xcirculate.t
| CirculateRequestEvent of Xcirculaterequest.t
| PropertyNotifyEvent of Xproperty.t
| SelectionClearEvent of Xselectionclear.t
| SelectionRequestEvent of Xselectionrequest.t
| SelectionNotifyEvent of Xselection.t
| ColormapNotifyEvent of Xcolormap.t
| ClientMessageEvent of Xclient.t
| MappingNotifyEvent of Xmapping.t
| CoreEvent of string

(* types record *)

type charInfo =
  { mutable char_lbearing: int;
    mutable char_rbearing: int;
    mutable char_width: int;
    mutable char_ascent: int;
    mutable char_descent: int;
    mutable char_attributes: int }
type fontInfo = {
    font_min_bounds : charInfo;
    font_max_bounds : charInfo;
    font_min_char_or_byte2 : int;
    font_max_char_or_byte2 : int;
    font_default_char : int;
    font_draw_direction : fontDirection;
    font_min_byte1 : int;
    font_max_byte1 : int;
    font_all_char_exist : bool;
    font_ascent : int;
    font_descent : int;
    font_properties : (atom * int) list }

type errorEvent =
  { err_resourceid: int;
    err_serial: int;
    err_code: errorCode;
    err_request_code: requestOpcode;
    err_minor_code: int }
type  xerror =
  NoError
| Error of errorEvent
type pixmapFormat =
  { pxf_depth: int;
    pxf_bits_per_pixel: int;
    pxf_scanline_pad: int }
type visualType =
  { vsl_id: int;
    vsl_class: visualClass;
    vsl_bits_per_rgb: int;
    vsl_colormap_entries: int;
    vsl_red_mask: int;
    vsl_green_mask: int;
    vsl_blue_mask: int }
type depth = { dpf_depth: int; dpf_visuals: visualType list }
type screen =
  { scr_root: window;
    scr_default_colormap: colormap;
    scr_white_pixel: pixel;
    scr_black_pixel: pixel;
    scr_current_input_mask: eventMask list;
    scr_width: int;
    scr_height: int;
    scr_width_mm: int;
    scr_height_mm: int;
    scr_min_installed_colormap: int;
    scr_max_installed_colormap: int;
    scr_root_visual_id: int;
    scr_backing_stores: backingStore;
    scr_save_unders: bool;
    scr_root_depth: int;
    scr_depths: depth list }
type xevent =
  {
    ev_num : int; (* type [int] *)
    ev_type : eventType; (* type *)
    ev_serial_out : int; (* derniere reception avant emission par X *)
    ev_serial_in : int; (* numero de reception par le programme *)
    ev_sent : bool;   (* sent ? *)
    ev_window : window; (* fenetre destinatrice *)
    ev_event : event;
  }

type keyTrans =
  {
    mutable key_string: string;
    mutable key_len:int;
    mutable key_key: keySym;
    mutable key_state: int;
    mutable key_mlen: int;
    mutable key_modifiers: keySym array;
  }

type extension = {
    major_opcode : int;
    first_event : int;
    last_event : int;
    first_error : int;
    last_error : int;
  }

type display =
  {
    socket: Unix.file_descr;
    mutable serial_out: int;
    mutable serial_in: int;
    
    wrappers_queue: (int * (string->unit)) Xfifo.t;
    event_queue: xevent Equeue.t;
    
    mutable last_resource_id : int;
    resource_mask : int;
    resource_base : int;
    mutable resource_incr : int;
    mutable dpy_broken : unit -> unit;
    mutable dpy_key_bindings: keyTrans list;
    dpy_proto_major: int;
    dpy_proto_minor: int;
    dpy_proto_release: int;
    dpy_motion_buffer_size: int;
    dpy_screen_number: int;
    dpy_screen_default: int;
    dpy_pixmap_format_number: int;
    dpy_vendor: string;
    mutable dpy_keysyms : keycode array array;
    mutable dpy_modifiermap : int array array;
    mutable dpy_keysyms_per_keycode : int;
    mutable dpy_lock_meaning: keycode;
    mutable dpy_mode_switch: int;
    mutable dpy_num_lock: int;
    dpy_min_keycode: int;
    dpy_max_keycode: int;
    dpy_pixmap_formats: pixmapFormat list;
    dpy_roots: screen array;
    dpy_max_request_length: int;
    dpy_image_byte_order: byteOrder;
    dpy_bitmap_format_bit_order: byteOrder;
    dpy_bitmap_format_scanline_unit: int;
    dpy_bitmap_format_scanline_pad: int;
    
    dpy_server_name: string;
    mutable dpy_extensions: (string * extension) list;
  }

(* exceptions *)
exception GrabError of grabStatus
exception OutOfScreen
exception MappingBusy
exception MappingFailed
exception Found_but of atom * int * int
exception XError of errorEvent


(*

   Modules ICCCM


*)

type wm_size_hints =
  { mutable user_position: bool;
    mutable user_size: bool;
    mutable program_position: bool;
    mutable program_size: bool;
    mutable min_size: (int * int) option;
    mutable max_size: (int * int) option;
    mutable resize_inc: (int * int) option;
    mutable base_size: (int * int) option;
    mutable aspect: (int * int * int * int) option;
    mutable win_gravity: bitGravity option }
type wm_hints =
  { mutable urgency_hint: bool;
    mutable input: bool option;
    mutable initial_state: wmState option;
    mutable icon_pixmap: pixmap;
    mutable icon_window: window;
    mutable icon_position: (int * int) option;
    mutable icon_mask: pixmap;
    mutable window_group: window }
type wm_icon_size =
  { mutable min_width: int;
    mutable min_height: int;
    mutable max_width: int;
    mutable max_height: int;
    mutable width_inc: int;
    mutable height_inc: int }

(*

modules Xloop

*)


(* types abstraits *)
(*[kbValues] voir <A HREF=#X.createKBValues> X.createKBValues </A> *)
type kbValues = string
type kbState = string
(*[windowAttributes] voir <A HREF=#X.createWindowAttributes> X.createWindowAttributes </A> *)

type windowAttribute =
  WABacking_store of backingStore
| WAVisual of visual
| WAClass of waClass
| WABit_gravity of bitGravity
| WAWin_gravity of bitGravity
| WABacking_planes of int
| WABacking_pixel of pixel
| WASave_under of bool
| WAMap_is_installed of bool
| WAMap_state of mapState
| WAOverride_redirect of bool
| WAColormap of colormap
| WAAll_event_mask of eventMask list
| WAYour_event_mask of eventMask list
| WADont_propagate_mask of eventMask list

type configureWindow =
  CWX of int
| CWY of int
| CWWidth of int
| CWHeight of int
| CWBorderWidth of int
| CWSibling of window
| CWStackMode of stackMode

type setWindowAttributes =
| CWBackPixmap of pixmap
| CWBackPixel of pixel
| CWBorderPixmap of pixmap
| CWBorderPixel of pixel
| CWBitGravity of bitGravity
| CWWinGravity of bitGravity
| CWBackingStore of backingStore
| CWBackingPlanes of int
| CWBackingPixel of pixel
| CWOverrideRedirect of bool
| CWSaveUnder of bool
| CWEventMask of eventMask list
| CWDontPropagate of eventMask list
| CWColormap of colormap
| CWCursor of cursor

type setKBvalues =
  KBKeyClickPercent of int
| KBBellPercent of int
| KBBellPitch of int
| KBBellDuration of int
| KBLed of int
| KBLedMode of ledMode
| KBKey of int
| KBAutoRepeatMode of keyMode

type getWindowAttributesRep =               
  { gwa_backing_store : backingStore;
    gwa_visual : visual;
    gwa_wa_class : waClass;
    gwa_bit_gravity : bitGravity;
    gwa_win_gravity : bitGravity;
    gwa_backing_planes : int;
    gwa_backing_pixel : pixel;
    gwa_save_under : bool;
    gwa_map_is_installed : bool;
    gwa_map_state : mapState;
    gwa_override_redirect : bool;
    gwa_colormap : colormap;
    gwa_all_event_mask : eventMask list;
    gwa_your_event_mask : eventMask list;
    gwa_dont_propagate_mask : eventMask list;
  } 


type getGeometryRep =
  {
    gg_root : window;
    gg_x : int;
    gg_y : int;
    gg_width : int;
    gg_height : int;
    gg_border_width : int;
    gg_depth : int;
  } 

type queryTreeRep = 
  { qt_root : window;
    qt_parent : window;
    qt_subwindows : window list;
  } 

type getPropertyRep =
  { gp_type : atom;
    gp_format : int;
    gp_length : int;
    gp_value : string;
    gp_left : int;
  } 

type queryPointerRep = 
  { qp_root : window;
    qp_win : window;
    qp_root_x : int;
    qp_root_y : int;
    qp_win_x : int;
    qp_win_y : int;
    qp_modifiers : modifiers;
  } 

type motionEvent =
  { me_win : window;
    me_x : int;
    me_y : int }

type getInputFocusRep =
  { gif_win : window;
    gif_mode : revertMode }

type queryFontRep =
  { qf_info : fontInfo;
    qf_chars : charInfo array }

type queryTextExtentsRep =
  {
    qte_direction : fontDirection;
    qte_f_ascent : int;
    qte_f_descent : int;
    qte_ovll_ascent : int;
    qte_ovll_descent : int;
    qte_ovll_width : int;
    qte_ovll_left : int;
    qte_ovll_right : int;
  } 

type getImageRep =
  { gi_depth : int;
    gi_visual : visual;
    gi_image : string; }

type color =
  { red : int;
    green : int;
    blue : int;
  } 

type allocColorRep =
  { ac_pixel : pixel;
    ac_color : color; }

type allocNamedColorRep =
  { anc_pixel : pixel;
    anc_exact : color;
    anc_visual : color;
  } 

type allocColorCellsRep =
  { acc_pixels : pixel array;
    acc_masks : int array }

type allocColorPlanesRep =
  { acp_pixels : pixel array;
    acp_mask : color; }

type lookupColorRep =
  { lc_exact : color;
    lc_visual : color;
  } 

type queryBestSizeRep =
  { qbs_width : int;
    qbs_height : int;}

type queryExtensionRep =
  { qe_present : bool;
    qe_opcode : int;
    qe_event : int;
    qe_error : int;
  } 

type getKeyboardMappingRep =
  { gkm_keysyms_per_keycode : int;
    gkm_keycodes : keycode array array; }

type  getPointerControlRep =
  { gpc_acc_num : int;
    gpc_acc_den : int;
    gpc_threshold : int;
  } 

type getKeyboardControlRep = 
  { gkc_key_click_percent: int;
    gkc_bell_percent: int;
    gkc_bell_pitch: int;
    gkc_bell_duration: int;
    gkc_led_mask: int;
    gkc_global_auto_repeat: keyMode;
    gkc_auto_repeats: string;
  }


type getScreenSaverRep =
  {
    gss_timeout : int;
    gss_interval : int;
    gss_pref_blanc : bool;
    gss_allow_exp : bool;
  } 

type listHostsRep =
  { lh_control : accessControl;
    lh_list : (hostDomaine * string) list;
  } 

type setGCattributes =
| GCfonction of gxFunction 
| GCplane_mask of int 
| GCforeground of pixel 
| GCbackground of pixel 
| GCline_width of int 
| GCline_style of lineStyle 
| GCcap_style of capStyle 
| GCjoin_style of joinStyle 
| GCfill_style of fillStyle 
| GCfill_rule of fillRule 
| GCtile of pixmap 
| GCstipple of pixmap 
| GCts_x_origin of coord 
| GCts_y_origin of coord 
| GCfont of font 
| GCsubwindow_mode of subwindowMode 
| GCgraphics_exposures of bool 
| GCclip_x_origin of coord 
| GCclip_y_origin of coord 
| GCclip_mask of pixmap 
| GCdash_offset of int 
| GCdashes of int 
| GCarc_mode of arcMode 


type family =
  FamilyLocal
| FamilyWild
| FamilyNetname
| FamilyKrb5Principal
| FamilyLocalHost
| FamilyIP

type xauth_record =
  {
    xauth_family: family;
    xauth_address: string;
    xauth_number: string;
    xauth_name : string;
    xauth_data : string
  }

external font_to_id : font -> int = "%identity"
external id_to_font : int -> font = "%identity"
external visual_to_id : visual -> int = "%identity"   
external id_to_visual : int -> visual = "%identity"
external pixmap_to_id : pixmap -> int = "%identity"   
external id_to_pixmap : int -> pixmap = "%identity"
external window_to_id : window -> int = "%identity"   
external id_to_window : int -> window = "%identity"
external colormap_to_id : colormap -> int = "%identity"   
external id_to_colormap : int -> colormap = "%identity"
external pixel_to_id : pixel -> int = "%identity"   
external id_to_pixel : int -> pixel = "%identity"
external cursor_to_id : cursor -> int = "%identity"   
external id_to_cursor : int -> cursor = "%identity"
external atom_to_id : atom -> int = "%identity"   
external id_to_atom : int -> atom = "%identity"
external gc_to_id : gc -> int = "%identity"   
external id_to_gc : int -> gc = "%identity"
let real_time (a1,a2) = float_of_int a1 *. 65536.0 +. float_of_int a2


let anyButton = 0
let copyVisualFromParent = 0
let pixmapParentRelative = 1
let copyPixmapFromParent = 0
let copyColormapFromParent = 0
let noCursor = 0
let copyDepthFromParent = 0
let unmapGravity = ForgetGravity
let noOwner = 0
let noConfineTo = 0
let noWindow = 0
let anyKey = 0
let pointerRoot = 1
let noFocus = 0
let anyModifier = 1 lsl 15
let shiftMask = 1
let lockMask = 2
let controlMask = 4
let mod1Mask = 8
let mod2Mask = 16
let mod3Mask = 32
let mod4Mask = 64
let mod5Mask = 128
let button1Mask = 256
let button2Mask = 512
let button3Mask = 1024
let button4Mask = 2048
let button5Mask = 4096
let noPixmap = 0
let noColormap = 0
let noAtom = 0
let none = 0
let parentRelative = 1
let copyFromParent = 0
let pointerWindow = 0
let inputFocus = 1
let anyPropertyType = 0
let anyButton = 0
let allTemporary = 0
let currentTime = 0,0
let noSymbol = 0
let xerrors =
  [|
    " Success";
    "BadRequest";
    "BadValue";
    "BadWindow";
    "BadPixmap";
    "BadAtom";
    "BadCursor";
    "BadFont";
    "BadMatch";
    "BadDrawable";
    "BadAccess";
    "BadAlloc";
    "BadColor";
    "BadGC";
    "BadIDChoice";
    "BadName";
    "BadLength";
    "BadImplementation"|]

let xopcodes =
  [| "UnusedOpcode";
    "CreateWindow";
    "ChangeWindowAttributes";
    "GetWindowAttributes";
    "DestroyWindow";
    "DestroySubwindows";
    "ChangeSaveSet";
    "ReparentWindow";
    "MapWindow";
    "MapSubwindows";
    "UnmapWindow";
    "UnmapSubwindows";
    "ConfigureWindow";
    "CirculateWindow";
    "GetGeometry";
    "QueryTree";
    "InternAtom";
    "GetAtomName";
    "ChangeProperty";
    "DeleteProperty";
    "GetProperty";
    "ListProperties";
    "SetSelectionOwner";
    "GetSelectionOwner";
    "ConvertSelection";
    "SendEvent";
    "GrabPointer";
    "UngrabPointer";
    "GrabButton";
    "UngrabButton";
    "ChangeActivePointerGrab";
    "GrabKeyboard";
    "UngrabKeyboard";
    "GrabKey";
    "UngrabKey";
    "AllowEvents";
    "GrabServer";
    "UngrabServer";
    "QueryPointer";
    "GetMotionEvents";
    "TranslateCoordinates";
    "WarpPointer";
    "SetInputFocus";
    "GetInputFocus";
    "QueryKeymap";
    "OpenFont";
    "CloseFont";
    "QueryFont";
    "QueryTextExtents";
    "ListFonts";
    "ListFontsWithInfo";
    "SetFontPath";
    "GetFontPath";
    "CreatePixmap";
    "FreePixmap";
    "CreateGC";
    "ChangeGC";
    "CopyGC";
    "SetDashes";
    "SetClipRectangles";
    "FreeGC";
    "ClearArea";
    "CopyArea";
    "CopyPlane";
    "PolyPoint";
    "PolyLine";
    "PolySegment";
    "PolyRectangle";
    "PolyArc";
    "FillPoly";
    "PolyFillRectangle";
    "PolyFillArc";
    "PutImage";
    "GetImage";
    "PolyText8";
    "PolyText16";
    "ImageText8";
    "ImageText16";
    "CreateColormap";
    "FreeColormap";
    "CopyColormapAndFree";
    "InstallColormap";
    "UninstallColormap";
    "ListInstalledColormaps";
    "AllocColor";
    "AllocNamedColor";
    "AllocColorCells";
    "AllocColorPlanes";
    "FreeColors";
    "StoreColors";
    "StoreNamedColor";
    "QueryColors";
    "LookupColor";
    "CreateCursor";
    "CreateGlyphCursor";
    "FreeCursor";
    "RecolorCursor";
    "QueryBestSize";
    "QueryExtension";
    "ListExtensions";
    "ChangeKeyboardMapping";
    "GetKeyboardMapping";
    "ChangeKeyboardControl";
    "GetKeyboardControl";
    "Bell";
    "ChangePointerControl";
    "GetPointerControl";
    "SetScreenSaver";
    "GetScreenSaver";
    "ChangeHosts";
    "ListHosts";
    "SetAccessControl";
    "SetCloseDownMode";
    "KillClient";
    "RotateProperties";
    "ForceScreenSaver";
    "SetPointerMapping";
    "GetPointerMapping";
    "SetModifierMapping";
    "GetModifierMapping";
    "NoOperation"|]

let events = [|
    "KeyPressEvent" 
    ;"KeyReleaseEvent" 
    ;"ButtonPressEvent" 
    ;"ButtonReleaseEvent" 
    ;"MotionNotifyEvent" 
    ;"EnterNotifyEvent" 
    ;"LeaveNotifyEvent" 
    ;"FocusInEvent" 
    ;"FocusOutEvent" 
    ;"KeymapNotifyEvent" 
    ;"ExposeEvent" 
    ;"GraphicsExposeEvent" 
    ;"NoExposeEvent" 
    ;"VisibilityNotifyEvent" 
    ;"CreateNotifyEvent" 
    ;"DestroyNotifyEvent" 
    ;"UnmapNotifyEvent" 
    ;"MapNotifyEvent" 
    ;"MapRequestEvent" 
    ;"ReparentNotifyEvent" 
    ;"ConfigureNotifyEvent" 
    ;"ConfigureRequestEvent" 
    ;"GravityNotifyEvent" 
    ;"ResizeRequestEvent" 
    ;"CirculateNotifyEvent" 
    ;"CirculateRequestEvent" 
    ;"PropertyNotifyEvent" 
    ;"SelectionClearEvent" 
    ;"SelectionRequestEvent" 
    ;"SelectionNotifyEvent" 
    ;"ColormapNotifyEvent" 
    ;"ClientMessageEvent" 
    ;"MappingNotifyEvent" 
    ;"CoreEvent"
  |]

let event_to_string ev =
  if ev.ev_num >= Array.length events then "CoreEvent" else events.(ev.ev_num)
  
let rect x y dx dy = { xx=x; yy=y; dx=dx; dy=dy}
  
let xlib_wait = Concur.Condition.create ()
let xlib_mutex = Concur.Mutex.create ()    
