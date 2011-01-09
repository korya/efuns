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

open Xtypes
open Xbuffer
open Display
open Xproto

(* Synchronisation *)

let sendRequest dpy buffer = let _ = send_alone dpy buffer in ()

let sendRequestAndGetReply dpy buffer wrapper =
  let event = Jeton.create ()
  in
  send_with_wrapper dpy buffer (parse_buffer event wrapper);
  Jeton.wait event

(* Requests *)

let clearArea dpy window x y dx dy exposures =
  sendRequest dpy
    (clearAreaReq window x y dx dy exposures)

let clearWindow dpy window = sendRequest dpy (clearWindowReq window)
  
let copyArea dpy gc src src_x src_y dst dst_x dst_y dx dy =
  sendRequest dpy
    (copyAreaReq  gc src src_x src_y dst dst_x dst_y dx dy)

let copyPlane dpy gc src src_x src_y dst dst_x dst_y dx dy planes =
  sendRequest dpy 
    (copyPlaneReq gc src src_x src_y dst dst_x dst_y dx dy planes)

let changeSaveSet dpy window op =
  sendRequest dpy
    (changeSaveSetReq window op)

let reparentWindow dpy window parent x y =
  sendRequest dpy (reparentWindowReq window parent x y)

let allocColor dpy cmap red green blue =
  sendRequestAndGetReply dpy
    (allocColorReq cmap red green blue)
    allocColorRep

let  allocNamedColor dpy cmap name =
  sendRequestAndGetReply dpy
    (allocNamedColorReq cmap name)
    allocNamedColorRep

let allocColorCells  dpy cmap colors planes contiguous =
  sendRequestAndGetReply dpy
    (allocColorCellsReq  cmap colors planes contiguous)
    allocColorCellsRep

let allocColorPlanes  dpy cmap colors reds greens blues contiguous =
  sendRequestAndGetReply dpy
    (allocColorPlanesReq  cmap colors reds greens blues contiguous)
    allocColorPlanesRep

let freeColors  dpy cmap plane_mask pixels =
  sendRequest dpy (freeColorsReq cmap plane_mask pixels)

let lookupColor  dpy cmap name =
  sendRequestAndGetReply dpy
    (lookupColorReq cmap name) lookupColorRep

let storeColors  dpy cmap list =
  sendRequest dpy (storeColorsReq cmap list)

let storeNamedColor  dpy cmap pixel mask name =
  sendRequest dpy
    (storeNamedColorReq cmap  pixel mask name)

let queryColors  dpy cmap list =
  sendRequestAndGetReply dpy
    (queryColorsReq cmap list)
    queryColorsRep

let createColormap dpy window visual alloc =
  let colormap = id_to_colormap (alloc_id dpy) in
  sendRequest dpy (createColormapReq colormap  window visual alloc);
  colormap

let freeColormap  dpy id =
  sendRequest dpy (freeColormapReq id)

let copyColormapAndFree dpy id =
  let colormap = id_to_colormap (alloc_id dpy) in
  sendRequest dpy (copyColormapAndFreeReq colormap  id);
  colormap

let installColormap dpy id =
  sendRequest dpy (installColormapReq id)

let uninstallColormap dpy id =
  sendRequest dpy (uninstallColormapReq id)

let listInstalledColormaps dpy win =
  sendRequestAndGetReply dpy
    (listInstalledColormapsReq win)
    listInstalledColormapsRep

let createWindow dpy parent x y dx dy depth wa_class visual
    border_width swa =
  let window = id_to_window (alloc_id dpy) in
  sendRequest dpy 
    (createWindowReq window parent x y dx dy depth wa_class visual
       border_width swa);
  window

let changeWindowAttributes  dpy win swa =
  sendRequest dpy (changeWindowAttributesReq win swa)

let configureWindow dpy win cw =
  sendRequest dpy (configureWindowReq win cw)

let createCursor dpy src mask fred fgreen fblue bred bgreen bblue x y =
  let cursor = id_to_cursor (alloc_id dpy) in
  sendRequest dpy 
    (createCursorReq cursor src mask fred fgreen fblue bred bgreen bblue x y);
  cursor

let createGlyphCursor dpy src mask src_char mask_char 
    fred fgreen fblue bred bgreen bblue =
  let cursor = id_to_cursor (alloc_id dpy) in
  sendRequest dpy 
    (createGlyphCursorReq cursor src mask src_char mask_char 
       fred fgreen fblue bred bgreen bblue);
  cursor

let freeCursor dpy cid =
  sendRequest dpy (freeCursorReq cid)

let recolorCursor  dpy cursor fred fgreen fblue bred bgreen bblue =
  sendRequest dpy
    (recolorCursorReq cursor fred fgreen fblue bred bgreen bblue)

let queryBestSize  dpy objet window width height =
  sendRequestAndGetReply dpy 
    (queryBestSizeReq  objet window width height)
    queryBestSizeRep

let queryExtension dpy name =
  sendRequestAndGetReply dpy
    (queryExtensionReq name)
    queryExtensionRep

let listExtensions dpy =
  sendRequestAndGetReply dpy
    (listExtensionsReq ())
    listExtensionsRep

let openFont dpy name =
  let font = id_to_font (alloc_id dpy) in
  sendRequest dpy (openFontReq font name);
  font

let closeFont dpy fid =
  sendRequest dpy (closeFontReq fid)

let queryTextExtents  dpy fid string =
  sendRequestAndGetReply dpy
    (queryTextExtentsReq  fid string)
    queryTextExtentsRep

let setFontPath  dpy list =
  sendRequest dpy (setFontPathReq list)

let getFontPath dpy =
  sendRequestAndGetReply dpy
    (getFontPathReq ())
    getFontPathRep

let listFonts  dpy pattern max_names =
  sendRequestAndGetReply dpy
    (listFontsReq  pattern max_names)
    listFontsRep

let listFontsWithInfo dpy pattern max_names =
  failwith "listFontsWithInfo: Not implemented"

let queryFont dpy fid =
  sendRequestAndGetReply dpy 
    (queryFontReq fid)
    queryFontRep

let setDashes  dpy gc dash_offset dashes =
  sendRequest dpy
    (setDashesReq  gc dash_offset dashes)

let createGC  dpy window arg =
  let gc = id_to_gc (alloc_id dpy) in
  sendRequest dpy
    (createGCReq gc window arg);
  gc

let changeGC  dpy gc arg =
  sendRequest dpy (changeGCReq gc arg)

let setClipRectangles  dpy gc ordering clip_x clip_y rects =
  sendRequest dpy (setClipRectanglesReq  gc ordering clip_x clip_y rects)

let copyGC  dpy src dst mask =
  sendRequest dpy (copyGCReq src dst mask)

let freeGC dpy gc =
  sendRequest dpy (freeGCReq gc)

let getGeometry  dpy window =
  sendRequestAndGetReply dpy
    (getGeometryReq window)
    getGeometryRep

let queryTree  dpy window =
  sendRequestAndGetReply dpy
    (queryTreeReq window)
    queryTreeRep

let getWindowAttributes dpy win =
  sendRequestAndGetReply dpy
    (getWindowAttributesReq win)
    getWindowAttributesRep

let ungrabPointer dpy time =
  sendRequest dpy (ungrabPointerReq time)

let grabPointer  dpy grab_window owner_events event_mask pointer_mode
    keyboard_mode confine_to cursor time =
  sendRequestAndGetReply dpy
    (grabPointerReq grab_window owner_events event_mask pointer_mode
       keyboard_mode confine_to cursor time)
    grabPointerRep

let grabButton   dpy grab_window owner_events event_mask pointer_mode
    keyboard_mode confine_to cursor button modifiers =
  sendRequest dpy
    (grabButtonReq grab_window owner_events event_mask pointer_mode
       keyboard_mode confine_to cursor button modifiers)


let ungrabButton  dpy grab_window button modifiers =
  sendRequest dpy
    (ungrabButtonReq grab_window button modifiers)

let changeActivePointerGrab  dpy event_mask cursor time =
  sendRequest dpy
    (changeActivePointerGrabReq event_mask cursor time)

let grabKeyboard  dpy grab_window owner_events pointer_mode
    keyboard_mode time =
  sendRequestAndGetReply dpy
    (grabKeyboardReq grab_window owner_events pointer_mode
    keyboard_mode time)
    grabKeyboardRep

let ungrabKeyboard dpy time =
  sendRequest dpy
    (ungrabKeyboardReq time)

let ungrabServer dpy =
  sendRequest dpy (ungrabServerReq ())


let grabServer dpy =
  sendRequest dpy (grabServerReq ())
  
let grabKey  dpy grab_window owner_events pointer_mode
    keyboard_mode key modifiers =
  sendRequest dpy
    (grabKeyReq grab_window owner_events pointer_mode
       keyboard_mode key modifiers)

let ungrabKey dpy grab_window key modifiers =
  sendRequest dpy
    (ungrabKeyReq grab_window key modifiers)

let allowEvents   dpy mode time =
  sendRequest dpy
    (allowEventsReq mode time)

let changeHosts dpy mode family name =
  sendRequest dpy
    (changeHostsReq mode family name)

let listHosts dpy =
  sendRequestAndGetReply dpy (listHostsReq ())
    listHostsRep

let setAccessControl dpy acc =
  sendRequest dpy (setAccessControlReq acc)

let putImage  dpy gc window x y dx dy left_pad depth format image =
  sendRequest dpy
    (putImageReq gc window x y dx dy left_pad depth format image)

let getImage  dpy window x y dx dy plane_mask format =
  sendRequestAndGetReply dpy
    (getImageReq window x y dx dy plane_mask format)
    getImageRep

let setInputFocus dpy win mode time =
  sendRequest dpy (setInputFocusReq win mode time)

let getInputFocus dpy =
  sendRequestAndGetReply dpy (getInputFocusReq ()) getInputFocusRep

let queryKeymap dpy =
  sendRequestAndGetReply dpy (queryKeymapReq ()) queryKeymapRep

let changeKeyboardMapping dpy keys keycodes =
  sendRequest dpy (changeKeyboardMappingReq keys keycodes)

let getKeyboardMapping  dpy first m =
  sendRequestAndGetReply dpy 
    (getKeyboardMappingReq first m)
    getKeyboardMappingRep

let changeKeyboardControl dpy kb = 
  sendRequest dpy (changeKeyboardControlReq kb)

let getKeyboardControl dpy =
  sendRequestAndGetReply dpy 
    (getKeyboardControlReq ())
  getKeyboardControlRep

let bell dpy d =
  sendRequest dpy (bellReq d)

let changePointerControl  dpy do_acc acc_num acc_den do_thres threshold =
    sendRequest dpy 
    (changePointerControlReq do_acc acc_num acc_den do_thres threshold)

let getPointerControl  dpy =
  sendRequestAndGetReply dpy (getPointerControlReq ())
    getPointerControlRep

let circulateWindow dpy win sens =
  sendRequest dpy (circulateWindowReq win sens)

let destroySubwindows dpy win =
  sendRequest dpy (destroySubwindowsReq win)

let destroyWindow dpy win =
  sendRequest dpy (destroyWindowReq win)

let unmapSubwindows dpy win =
  sendRequest dpy (unmapSubwindowsReq win)

let unmapWindow dpy win =
  sendRequest dpy (unmapWindowReq win)

let mapSubwindows dpy win =
  sendRequest dpy (mapSubwindowsReq win)

let mapWindow dpy win =
  sendRequest dpy (mapWindowReq win)

let setPointerMapping dpy map =
  sendRequestAndGetReply dpy (setPointerMappingReq map)
    setPointerMappingRep

let getPointerMapping dpy =
  sendRequestAndGetReply dpy (getPointerMappingReq ())
    getPointerMappingRep

let setModifierMapping  dpy list =
  sendRequestAndGetReply dpy
    (setModifierMappingReq list)
    setModifierMappingRep

let getModifierMapping dpy =
  sendRequestAndGetReply dpy
    (getModifierMappingReq ())
    getModifierMappingRep

let setCloseDownMode dpy mode =
  sendRequest dpy (setCloseDownModeReq mode)

let killClient dpy id =
  sendRequest dpy (killClientReq id)


let createPixmap dpy window dx dy depth =
  let pixmap = id_to_pixmap (alloc_id dpy) in
  sendRequest dpy
    (createPixmapReq pixmap window dx dy depth);
  pixmap

let freePixmap dpy pixmap =
  sendRequest dpy (freePixmapReq pixmap)

let queryPointer dpy win =
  sendRequestAndGetReply dpy
    (queryPointerReq win)
    queryPointerRep

let getMotionEvents dpy win start stop =
  sendRequestAndGetReply dpy
    (getMotionEventsReq win start stop)
    getMotionEventsRep

let translateCoordinates dpy root win x y =
  sendRequestAndGetReply dpy
    (translateCoordinatesReq root win x y)
    translateCoordinatesRep

let warpPointer  dpy src_window src_x src_y src_width src_height
    dst_window x y = 
  sendRequest dpy 
    (warpPointerReq src_window src_x src_y src_width src_height
       dst_window x y)

let polyPoint dpy win gc mode list =
  sendRequest dpy (polyPointReq win gc mode list)

let polyLine  dpy win gc mode list =
  sendRequest dpy (polyLineReq win gc mode list)

let polySegment  dpy win gc list =
  sendRequest dpy (polySegmentReq win gc list)

let polyRectangle  dpy win gc list =
  sendRequest dpy (polyRectangleReq win gc list)

let polyArc  dpy win gc list =
  sendRequest dpy (polyArcReq win gc list)

let fillPoly dpy window gc shape coord_mode list =
  sendRequest dpy (fillPolyReq window gc shape coord_mode list)

let polyFillRectangle   dpy win gc list =
  sendRequest dpy (polyFillRectangleReq win gc list)

let polyFillArc  dpy win gc list =
  sendRequest dpy (polyFillArcReq win gc list)

let listProperties dpy win =
  sendRequestAndGetReply dpy (listPropertiesReq win) listPropertiesRep

let internAtom dpy name create =
  sendRequestAndGetReply dpy (internAtomReq name create) internAtomRep

let getAtomName dpy atom =
  sendRequestAndGetReply dpy (getAtomNameReq atom) getAtomNameRep

let getSelectionOwner dpy atom =
  sendRequestAndGetReply dpy (getSelectionOwnerReq atom)
    getSelectionOwnerRep

let changeProperty dpy window mode property property_type format buffer =
  sendRequest dpy
    (changePropertyReq window mode property property_type format buffer)
    
let getProperty  dpy window mode property property_type offset lenght =
  sendRequestAndGetReply dpy 
    (getPropertyReq window mode property property_type offset lenght)
    getPropertyRep


let rotateProperties  dpy window delta list =
  sendRequest dpy (rotatePropertiesReq  window delta list)

let deleteProperty dpy win atom =
  sendRequest dpy (deletePropertyReq win atom)

let setScreenSaver  dpy timeout interval prefer_blanking allow_exposures =
  sendRequest dpy 
    (setScreenSaverReq timeout interval prefer_blanking allow_exposures)

let getScreenSaver dpy =
  sendRequestAndGetReply dpy
    (getScreenSaverReq ()) getScreenSaverRep

let forceScreenSaver  dpy state =
  sendRequest dpy (forceScreenSaverReq state)

let setSelectionOwner dpy win atom time =
  sendRequest dpy
    (setSelectionOwnerReq win atom time)

let convertSelection dpy win selection target property time =
  sendRequest dpy
    (convertSelectionReq  win selection target property time)

let sendEvent  dpy win propagate eventMask event =
  sendRequest dpy (sendEventReq  win propagate eventMask event)

let imageText8 dpy window gc x y string =
  sendRequest dpy (imageText8Req window gc x y string)

let imageSubText8 dpy window gc x y string pos len =
  sendRequest dpy (imageSubText8Req window gc x y string pos len)

let imageText16  dpy window gc x y string =
  sendRequest dpy (imageText16Req window gc x y string)

let polyText8  dpy window gc x y list =
  sendRequest dpy (polyText8Req window gc x y list)

let polyText16 dpy window gc x y list =
  sendRequest dpy (polyText16Req window gc x y list)
    

