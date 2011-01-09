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

val simpleWindowRequest : Xtypes.requestOpcode -> Xtypes.window -> string
val simpleFontRequest : Xtypes.requestOpcode -> Xtypes.font -> string
val simpleColormapRequest : Xtypes.requestOpcode -> Xtypes.colormap -> string
val strListLength : string list -> int
val getCharInfo : string -> int -> Xtypes.charInfo
val getFontProp : string -> int -> 'a * int
val getOneFontInfo : string -> Xtypes.fontInfo
val copyDatas :
  string ->
  Xtypes.gc ->
  Xtypes.window ->
  int -> int -> Xtypes.window -> int -> int -> int -> int -> unit
val textItemLength : Xtypes.textItem -> int
val textItemListLength : Xtypes.textItem list -> int
val setTextItem8 : string -> int -> Xtypes.textItem list -> unit
val setTextItem16 : string -> int -> Xtypes.textItem list -> unit
val createWindowReq :
  Xtypes.window ->
  Xtypes.window ->
  int ->
  int ->
  int ->
  int ->
  int ->
  'a -> Xtypes.visual -> int -> Xtypes.setWindowAttributes list -> string
val changeWindowAttributesReq :
  Xtypes.window -> Xtypes.setWindowAttributes list -> string
val getWindowAttributesReq : Xtypes.window -> string
val getWindowAttributesRep : string -> Xtypes.getWindowAttributesRep
val destroyWindowReq : Xtypes.window -> string
val destroySubwindowsReq : Xtypes.window -> string
val changeSaveSetReq : Xtypes.window -> 'a -> string
val reparentWindowReq :
  Xtypes.window -> Xtypes.window -> int -> int -> string
val mapWindowReq : Xtypes.window -> string
val mapSubwindowsReq : Xtypes.window -> string
val unmapWindowReq : Xtypes.window -> string
val unmapSubwindowsReq : Xtypes.window -> string
val configureWindowReq :
  Xtypes.window -> Xtypes.configureWindow list -> string
val circulateWindowReq : Xtypes.window -> Xtypes.direction -> string
val getGeometryReq : Xtypes.window -> string
val getGeometryRep : string -> Xtypes.getGeometryRep
val queryTreeReq : Xtypes.window -> string
val queryTreeRep : string -> Xtypes.queryTreeRep
val internAtomReq : string -> 'a -> string
val internAtomRep : string -> Xtypes.atom
val getAtomNameReq : Xtypes.atom -> string
val getAtomNameRep : string -> string
val changePropertyReq :
  Xtypes.window ->
  'a -> Xtypes.atom -> Xtypes.atom -> int -> string -> string
val deletePropertyReq : Xtypes.window -> Xtypes.atom -> string
val getPropertyReq :
  Xtypes.window -> 'a -> Xtypes.atom -> Xtypes.atom -> int -> int -> string
val getPropertyRep : string -> Xtypes.getPropertyRep
val listPropertiesReq : Xtypes.window -> string
val listPropertiesRep : string -> Xtypes.atom list
val setSelectionOwnerReq :
  Xtypes.window -> Xtypes.atom -> Xtypes.time -> string
val getSelectionOwnerReq : Xtypes.atom -> string
val getSelectionOwnerRep : string -> Xtypes.window
val convertSelectionReq :
  Xtypes.window ->
  Xtypes.atom -> Xtypes.atom -> Xtypes.atom -> Xtypes.time -> string
val sendEventReq :
  Xtypes.window -> bool -> Xtypes.eventMask list -> Xtypes.event -> string
  val grabPointerReq :
    Xtypes.window ->
    bool ->
    Xtypes.eventMask list ->
    Xtypes.grabMode ->
    Xtypes.grabMode ->
    Xtypes.window -> Xtypes.cursor -> Xtypes.time -> string
val grabPointerRep : string -> unit
val ungrabPointerReq : Xtypes.time -> string
  val grabButtonReq :
    Xtypes.window ->
    bool ->
    Xtypes.eventMask list ->
    Xtypes.grabMode ->
    Xtypes.grabMode -> Xtypes.window -> Xtypes.cursor -> int -> int -> string
val ungrabButtonReq : Xtypes.window -> int -> int -> string
val changeActivePointerGrabReq : Xtypes.eventMask list -> Xtypes.cursor -> Xtypes.time -> string
    val grabKeyboardReq :
    Xtypes.window ->
    bool -> Xtypes.grabMode -> Xtypes.grabMode -> Xtypes.time -> string
val grabKeyboardRep : string -> unit
val ungrabKeyboardReq : Xtypes.time -> string
  val grabKeyReq :
    Xtypes.window ->
    bool -> Xtypes.grabMode -> Xtypes.grabMode -> int -> int -> string
val ungrabKeyReq : Xtypes.window -> int -> int -> string
val allowEventsReq : 'a -> Xtypes.time -> string
val grabServerReq : unit -> string
val ungrabServerReq : unit -> string
val queryPointerReq : Xtypes.window -> string
val queryPointerRep : string -> Xtypes.queryPointerRep
val getMotionEventsReq : Xtypes.window -> Xtypes.time -> Xtypes.time -> string
val getMotionEventsRep : string -> Xtypes.motionEvent list
val translateCoordinatesReq :
  Xtypes.window -> Xtypes.window -> int -> int -> string
val translateCoordinatesRep : string -> Xtypes.motionEvent
val warpPointerReq :
  Xtypes.window ->
  int -> int -> int -> int -> Xtypes.window -> int -> int -> string
val setInputFocusReq : Xtypes.window -> 'a -> Xtypes.time -> string
val getInputFocusReq : unit -> string
val getInputFocusRep : string -> Xtypes.getInputFocusRep
val queryKeymapReq : unit -> string
val queryKeymapRep : string -> int array
val openFontReq : Xtypes.font -> string -> string
val closeFontReq : Xtypes.font -> string
val queryFontReq : Xtypes.font -> string
val queryFontRep : string -> Xtypes.queryFontRep
val queryTextExtentsReq : Xtypes.font -> string -> string
val queryTextExtentsRep : string -> Xtypes.queryTextExtentsRep
val listFontsReq : string -> int -> string
val listFontsRep : string -> string list
val listFontsWithInfoReq : 'a -> 'b -> 'c
val listFontsWithInfoRep : 'a -> 'b
val setFontPathReq : string list -> string
val getFontPathReq : unit -> string
val getFontPathRep : string -> string list
val createPixmapReq :
  Xtypes.pixmap -> Xtypes.window -> int -> int -> int -> string
val freePixmapReq : Xtypes.window -> string
val createGCReq :
  Xtypes.gc -> Xtypes.window -> Xtypes.setGCattributes list -> string
val changeGCReq : Xtypes.gc -> Xtypes.setGCattributes list -> string
val copyGCReq : Xtypes.gc -> Xtypes.gc -> 'a list -> string
val setDashesReq : Xtypes.gc -> int -> string -> string
val setClipRectanglesReq :
  Xtypes.gc -> 'a -> int -> int -> (int * int * int * int) list -> string
val freeGCReq : Xtypes.gc -> string
val clearAreaReq : Xtypes.window -> int -> int -> int -> int -> 'a -> string
val clearWindowReq : Xtypes.window -> string
val copyAreaReq :
  Xtypes.gc ->
  Xtypes.window ->
  int -> int -> Xtypes.window -> int -> int -> int -> int -> string
val copyPlaneReq :
  Xtypes.gc ->
  Xtypes.window ->
  int -> int -> Xtypes.window -> int -> int -> int -> int -> int -> string
val polyPointReq : Xtypes.window -> Xtypes.gc -> Xtypes.coordMode -> (int * int) list -> string
val polyLineReq : Xtypes.window -> Xtypes.gc -> Xtypes.coordMode -> (int * int) list -> string
val polySegmentReq :
  Xtypes.window -> Xtypes.gc -> (int * int * int * int) list -> string
val polyRectangleReq :
  Xtypes.window -> Xtypes.gc -> (int * int * int * int) list -> string
val polyArcReq :
  Xtypes.window ->
  Xtypes.gc -> (int * int * int * int * int * int) list -> string
val fillPolyReq :
  Xtypes.window -> Xtypes.gc -> 'a -> 'b -> (int * int) list -> string
val polyFillRectangleReq :
  Xtypes.window -> Xtypes.gc -> (int * int * int * int) list -> string
val polyFillArcReq :
  Xtypes.window ->
  Xtypes.gc -> (int * int * int * int * int * int) list -> string
val putImageReq :
  Xtypes.gc ->
  Xtypes.window ->
  int -> int -> int -> int -> int -> int -> 'a -> string -> string
val getImageReq :
  Xtypes.window -> int -> int -> int -> int -> int -> 'a -> string
val getImageRep : string -> Xtypes.getImageRep
val polyText8Req :
  Xtypes.window -> Xtypes.gc -> int -> int -> Xtypes.textItem list -> string
val polyText16Req :
  Xtypes.window -> Xtypes.gc -> int -> int -> Xtypes.textItem list -> string
val imageText8Req :
  Xtypes.window -> Xtypes.gc -> int -> int -> string -> string
val imageSubText8Req :
  Xtypes.window -> Xtypes.gc -> int -> int -> string -> int -> int -> string
val imageText16Req :
  Xtypes.window -> Xtypes.gc -> int -> int -> int array -> string
val createColormapReq :
  Xtypes.colormap -> Xtypes.window -> Xtypes.visual -> 'a -> string
val freeColormapReq : Xtypes.colormap -> string
val copyColormapAndFreeReq : Xtypes.colormap -> Xtypes.colormap -> string
val installColormapReq : Xtypes.colormap -> string
val uninstallColormapReq : Xtypes.colormap -> string
val listInstalledColormapsReq : Xtypes.window -> string
val listInstalledColormapsRep : string -> Xtypes.colormap list
val allocColorReq : Xtypes.colormap -> int -> int -> int -> string
val allocColorRep : string -> Xtypes.allocColorRep
val allocNamedColorReq : Xtypes.colormap -> string -> string
val allocNamedColorRep : string -> Xtypes.allocNamedColorRep
val allocColorCellsReq : Xtypes.colormap -> int -> int -> 'a -> string
val allocColorCellsRep : string -> Xtypes.allocColorCellsRep
val allocColorPlanesReq :
  Xtypes.colormap -> int -> int -> int -> int -> 'a -> string
val allocColorPlanesRep : string -> Xtypes.allocColorPlanesRep
val freeColorsReq : Xtypes.colormap -> int -> Xtypes.pixel list -> string
val storeColorsReq :
  Xtypes.colormap ->
  (Xtypes.pixel * int * int * int * 'a list) list -> string
val storeNamedColorReq :
  Xtypes.colormap -> Xtypes.pixel -> 'a list -> string -> string
val queryColorsReq : Xtypes.colormap -> 'a list -> string
val queryColorsRep : string -> Xtypes.color array
val lookupColorReq : Xtypes.colormap -> string -> string
val lookupColorRep : string -> Xtypes.lookupColorRep
val createCursorReq :
  Xtypes.cursor ->
  Xtypes.pixmap ->
  Xtypes.pixmap ->
  int -> int -> int -> int -> int -> int -> int -> int -> string
val createGlyphCursorReq :
  Xtypes.cursor ->
  Xtypes.font ->
  Xtypes.font ->
  int -> int -> int -> int -> int -> int -> int -> int -> string
val freeCursorReq : Xtypes.cursor -> string
val recolorCursorReq :
  Xtypes.cursor -> int -> int -> int -> int -> int -> int -> string
val queryBestSizeReq : 'a -> Xtypes.window -> int -> int -> string
val queryBestSizeRep : string -> Xtypes.queryBestSizeRep
val queryExtensionReq : string -> string
val queryExtensionRep : string -> Xtypes.queryExtensionRep
val listExtensionsReq : unit -> string
val listExtensionsRep : string -> string list
val changeKeyboardMappingReq : int -> int array array -> string
val getKeyboardMappingReq : int -> int -> string
val getKeyboardMappingRep : string -> Xtypes.getKeyboardMappingRep
val changeKeyboardControlReq : Xtypes.setKBvalues list -> string
val getKeyboardControlReq : unit -> string
val getKeyboardControlRep : string -> Xtypes.getKeyboardControlRep
val bellReq : int -> string
val changePointerControlReq : 'a -> int -> int -> 'b -> int -> string
val getPointerControlReq : unit -> string
val getPointerControlRep : string -> Xtypes.getPointerControlRep
val setScreenSaverReq : int -> int -> 'a -> 'b -> string
val getScreenSaverReq : unit -> string
val getScreenSaverRep : string -> Xtypes.getScreenSaverRep
val changeHostsReq : 'a -> 'b -> string -> string
val listHostsReq : unit -> string
val listHostsRep : string -> Xtypes.listHostsRep
val setAccessControlReq : 'a -> string
val setCloseDownModeReq : 'a -> string
val killClientReq : int -> string
val rotatePropertiesReq : Xtypes.window -> int -> Xtypes.atom list -> string
val forceScreenSaverReq : 'a -> string
val setPointerMappingReq : string -> string
val setPointerMappingRep : string -> unit
val getPointerMappingReq : unit -> string
val getPointerMappingRep : string -> string
val setModifierMappingReq : int array list -> string
val setModifierMappingRep : string -> unit
val getModifierMappingReq : unit -> string
val getModifierMappingRep : string -> int array array

