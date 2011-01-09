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
val sendRequest : Xtypes.display -> string -> unit
val sendRequestAndGetReply : Xtypes.display -> string -> (string -> 'a) -> 'a
val clearArea :
  Xtypes.display -> Xtypes.window -> int -> int -> int -> int -> 'a -> unit
val clearWindow : Xtypes.display -> Xtypes.window -> unit
val copyArea :
  Xtypes.display ->
  Xtypes.gc ->
  Xtypes.window ->
  int -> int -> Xtypes.window -> int -> int -> int -> int -> unit
val copyPlane :
  Xtypes.display ->
  Xtypes.gc ->
  Xtypes.window ->
  int -> int -> Xtypes.window -> int -> int -> int -> int -> int -> unit
val changeSaveSet : Xtypes.display -> Xtypes.window -> 'a -> unit
val reparentWindow :
  Xtypes.display -> Xtypes.window -> Xtypes.window -> int -> int -> unit
val allocColor :
  Xtypes.display ->
  Xtypes.colormap -> int -> int -> int -> Xtypes.allocColorRep
val allocNamedColor :
  Xtypes.display -> Xtypes.colormap -> string -> Xtypes.allocNamedColorRep
val allocColorCells :
  Xtypes.display ->
  Xtypes.colormap -> int -> int -> 'a -> Xtypes.allocColorCellsRep
val allocColorPlanes :
  Xtypes.display ->
  Xtypes.colormap ->
  int -> int -> int -> int -> 'a -> Xtypes.allocColorPlanesRep
val freeColors :
  Xtypes.display -> Xtypes.colormap -> int -> Xtypes.pixel list -> unit
val lookupColor :
  Xtypes.display -> Xtypes.colormap -> string -> Xtypes.lookupColorRep
val storeColors :
  Xtypes.display ->
  Xtypes.colormap -> (Xtypes.pixel * int * int * int * 'a list) list -> unit
val storeNamedColor :
  Xtypes.display ->
  Xtypes.colormap -> Xtypes.pixel -> 'a list -> string -> unit
val queryColors :
  Xtypes.display -> Xtypes.colormap -> 'a list -> Xtypes.color array
val createColormap :
  Xtypes.display -> Xtypes.window -> Xtypes.visual -> 'a -> Xtypes.colormap
val freeColormap : Xtypes.display -> Xtypes.colormap -> unit
val copyColormapAndFree :
  Xtypes.display -> Xtypes.colormap -> Xtypes.colormap
val installColormap : Xtypes.display -> Xtypes.colormap -> unit
val uninstallColormap : Xtypes.display -> Xtypes.colormap -> unit
val listInstalledColormaps :
  Xtypes.display -> Xtypes.window -> Xtypes.colormap list
val createWindow :
  Xtypes.display ->
  Xtypes.window ->
  int ->
  int ->
  int ->
  int ->
  int ->
  'a ->
  Xtypes.visual -> int -> Xtypes.setWindowAttributes list -> Xtypes.window
val changeWindowAttributes :
  Xtypes.display -> Xtypes.window -> Xtypes.setWindowAttributes list -> unit
val configureWindow :
  Xtypes.display -> Xtypes.window -> Xtypes.configureWindow list -> unit
val createCursor :
  Xtypes.display ->
  Xtypes.pixmap ->
  Xtypes.pixmap ->
  int -> int -> int -> int -> int -> int -> int -> int -> Xtypes.cursor
val createGlyphCursor :
  Xtypes.display ->
  Xtypes.font ->
  Xtypes.font ->
  int -> int -> int -> int -> int -> int -> int -> int -> Xtypes.cursor
val freeCursor : Xtypes.display -> Xtypes.cursor -> unit
val recolorCursor :
  Xtypes.display ->
  Xtypes.cursor -> int -> int -> int -> int -> int -> int -> unit
val queryBestSize :
  Xtypes.display ->
  'a -> Xtypes.window -> int -> int -> Xtypes.queryBestSizeRep
val queryExtension : Xtypes.display -> string -> Xtypes.queryExtensionRep
val listExtensions : Xtypes.display -> string list
val openFont : Xtypes.display -> string -> Xtypes.font
val closeFont : Xtypes.display -> Xtypes.font -> unit
val queryTextExtents :
  Xtypes.display -> Xtypes.font -> string -> Xtypes.queryTextExtentsRep
val setFontPath : Xtypes.display -> string list -> unit
val getFontPath : Xtypes.display -> string list
val listFonts : Xtypes.display -> string -> int -> string list
val listFontsWithInfo : 'a -> 'b -> 'c -> 'd
val queryFont : Xtypes.display -> Xtypes.font -> Xtypes.queryFontRep
val setDashes : Xtypes.display -> Xtypes.gc -> int -> string -> unit
val createGC :
  Xtypes.display -> Xtypes.window -> Xtypes.setGCattributes list -> Xtypes.gc
val changeGC :
  Xtypes.display -> Xtypes.gc -> Xtypes.setGCattributes list -> unit
val setClipRectangles :
  Xtypes.display ->
  Xtypes.gc -> 'a -> int -> int -> (int * int * int * int) list -> unit
val copyGC : Xtypes.display -> Xtypes.gc -> Xtypes.gc -> 'a list -> unit
val freeGC : Xtypes.display -> Xtypes.gc -> unit
val getGeometry : Xtypes.display -> Xtypes.window -> Xtypes.getGeometryRep
val queryTree : Xtypes.display -> Xtypes.window -> Xtypes.queryTreeRep
val getWindowAttributes :
  Xtypes.display -> Xtypes.window -> Xtypes.getWindowAttributesRep
val ungrabPointer : Xtypes.display -> Xtypes.time -> unit
  val grabPointer :
    Xtypes.display ->
    Xtypes.window ->
    bool ->
    Xtypes.eventMask list ->
    Xtypes.grabMode ->
  Xtypes.grabMode -> Xtypes.window -> Xtypes.cursor -> Xtypes.time -> unit
    val grabButton :
    Xtypes.display ->
    Xtypes.window ->
    bool ->
    Xtypes.eventMask list ->
    Xtypes.grabMode ->
    Xtypes.grabMode -> Xtypes.window -> Xtypes.cursor -> int -> int -> unit
val ungrabButton : Xtypes.display -> Xtypes.window -> int -> int -> unit
val changeActivePointerGrab :
  Xtypes.display -> Xtypes.eventMask list -> Xtypes.cursor -> Xtypes.time -> unit
  val grabKeyboard :
    Xtypes.display ->
    Xtypes.window ->
    bool -> Xtypes.grabMode -> Xtypes.grabMode -> Xtypes.time -> unit
val ungrabKeyboard : Xtypes.display -> Xtypes.time -> unit
val ungrabServer : Xtypes.display -> unit
val grabServer : Xtypes.display -> unit
  val grabKey :
    Xtypes.display ->
    Xtypes.window ->
    bool -> Xtypes.grabMode -> Xtypes.grabMode -> int -> int -> unit
val ungrabKey : Xtypes.display -> Xtypes.window -> int -> int -> unit
val allowEvents : Xtypes.display -> 'a -> Xtypes.time -> unit
val changeHosts : Xtypes.display -> 'a -> 'b -> string -> unit
val listHosts : Xtypes.display -> Xtypes.listHostsRep
val setAccessControl : Xtypes.display -> 'a -> unit
val putImage :
  Xtypes.display ->
  Xtypes.gc ->
  Xtypes.window ->
  int -> int -> int -> int -> int -> int -> 'a -> string -> unit
val getImage :
  Xtypes.display ->
  Xtypes.window ->
  int -> int -> int -> int -> int -> 'a -> Xtypes.getImageRep
val setInputFocus :
  Xtypes.display -> Xtypes.window -> 'a -> Xtypes.time -> unit
val getInputFocus : Xtypes.display -> Xtypes.getInputFocusRep
val queryKeymap : Xtypes.display -> int array
val changeKeyboardMapping : Xtypes.display -> int -> int array array -> unit
val getKeyboardMapping :
  Xtypes.display -> int -> int -> Xtypes.getKeyboardMappingRep
val changeKeyboardControl : Xtypes.display -> Xtypes.setKBvalues list -> unit
val getKeyboardControl : Xtypes.display -> Xtypes.getKeyboardControlRep
val bell : Xtypes.display -> int -> unit
val changePointerControl :
  Xtypes.display -> 'a -> int -> int -> 'b -> int -> unit
val getPointerControl : Xtypes.display -> Xtypes.getPointerControlRep
val circulateWindow :
  Xtypes.display -> Xtypes.window -> Xtypes.direction -> unit
val destroySubwindows : Xtypes.display -> Xtypes.window -> unit
val destroyWindow : Xtypes.display -> Xtypes.window -> unit
val unmapSubwindows : Xtypes.display -> Xtypes.window -> unit
val unmapWindow : Xtypes.display -> Xtypes.window -> unit
val mapSubwindows : Xtypes.display -> Xtypes.window -> unit
val mapWindow : Xtypes.display -> Xtypes.window -> unit
val setPointerMapping : Xtypes.display -> string -> unit
val getPointerMapping : Xtypes.display -> string
val setModifierMapping : Xtypes.display -> int array list -> unit
val getModifierMapping : Xtypes.display -> int array array
val setCloseDownMode : Xtypes.display -> 'a -> unit
val killClient : Xtypes.display -> int -> unit
val createPixmap :
  Xtypes.display -> Xtypes.window -> int -> int -> int -> Xtypes.pixmap
val freePixmap : Xtypes.display -> Xtypes.window -> unit
val queryPointer : Xtypes.display -> Xtypes.window -> Xtypes.queryPointerRep
val getMotionEvents :
  Xtypes.display ->
  Xtypes.window -> Xtypes.time -> Xtypes.time -> Xtypes.motionEvent list
val translateCoordinates :
  Xtypes.display ->
  Xtypes.window -> Xtypes.window -> int -> int -> Xtypes.motionEvent
val warpPointer :
  Xtypes.display ->
  Xtypes.window ->
  int -> int -> int -> int -> Xtypes.window -> int -> int -> unit
val polyPoint :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> Xtypes.coordMode -> (int * int) list -> unit
val polyLine :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> Xtypes.coordMode -> (int * int) list -> unit
val polySegment :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> (int * int * int * int) list -> unit
val polyRectangle :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> (int * int * int * int) list -> unit
val polyArc :
  Xtypes.display ->
  Xtypes.window ->
  Xtypes.gc -> (int * int * int * int * int * int) list -> unit
val fillPoly :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> 'a -> 'b -> (int * int) list -> unit
val polyFillRectangle :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> (int * int * int * int) list -> unit
val polyFillArc :
  Xtypes.display ->
  Xtypes.window ->
  Xtypes.gc -> (int * int * int * int * int * int) list -> unit
val listProperties : Xtypes.display -> Xtypes.window -> Xtypes.atom list
val internAtom : Xtypes.display -> string -> 'a -> Xtypes.atom
val getAtomName : Xtypes.display -> Xtypes.atom -> string
val getSelectionOwner : Xtypes.display -> Xtypes.atom -> Xtypes.window
val changeProperty :
  Xtypes.display ->
  Xtypes.window -> 'a -> Xtypes.atom -> Xtypes.atom -> int -> string -> unit
val getProperty :
  Xtypes.display ->
  Xtypes.window ->
  'a -> Xtypes.atom -> Xtypes.atom -> int -> int -> Xtypes.getPropertyRep
val rotateProperties :
  Xtypes.display -> Xtypes.window -> int -> Xtypes.atom list -> unit
val deleteProperty : Xtypes.display -> Xtypes.window -> Xtypes.atom -> unit
val setScreenSaver : Xtypes.display -> int -> int -> 'a -> 'b -> unit
val getScreenSaver : Xtypes.display -> Xtypes.getScreenSaverRep
val forceScreenSaver : Xtypes.display -> 'a -> unit
val setSelectionOwner :
  Xtypes.display -> Xtypes.window -> Xtypes.atom -> Xtypes.time -> unit
val convertSelection :
  Xtypes.display ->
  Xtypes.window ->
  Xtypes.atom -> Xtypes.atom -> Xtypes.atom -> Xtypes.time -> unit
val sendEvent :
  Xtypes.display ->
  Xtypes.window -> bool -> Xtypes.eventMask list -> Xtypes.event -> unit
val imageText8 :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> string -> unit
val imageSubText8 :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> string -> int -> int -> unit
val imageText16 :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> int array -> unit
val polyText8 :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> Xtypes.textItem list -> unit
val polyText16 :
  Xtypes.display ->
  Xtypes.window -> Xtypes.gc -> int -> int -> Xtypes.textItem list -> unit
