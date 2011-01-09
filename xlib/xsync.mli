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

val clearArea :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.coord ->
          Xtypes.coord -> Xtypes.size -> Xtypes.size -> bool -> unit
val clearWindow : Xtypes.display -> Xtypes.window -> unit
val copyArea :
    Xtypes.display ->
      Xtypes.gc ->
        Xtypes.window ->
          Xtypes.coord ->
            Xtypes.coord ->
              Xtypes.window ->
                Xtypes.coord ->
                  Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
val copyPlane :
    Xtypes.display ->
      Xtypes.gc ->
        Xtypes.window ->
          Xtypes.coord ->
            Xtypes.coord ->
              Xtypes.window ->
                Xtypes.coord ->
                  Xtypes.coord -> Xtypes.size -> Xtypes.size -> int -> unit
val changeSaveSet :
    Xtypes.display ->
      Xtypes.window -> Xtypes.saveSetOp -> unit
val reparentWindow :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.window -> Xtypes.coord -> Xtypes.coord -> unit
val allocColor :
    Xtypes.display ->
      Xtypes.colormap ->
        Xtypes.red ->
          Xtypes.green ->
            Xtypes.blue -> Xtypes.allocColorRep

val allocNamedColor :
    Xtypes.display ->
      Xtypes.colormap ->
        string -> Xtypes.allocNamedColorRep

val allocColorCells :
    Xtypes.display ->
      Xtypes.colormap ->
        int -> int -> bool -> Xtypes.allocColorCellsRep
val allocColorPlanes :
    Xtypes.display ->
      Xtypes.colormap ->
        int ->
          Xtypes.red ->
            Xtypes.green ->
              Xtypes.blue ->
                bool -> Xtypes.allocColorPlanesRep

val freeColors :
    Xtypes.display ->
      Xtypes.colormap -> int -> Xtypes.pixel list -> unit
val lookupColor :
    Xtypes.display ->
      Xtypes.colormap ->
        string -> Xtypes.lookupColorRep

val storeColors :
    Xtypes.display ->
      Xtypes.colormap ->
        (Xtypes.pixel * Xtypes.red * Xtypes.green * Xtypes.blue *
           Xtypes.colorMask list)
          list -> unit
val storeNamedColor :
    Xtypes.display ->
      Xtypes.colormap ->
        Xtypes.pixel -> Xtypes.colorMask list -> string -> unit
val queryColors :
    Xtypes.display ->
      Xtypes.colormap ->
        Xtypes.pixel list -> 
           Xtypes.color  array
val createColormap :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.visual ->
          Xtypes.allocCells ->
            Xtypes.colormap
val freeColormap :
    Xtypes.display -> Xtypes.colormap -> unit
val copyColormapAndFree :
    Xtypes.display ->
      Xtypes.colormap ->
        Xtypes.colormap
val installColormap :
    Xtypes.display -> Xtypes.colormap -> unit
val uninstallColormap :
    Xtypes.display -> Xtypes.colormap -> unit
val listInstalledColormaps :
    Xtypes.display -> Xtypes.window -> Xtypes.colormap list
val createWindow :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.coord ->
          Xtypes.coord ->
            Xtypes.size ->
              Xtypes.size ->
                int ->
                  Xtypes.waClass ->
                    Xtypes.visual ->
                      int ->
                        Xtypes.setWindowAttributes list ->
                          Xtypes.window
val changeWindowAttributes :
    Xtypes.display ->
      Xtypes.window -> Xtypes.setWindowAttributes list -> unit
val configureWindow :
    Xtypes.display ->
      Xtypes.window -> Xtypes.configureWindow list -> unit

val createCursor :
    Xtypes.display ->
      Xtypes.pixmap ->
        Xtypes.pixmap ->
          Xtypes.red ->
            Xtypes.green ->
              Xtypes.blue ->
                Xtypes.red ->
                  Xtypes.green ->
                    Xtypes.blue ->
                      Xtypes.coord ->
                        Xtypes.coord ->
                          Xtypes.cursor
val createGlyphCursor :
    Xtypes.display ->
      Xtypes.font ->
        Xtypes.font ->
          int ->
            int ->
              Xtypes.red ->
                Xtypes.green ->
                  Xtypes.blue ->
                    Xtypes.red ->
                      Xtypes.green ->
                        Xtypes.blue ->
                          Xtypes.cursor
val freeCursor :Xtypes.display -> Xtypes.cursor -> unit
val recolorCursor :
    Xtypes.display ->
      Xtypes.cursor ->
        Xtypes.red ->
          Xtypes.green ->
            Xtypes.blue -> Xtypes.red -> Xtypes.green -> Xtypes.blue -> unit
val queryBestSize :
    Xtypes.display ->
      Xtypes.objet ->
        Xtypes.window ->
          Xtypes.size -> Xtypes.size -> Xtypes.queryBestSizeRep
val queryExtension :
    Xtypes.display -> string -> Xtypes.queryExtensionRep
val listExtensions :Xtypes.display -> string list
val openFont :
    Xtypes.display ->
      string -> Xtypes.font
val closeFont :Xtypes.display -> Xtypes.font -> unit
val queryTextExtents :
    Xtypes.display ->
      Xtypes.font ->
        string -> 
          Xtypes.queryTextExtentsRep
val setFontPath :Xtypes.display -> string list -> unit
val getFontPath :Xtypes.display -> string list
val listFonts :
    Xtypes.display -> string -> int -> string list
val listFontsWithInfo :
    Xtypes.display ->
      string -> int -> (string * Xtypes.fontInfo) list
val queryFont :Xtypes.display -> Xtypes.font -> Xtypes.queryFontRep

val setDashes :
    Xtypes.display -> Xtypes.gc -> int -> string -> unit
val createGC :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.setGCattributes list ->
          Xtypes.gc
val changeGC :
    Xtypes.display -> Xtypes.gc -> Xtypes.setGCattributes list -> unit
val setClipRectangles :
    Xtypes.display ->
      Xtypes.gc ->
        Xtypes.clipOrder ->
          Xtypes.coord ->
            Xtypes.coord ->
              (Xtypes.coord * Xtypes.coord * Xtypes.size * Xtypes.size) list ->
                unit
val copyGC :
    Xtypes.display ->
      Xtypes.gc -> Xtypes.gc -> Xtypes.gcMask list -> unit
val freeGC :Xtypes.display -> Xtypes.gc -> unit
val getGeometry :
    Xtypes.display ->
      Xtypes.window -> Xtypes.getGeometryRep
val queryTree :
    Xtypes.display ->
      Xtypes.window -> Xtypes.queryTreeRep

val getWindowAttributes :
    Xtypes.display ->
      Xtypes.window -> Xtypes.getWindowAttributesRep

val ungrabPointer :Xtypes.display -> Xtypes.time -> unit
val grabPointer :
    Xtypes.display ->
      Xtypes.window ->
        bool ->
          Xtypes.eventMask list ->
            Xtypes.grabMode ->
              Xtypes.grabMode ->
                Xtypes.window -> Xtypes.cursor -> Xtypes.time -> unit
val grabButton :
    Xtypes.display ->
      Xtypes.window ->
        bool ->
          Xtypes.eventMask list ->
            Xtypes.grabMode ->
              Xtypes.grabMode ->
                Xtypes.window ->
                  Xtypes.cursor -> Xtypes.button -> Xtypes.modifiers -> unit
val ungrabButton :
    Xtypes.display ->
      Xtypes.window -> Xtypes.button -> Xtypes.modifiers -> unit
val changeActivePointerGrab :
    Xtypes.display ->
      Xtypes.eventMask list -> Xtypes.cursor -> Xtypes.time -> unit
val grabKeyboard :
    Xtypes.display ->
      Xtypes.window ->
        bool -> Xtypes.grabMode -> Xtypes.grabMode -> Xtypes.time -> unit
val ungrabKeyboard :Xtypes.display -> Xtypes.time -> unit
val grabServer :Xtypes.display -> unit
val ungrabServer :Xtypes.display -> unit
val grabKey :
    Xtypes.display ->
      Xtypes.window ->
        bool ->
          Xtypes.grabMode ->
            Xtypes.grabMode -> Xtypes.keycode -> Xtypes.modifiers -> unit
val ungrabKey :
    Xtypes.display ->
      Xtypes.window -> Xtypes.keycode -> Xtypes.modifiers -> unit
val allowEvents :
    Xtypes.display -> Xtypes.eventMode -> Xtypes.time -> unit
val changeHosts :
    Xtypes.display ->
      Xtypes.hostMode -> Xtypes.hostDomaine -> string -> unit
val listHosts :
    Xtypes.display -> Xtypes.listHostsRep
val setAccessControl :
    Xtypes.display -> Xtypes.accessControl -> unit
val putImage :
    Xtypes.display ->
      Xtypes.gc ->
        Xtypes.window ->
          Xtypes.coord ->
            Xtypes.coord ->
              Xtypes.size ->
                Xtypes.size ->
                  int -> int -> Xtypes.imageFormat -> Xtypes.image -> unit
val getImage :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.coord ->
          Xtypes.coord ->
            Xtypes.size ->
              Xtypes.size ->
                int -> Xtypes.imageFormat ->   Xtypes.getImageRep
val setInputFocus :
    Xtypes.display ->
      Xtypes.window -> Xtypes.revertMode -> Xtypes.time -> unit
val getInputFocus :
    Xtypes.display -> Xtypes.getInputFocusRep
val queryKeymap :Xtypes.display -> Xtypes.keycode array
val changeKeyboardMapping :
    Xtypes.display ->
      int -> Xtypes.keycode array array -> unit
val getKeyboardMapping :
    Xtypes.display ->
      int -> int -> Xtypes.getKeyboardMappingRep
val changeKeyboardControl :
    Xtypes.display -> Xtypes.setKBvalues list -> unit

val getKeyboardControl :Xtypes.display -> Xtypes.getKeyboardControlRep
val bell :Xtypes.display -> int -> unit
val changePointerControl :
    Xtypes.display ->
      bool -> int -> int -> bool -> int -> unit
val getPointerControl :Xtypes.display -> Xtypes.getPointerControlRep
val circulateWindow :
    Xtypes.display ->
      Xtypes.window -> Xtypes.direction -> unit
val destroySubwindows :
    Xtypes.display -> Xtypes.window -> unit
val destroyWindow :Xtypes.display -> Xtypes.window -> unit
val unmapSubwindows :
    Xtypes.display -> Xtypes.window -> unit
val unmapWindow :Xtypes.display -> Xtypes.window -> unit
val mapSubwindows :Xtypes.display -> Xtypes.window -> unit
val mapWindow :Xtypes.display -> Xtypes.window -> unit
val setPointerMapping :Xtypes.display -> string -> unit
val getPointerMapping :Xtypes.display -> string
val setModifierMapping :
    Xtypes.display -> int array list -> unit
val getModifierMapping :
    Xtypes.display -> Xtypes.keycode array array
val setCloseDownMode :
    Xtypes.display -> Xtypes.closeDownMode -> unit
val killClient :Xtypes.display -> int -> unit
val createPixmap :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.size ->
          Xtypes.size ->
            int -> Xtypes.pixmap
val freePixmap :Xtypes.display -> Xtypes.pixmap -> unit
val queryPointer :
    Xtypes.display ->
      Xtypes.window -> Xtypes.queryPointerRep
val getMotionEvents :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.time ->
          Xtypes.time -> Xtypes.motionEvent list
val translateCoordinates :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.window ->
          Xtypes.coord ->
            Xtypes.coord -> Xtypes.motionEvent
val warpPointer :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.coord ->
          Xtypes.coord ->
            Xtypes.size ->
              Xtypes.size ->
                Xtypes.window -> Xtypes.coord -> Xtypes.coord -> unit
val polyPoint :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc -> Xtypes.coordMode -> (Xtypes.coord * Xtypes.coord) list -> unit
val polyLine :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc -> Xtypes.coordMode -> (Xtypes.coord * Xtypes.coord) list -> unit
val polySegment :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          (Xtypes.coord * Xtypes.coord * Xtypes.coord * Xtypes.coord) list ->
            unit
val polyRectangle :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          (Xtypes.coord * Xtypes.coord * Xtypes.size * Xtypes.size) list ->
            unit
val polyArc :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          (Xtypes.coord * Xtypes.coord * Xtypes.size * Xtypes.size * 
             int * int)
            list -> unit
val fillPoly :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          Xtypes.shapeMode ->
            Xtypes.coordMode -> (Xtypes.coord * Xtypes.coord) list -> unit
val polyFillRectangle :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          (Xtypes.coord * Xtypes.coord * Xtypes.size * Xtypes.size) list ->
            unit
val polyFillArc :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          (Xtypes.coord * Xtypes.coord * Xtypes.size * Xtypes.size * 
             int * int)
            list -> unit
val listProperties :
    Xtypes.display -> Xtypes.window -> Xtypes.atom list
val internAtom :
    Xtypes.display -> string -> bool -> Xtypes.atom
val getAtomName :Xtypes.display -> Xtypes.atom -> string
val getSelectionOwner :
    Xtypes.display -> Xtypes.atom -> Xtypes.window
val changeProperty :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.propMode ->
          Xtypes.atom -> Xtypes.atom -> int -> string -> unit
val getProperty :
    Xtypes.display ->
      Xtypes.window ->
        bool ->
          Xtypes.atom ->
            Xtypes.atom ->
              int -> int -> Xtypes.getPropertyRep
val rotateProperties :
    Xtypes.display ->
      Xtypes.window -> int -> Xtypes.atom list -> unit
val deleteProperty :
    Xtypes.display -> Xtypes.window -> Xtypes.atom -> unit
val setScreenSaver :
    Xtypes.display ->
      int -> int -> Xtypes.saverMode -> Xtypes.saverMode -> unit
val getScreenSaver :
    Xtypes.display -> Xtypes.getScreenSaverRep

val forceScreenSaver :
    Xtypes.display -> Xtypes.saverState -> unit
val setSelectionOwner :
    Xtypes.display ->
      Xtypes.window -> Xtypes.atom -> Xtypes.time -> unit
val convertSelection :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.atom -> Xtypes.atom -> Xtypes.atom -> Xtypes.time -> unit
val sendEvent :
    Xtypes.display ->
      Xtypes.window ->
        bool -> Xtypes.eventMask list -> Xtypes.event -> unit
val imageText8 :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc -> Xtypes.coord -> Xtypes.coord -> string -> unit
val imageSubText8 :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc -> Xtypes.coord -> Xtypes.coord -> 
          string -> int -> int -> unit
val imageText16 :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc -> Xtypes.coord -> Xtypes.coord -> int array -> unit
val polyText8 :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          Xtypes.coord -> Xtypes.coord -> Xtypes.textItem list -> unit
val polyText16 :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          Xtypes.coord -> Xtypes.coord -> Xtypes.textItem list -> unit
