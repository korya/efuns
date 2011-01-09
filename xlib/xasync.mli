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
              Jeton.t 
val clearWindow : Xtypes.display -> Xtypes.window -> unit Jeton.t
val copyArea :
    Xtypes.display ->
      Xtypes.gc ->
        Xtypes.window ->
          Xtypes.coord ->
            Xtypes.coord ->
              Xtypes.window ->
                Xtypes.coord ->
                  Xtypes.coord -> Xtypes.size -> Xtypes.size -> unit
                      Jeton.t 
val copyPlane :
    Xtypes.display ->
      Xtypes.gc ->
        Xtypes.window ->
          Xtypes.coord ->
            Xtypes.coord ->
              Xtypes.window ->
                Xtypes.coord ->
                  Xtypes.coord -> Xtypes.size -> Xtypes.size -> int -> unit
                      Jeton.t 
val changeSaveSet :
    Xtypes.display ->
      Xtypes.window -> Xtypes.saveSetOp -> unit
          Jeton.t 
val reparentWindow :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.window -> Xtypes.coord -> Xtypes.coord -> unit
            Jeton.t 
val allocColor :
    Xtypes.display ->
      Xtypes.colormap ->
        Xtypes.red ->
          Xtypes.green ->
            Xtypes.blue -> Xtypes.allocColorRep

                Jeton.t 
val allocNamedColor :
    Xtypes.display ->
      Xtypes.colormap ->
        string -> Xtypes.allocNamedColorRep

            Jeton.t 
val allocColorCells :
    Xtypes.display ->
      Xtypes.colormap ->
        int -> int -> bool -> Xtypes.allocColorCellsRep
            Jeton.t 
val allocColorPlanes :
    Xtypes.display ->
      Xtypes.colormap ->
        int ->
          Xtypes.red ->
            Xtypes.green ->
              Xtypes.blue ->
                bool -> Xtypes.allocColorPlanesRep

                    Jeton.t 
val freeColors :
    Xtypes.display ->
      Xtypes.colormap -> int -> Xtypes.pixel list -> unit
          Jeton.t 
val lookupColor :
    Xtypes.display ->
      Xtypes.colormap ->
        string -> Xtypes.lookupColorRep

            Jeton.t 
val storeColors :
    Xtypes.display ->
      Xtypes.colormap ->
        (Xtypes.pixel * Xtypes.red * Xtypes.green * Xtypes.blue *
           Xtypes.colorMask list)
          list -> unit
              Jeton.t 
val storeNamedColor :
    Xtypes.display ->
      Xtypes.colormap ->
        Xtypes.pixel -> Xtypes.colorMask list -> string -> unit
            Jeton.t 
val queryColors :
    Xtypes.display ->
      Xtypes.colormap ->
        Xtypes.pixel list -> 
          Xtypes.color  array
            Jeton.t 
val createColormap :
    Xtypes.display -> 
      Xtypes.colormap ->
        Xtypes.window ->
          Xtypes.visual ->
            Xtypes.allocCells ->
              unit             Jeton.t 
val freeColormap :
    Xtypes.display -> Xtypes.colormap -> unit
        Jeton.t 
val copyColormapAndFree :
    Xtypes.display -> Xtypes.colormap ->
      Xtypes.colormap ->
        unit          Jeton.t 
val installColormap :
    Xtypes.display -> Xtypes.colormap -> unit
        Jeton.t 
val uninstallColormap :
    Xtypes.display -> Xtypes.colormap -> unit
        Jeton.t 
val listInstalledColormaps :
    Xtypes.display -> Xtypes.window -> Xtypes.colormap list
        Jeton.t 
val createWindow :
    Xtypes.display ->
      Xtypes.window ->
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
                          unit
                            Jeton.t 
val changeWindowAttributes :
    Xtypes.display ->
      Xtypes.window -> Xtypes.setWindowAttributes list -> unit
          Jeton.t 
val configureWindow :
    Xtypes.display ->
      Xtypes.window -> Xtypes.configureWindow list -> unit

          Jeton.t 
val createCursor :
    Xtypes.display ->
      Xtypes.cursor ->
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
                          unit
                            Jeton.t 
val createGlyphCursor :
    Xtypes.display ->
      Xtypes.cursor ->
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
                          unit
                            Jeton.t 
val freeCursor :Xtypes.display -> Xtypes.cursor -> unit
    Jeton.t 
val recolorCursor :
    Xtypes.display ->
      Xtypes.cursor ->
        Xtypes.red ->
          Xtypes.green ->
            Xtypes.blue -> Xtypes.red -> Xtypes.green -> Xtypes.blue -> unit
                Jeton.t 
val queryBestSize :
    Xtypes.display ->
      Xtypes.objet ->
        Xtypes.window ->
          Xtypes.size -> Xtypes.size -> Xtypes.queryBestSizeRep
              Jeton.t 
val queryExtension :
    Xtypes.display -> string -> Xtypes.queryExtensionRep
        Jeton.t 
val listExtensions :Xtypes.display -> string list
    Jeton.t 
val openFont :
    Xtypes.display -> Xtypes.font ->
      string -> unit
          Jeton.t 
val closeFont :Xtypes.display -> Xtypes.font -> unit
    Jeton.t 
val queryTextExtents :
    Xtypes.display ->
      Xtypes.font ->
        string -> 
          Xtypes.queryTextExtentsRep
            Jeton.t 
val setFontPath :Xtypes.display -> string list -> unit
    Jeton.t 
val getFontPath :Xtypes.display -> string list
    Jeton.t 
val listFonts :
    Xtypes.display -> string -> int -> string list
        Jeton.t 
val listFontsWithInfo :
    Xtypes.display ->
      string -> int -> (string * Xtypes.fontInfo) list
          Jeton.t 
val queryFont :Xtypes.display -> Xtypes.font -> Xtypes.queryFontRep

    Jeton.t 
val setDashes :
    Xtypes.display -> Xtypes.gc -> int -> string -> unit
        Jeton.t 
val createGC :
    Xtypes.display ->
      Xtypes.gc ->
      Xtypes.window ->
        Xtypes.setGCattributes list ->
          unit
            Jeton.t 
val changeGC :
    Xtypes.display -> Xtypes.gc -> Xtypes.setGCattributes list -> unit
        Jeton.t 
val setClipRectangles :
    Xtypes.display ->
      Xtypes.gc ->
        Xtypes.clipOrder ->
          Xtypes.coord ->
            Xtypes.coord ->
              (Xtypes.coord * Xtypes.coord * Xtypes.size * Xtypes.size) list ->
                unit
                  Jeton.t 
val copyGC :
    Xtypes.display ->
      Xtypes.gc -> Xtypes.gc -> Xtypes.gcMask list -> unit
          Jeton.t 
val freeGC :Xtypes.display -> Xtypes.gc -> unit
    Jeton.t 
val getGeometry :
    Xtypes.display ->
      Xtypes.window -> Xtypes.getGeometryRep
          Jeton.t 
val queryTree :
    Xtypes.display ->
      Xtypes.window -> Xtypes.queryTreeRep

          Jeton.t 
val getWindowAttributes :
    Xtypes.display ->
      Xtypes.window -> Xtypes.getWindowAttributesRep

          Jeton.t 
val ungrabPointer :Xtypes.display -> Xtypes.time -> unit
    Jeton.t 
val grabPointer :
    Xtypes.display ->
      Xtypes.window ->
        bool ->
          Xtypes.eventMask list ->
            Xtypes.grabMode ->
              Xtypes.grabMode ->
                Xtypes.window -> Xtypes.cursor -> Xtypes.time -> unit
                    Jeton.t 
val grabButton :
    Xtypes.display ->
      Xtypes.window ->
        bool ->
          Xtypes.eventMask list ->
            Xtypes.grabMode ->
              Xtypes.grabMode ->
                Xtypes.window ->
                  Xtypes.cursor -> Xtypes.button -> Xtypes.modifiers -> unit
                      Jeton.t 
val ungrabButton :
    Xtypes.display ->
      Xtypes.window -> Xtypes.button -> Xtypes.modifiers -> unit
          Jeton.t 
val changeActivePointerGrab :
    Xtypes.display ->
      Xtypes.eventMask list -> Xtypes.cursor -> Xtypes.time -> unit
          Jeton.t 
val grabKeyboard :
    Xtypes.display ->
      Xtypes.window ->
        bool -> Xtypes.grabMode -> Xtypes.grabMode -> Xtypes.time -> unit
            Jeton.t 
val ungrabKeyboard :Xtypes.display -> Xtypes.time -> unit
    Jeton.t 
val grabServer :Xtypes.display -> unit
    Jeton.t 
val ungrabServer :Xtypes.display -> unit
    Jeton.t 
val grabKey :
    Xtypes.display ->
      Xtypes.window ->
        bool ->
          Xtypes.grabMode ->
            Xtypes.grabMode -> Xtypes.keycode -> Xtypes.modifiers -> unit
                Jeton.t 
val ungrabKey :
    Xtypes.display ->
      Xtypes.window -> Xtypes.keycode -> Xtypes.modifiers -> unit
          Jeton.t 
val allowEvents :
    Xtypes.display -> Xtypes.eventMode -> Xtypes.time -> unit
        Jeton.t 
val changeHosts :
    Xtypes.display ->
      Xtypes.hostMode -> Xtypes.hostDomaine -> string -> unit
          Jeton.t 
val listHosts :
    Xtypes.display -> Xtypes.listHostsRep
        Jeton.t 
val setAccessControl :
    Xtypes.display -> Xtypes.accessControl -> unit
        Jeton.t 
val putImage :
    Xtypes.display ->
      Xtypes.gc ->
        Xtypes.window ->
          Xtypes.coord ->
            Xtypes.coord ->
              Xtypes.size ->
                Xtypes.size ->
                  int -> int -> Xtypes.imageFormat -> Xtypes.image -> unit
                      Jeton.t 
val getImage :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.coord ->
          Xtypes.coord ->
            Xtypes.size ->
              Xtypes.size ->
                int -> Xtypes.imageFormat ->   Xtypes.getImageRep
                    Jeton.t 
val setInputFocus :
    Xtypes.display ->
      Xtypes.window -> Xtypes.revertMode -> Xtypes.time -> unit
          Jeton.t 
val getInputFocus :
    Xtypes.display -> Xtypes.getInputFocusRep
        Jeton.t 
val queryKeymap :Xtypes.display -> Xtypes.keycode array
    Jeton.t 
val changeKeyboardMapping :
    Xtypes.display ->
      int -> Xtypes.keycode array array -> unit
          Jeton.t 
val getKeyboardMapping :
    Xtypes.display ->
      int -> int -> Xtypes.getKeyboardMappingRep
          Jeton.t 
val changeKeyboardControl :
    Xtypes.display -> Xtypes.setKBvalues list -> unit

        Jeton.t 
val getKeyboardControl :Xtypes.display -> Xtypes.getKeyboardControlRep
    Jeton.t 
val bell :Xtypes.display -> int -> unit
    Jeton.t 
val changePointerControl :
    Xtypes.display ->
      bool -> int -> int -> bool -> int -> unit
          Jeton.t 
val getPointerControl :Xtypes.display -> Xtypes.getPointerControlRep
    Jeton.t 
val circulateWindow :
    Xtypes.display ->
      Xtypes.window -> Xtypes.direction -> unit
          Jeton.t 
val destroySubwindows :
    Xtypes.display -> Xtypes.window -> unit
        Jeton.t 
val destroyWindow :Xtypes.display -> Xtypes.window -> unit
    Jeton.t 
val unmapSubwindows :
    Xtypes.display -> Xtypes.window -> unit
        Jeton.t 
val unmapWindow :Xtypes.display -> Xtypes.window -> unit
    Jeton.t 
val mapSubwindows :Xtypes.display -> Xtypes.window -> unit
    Jeton.t 
val mapWindow :Xtypes.display -> Xtypes.window -> unit
    Jeton.t 
val setPointerMapping :Xtypes.display -> string -> unit
    Jeton.t 
val getPointerMapping :Xtypes.display -> string
    Jeton.t 
val setModifierMapping :
    Xtypes.display -> int array list -> unit
        Jeton.t 
val getModifierMapping :
    Xtypes.display -> Xtypes.keycode array array
        Jeton.t 
val setCloseDownMode :
    Xtypes.display -> Xtypes.closeDownMode -> unit
        Jeton.t 
val killClient :Xtypes.display -> int -> unit
    Jeton.t 
val createPixmap :
    Xtypes.display ->
      Xtypes.pixmap ->
      Xtypes.window ->
        Xtypes.size ->
          Xtypes.size ->
            int -> unit
                Jeton.t 
val freePixmap :Xtypes.display -> Xtypes.pixmap -> unit
    Jeton.t 
val queryPointer :
    Xtypes.display ->
      Xtypes.window -> Xtypes.queryPointerRep
          Jeton.t 
val getMotionEvents :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.time ->
          Xtypes.time -> Xtypes.motionEvent list
              Jeton.t 
val translateCoordinates :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.window ->
          Xtypes.coord ->
            Xtypes.coord -> Xtypes.motionEvent
                Jeton.t 
val warpPointer :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.coord ->
          Xtypes.coord ->
            Xtypes.size ->
              Xtypes.size ->
                Xtypes.window -> Xtypes.coord -> Xtypes.coord -> unit
                    Jeton.t 
val polyPoint :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc -> Xtypes.coordMode -> (Xtypes.coord * Xtypes.coord) list -> unit
            Jeton.t 
val polyLine :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc -> Xtypes.coordMode -> (Xtypes.coord * Xtypes.coord) list -> unit
            Jeton.t 
val polySegment :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          (Xtypes.coord * Xtypes.coord * Xtypes.coord * Xtypes.coord) list ->
            unit
              Jeton.t 
val polyRectangle :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          (Xtypes.coord * Xtypes.coord * Xtypes.size * Xtypes.size) list ->
            unit
              Jeton.t 
val polyArc :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          (Xtypes.coord * Xtypes.coord * Xtypes.size * Xtypes.size * 
             int * int)
            list -> unit
                Jeton.t 
val fillPoly :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          Xtypes.shapeMode ->
            Xtypes.coordMode -> (Xtypes.coord * Xtypes.coord) list -> unit
                Jeton.t 
val polyFillRectangle :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          (Xtypes.coord * Xtypes.coord * Xtypes.size * Xtypes.size) list ->
            unit
              Jeton.t 
val polyFillArc :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          (Xtypes.coord * Xtypes.coord * Xtypes.size * Xtypes.size * 
             int * int)
            list -> unit
                Jeton.t 
val listProperties :
    Xtypes.display -> Xtypes.window -> Xtypes.atom list
        Jeton.t 
val internAtom :
    Xtypes.display -> string -> bool -> Xtypes.atom
        Jeton.t 
val getAtomName :Xtypes.display -> Xtypes.atom -> string
    Jeton.t 
val getSelectionOwner :
    Xtypes.display -> Xtypes.atom -> Xtypes.window
        Jeton.t 
val changeProperty :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.propMode ->
          Xtypes.atom -> Xtypes.atom -> int -> string -> unit
              Jeton.t 
val getProperty :
    Xtypes.display ->
      Xtypes.window ->
        bool ->
          Xtypes.atom ->
            Xtypes.atom ->
              int -> int -> Xtypes.getPropertyRep
                  Jeton.t 
val rotateProperties :
    Xtypes.display ->
      Xtypes.window -> int -> Xtypes.atom list -> unit
          Jeton.t 
val deleteProperty :
    Xtypes.display -> Xtypes.window -> Xtypes.atom -> unit
        Jeton.t 
val setScreenSaver :
    Xtypes.display ->
      int -> int -> Xtypes.saverMode -> Xtypes.saverMode -> unit
          Jeton.t 
val getScreenSaver :
    Xtypes.display -> Xtypes.getScreenSaverRep

        Jeton.t 
val forceScreenSaver :
    Xtypes.display -> Xtypes.saverState -> unit
        Jeton.t 
val setSelectionOwner :
    Xtypes.display ->
      Xtypes.window -> Xtypes.atom -> Xtypes.time -> unit
          Jeton.t 
val convertSelection :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.atom -> Xtypes.atom -> Xtypes.atom -> Xtypes.time -> unit
            Jeton.t 
val sendEvent :
    Xtypes.display ->
      Xtypes.window ->
        bool -> Xtypes.eventMask list -> Xtypes.event -> unit
            Jeton.t 
val imageText8 :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc -> Xtypes.coord -> Xtypes.coord -> string -> unit
            Jeton.t 
val imageSubText8 :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc -> Xtypes.coord -> Xtypes.coord -> 
          string -> int -> int -> unit Jeton.t
val imageText16 :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc -> Xtypes.coord -> Xtypes.coord -> int array -> unit
            Jeton.t 
val polyText8 :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          Xtypes.coord -> Xtypes.coord -> Xtypes.textItem list -> unit
              Jeton.t 
val polyText16 :
    Xtypes.display ->
      Xtypes.window ->
        Xtypes.gc ->
          Xtypes.coord -> Xtypes.coord -> Xtypes.textItem list -> unit Jeton.t
