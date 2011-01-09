open Xpm
  
let _ = 
  Arg.parse [] (fun file ->
      let data = readPixmapDataFromFile file in
      let mod_name = Filename.chop_extension (Filename.basename file) in
      let s = createMLStringFromPixmapData data "pixmap" in
      let oc = open_out (mod_name ^ ".ml") in
      output_string oc (Printf.sprintf "(* XPM *)\n (* File generated from %s *)\n"
          file);
      output_string oc s;
      close_out oc
  ) "Generate CAML modules from pixmap files\nUsage: pix2ml <list of .xpm files>"