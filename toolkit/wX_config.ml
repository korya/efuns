open Sys
  
let usage () = 
  prerr_endline "Usage: wx_config [-src] [-str] [-opt/-byte] -c";
  exit 1
  
let str_opt = ref ""
let str_byte = ref ""
let libs_dir = ref (Printf.sprintf "-I %s" Version.installdir)
  
let _ = 
  if Array.length Sys.argv = 1 then usage ();
  Arg.parse [
    "-src", Arg.Unit (fun () ->
        libs_dir := Printf.sprintf "-I %s/toolkit -I %s/xlib -I %s/common -I %s/concur -cclib -L%s/concur" Version.src_dir  Version.src_dir  Version.src_dir  Version.src_dir Version.src_dir
    ), ": To compile without installing (from sources directory)";
    "-str", Arg.Unit (fun () ->
        str_opt := " -cclib -lstr str.cmxa";
        str_byte := " -cclib -lstr str.cma";
    ), ": add flags for Str regexp library";
    "-c", Arg.Unit (fun () -> 
        Printf.printf "%s" !libs_dir; flush stdout
    ), ": print flags for compiling";
    "-opt", Arg.Unit (fun () ->
        Printf.printf "%s -cclib -lunix unix.cmxa -cclib -lconcurnat common.cmxa concur.cmxa xlib.cmxa%s WXlib.cmxa" !libs_dir !str_opt; flush stdout), ": print flags for native linking";
    "-byte", Arg.Unit (fun () ->
        Printf.printf "-custom %s -cclib -lunix unix.cma -cclib -lconcur common.cma concur.cma xlib.cma%s WXlib.cma" !libs_dir !str_byte; flush stdout), ": print flags for bytecode linking";
    ] (fun _ -> usage ()) "WXlib config finder"
    
    