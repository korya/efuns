
let filename = Sys.argv.(2)
let version = Sys.argv.(1)
let ic = open_in filename
let oc = open_out (String.sub filename 0 (String.length filename - 1))
  
let version_str = "VERSION["
let version_len = String.length version_str

let get_version s =
  let len = String.length s in
  try
    let plus_pos = String.index s '+' in
    (float_of_string (String.sub s 0 plus_pos)), 
    (int_of_string  (String.sub s (plus_pos+1) (len - plus_pos - 1)))
  with _ ->
      (float_of_string s), 0

let version = get_version version
  
let _ =
  try
    while true do
      let line = input_line ic in
      let len = String.length line in
      if len > version_len && String.sub line 0 version_len = version_str then
        let minus_pos = String.index line '-' in
        let bracket_pos = String.index line ']' in
        let begin_ver = String.sub line version_len (minus_pos - version_len) in
        let end_ver = String.sub line (minus_pos+1) (bracket_pos - minus_pos - 1) in
        Printf.printf "[%s] [%s]" begin_ver end_ver; print_newline ();
        let begin_ver = get_version begin_ver in
        let end_ver = get_version end_ver in
        Printf.printf "[%f+%d] [%f+%d]" (fst begin_ver) (snd begin_ver) (fst end_ver) (snd end_ver); print_newline ();
        let print_line = 
          (begin_ver <= version) && (end_ver = (0.0,0) || end_ver >= version)
        in
        if print_line then begin
            output_string oc (String.sub line (bracket_pos+1) (len - bracket_pos - 1));
            output_char oc '\n'
          end
        else ()
            
      else begin
          output_string oc line;
          output_char oc '\n'
        end
    done
  with _ -> 
      close_in ic; close_out oc