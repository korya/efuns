(* Test a file copy function *)

let test msg funct f1 f2 =
  print_string msg; print_newline();
  funct f1 f2;
  if Sys.command ("cmp " ^ f1 ^ " " ^ f2) = 0
  then print_string "passed"
  else print_string "FAILED";
  print_newline()

(* File copy with constant-sized chunks *)

let copy_file sz infile ofile =
  let ic = open_in infile in
  let oc = open_out ofile in
  let buffer = String.create sz in
  let rec copy () =
    let n = input ic buffer 0 sz in
    if n = 0 then () else begin
      output oc buffer 0 n;
      copy ()
    end in
  copy();
  close_in ic;
  close_out oc

(* File copy with random-sized chunks *)

let copy_random sz infile ofile =
  let ic = open_in infile in
  let oc = open_out ofile in
  let buffer = String.create sz in
  let rec copy () =
    let s = 1 + Random.int sz in
    let n = input ic buffer 0 s in
    if n = 0 then () else begin
      output oc buffer 0 n;
      copy ()
    end in
  copy();
  close_in ic;
  close_out oc

(* File copy line per line *)

let copy_line infile ofile =
  let ic = open_in infile in
  let oc = open_out ofile in
  try
    while true do
      output_string oc (input_line ic); output_char oc '\n'
    done
  with End_of_file ->
    close_in ic;
    close_out oc

(* Backward copy, with lots of seeks *)

let copy_seek chunksize infile ofile =
  let ic = open_in_bin infile in
  let oc = open_out_bin ofile in
  let size = in_channel_length ic in
  let buffer = String.create chunksize in
  for i = (size - 1) / chunksize downto 0 do
    seek_in ic (i * chunksize);
    seek_out oc (i * chunksize);
    let n = input ic buffer 0 chunksize in
    output oc buffer 0 n
  done;
  close_in ic;
  close_out oc

(* Create long lines of text *)

let make_lines ofile =
  let oc = open_out ofile in
  for i = 1 to 256 do
    output_string oc (String.make (i*64) '.'); output_char oc '\n'
  done;
  close_out oc

(* The test *)

let _ =
  let src = Sys.argv.(1) in
  test "16-byte chunks" (copy_file 16) src "/tmp/testio";
  test "256-byte chunks" (copy_file 256) src "/tmp/testio";
  test "4096-byte chunks" (copy_file 4096) src "/tmp/testio";
  test "65536-byte chunks" (copy_file 65536) src "/tmp/testio";
  test "19-byte chunks" (copy_file 19) src "/tmp/testio";
  test "263-byte chunks" (copy_file 263) src "/tmp/testio";
  test "4011-byte chunks" (copy_file 4011) src "/tmp/testio";
  test "0...8192 byte chunks" (copy_random 8192) src "/tmp/testio";
  test "line per line, short lines" copy_line "/etc/hosts" "/tmp/testio";
  make_lines "/tmp/lines";
  test "line per line, short and long lines" copy_line "/tmp/lines" "/tmp/testio";
  test "backwards, 4096-byte chunks" (copy_seek 4096) src "/tmp/testio"; 
  test "backwards, 64-byte chunks" (copy_seek 64) src "/tmp/testio"; 
  Sys.remove "/tmp/lines";
  Sys.remove "/tmp/testio";
  exit 0
