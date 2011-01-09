open Msetint
  
let set = empty ()
  
let _ = assert (Array.length Sys.argv = 2)
let limit = int_of_string Sys.argv.(1) * 1000
let _ =  
  for i = 0 to limit do
     add i set
  done;
  let count = ref 0 in
  for i = 0 to limit do
    if mem i set then incr count
  done;
  Printf.printf "Ele = %d" !count;
  print_newline ();
  for i = 0 to limit do
    if i mod 2 = 0 then remove i set
  done;
  for i = 0 to limit/5 do
    remove i set
  done;
  for i = 4*(limit/5) to limit do
    if i mod 2 = 0 then remove i set
  done;
  let v = ref false in
  for i = 0 to limit do
    let a = max_elt set in
    let b = min_elt set in
    v := a < b
  done;
  let count = ref 0 in
  for i = 0 to limit do
    if mem i set then incr count
  done;
  Printf.printf "Ele = %d" !count;
  print_newline ();
  