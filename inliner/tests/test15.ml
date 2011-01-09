let e = Exit

let list0 = List.map (fun u -> ()) []
let list1 = List.map (fun u -> u,u) [1]
let list2 = List.map (fun u -> u,u) [1;2]
  
let total = ref 0
  
let f list =
  Printf.printf "Len: %d" (List.length list); print_newline ();
  if List.length list = 0 then (incr total; Printf.printf "len = 0"; print_newline ()) 
  else (Printf.printf "not len = 0"; print_newline ());
  if List.length list = 1 then (incr total; Printf.printf "len = 1"; print_newline ()) 
  else (Printf.printf "not len = 1"; print_newline ());
  if List.length list = 2 then (incr total; Printf.printf "len = 2"; print_newline ()) 
  else (Printf.printf "not len = 2"; print_newline ());
  if List.length list <> 0 then (incr total; Printf.printf "len <> 0"; print_newline ()) 
  else (Printf.printf "not len <> 0"; print_newline ());
  if List.length list <> 1 then (incr total; Printf.printf "len <> 1"; print_newline ()) 
  else (Printf.printf "not len <> 1"; print_newline ());
  if List.length list <> 2 then (incr total; Printf.printf "len <> 2"; print_newline ()) 
  else (Printf.printf "not len <> 2"; print_newline ());
  if List.length list >= 0 then (incr total; Printf.printf "len >= 0"; print_newline ()) 
  else (Printf.printf "not len >= 0"; print_newline ());
  if List.length list >= 1 then (incr total; Printf.printf "len >= 1"; print_newline ()) 
  else (Printf.printf "not len >= 1"; print_newline ());
  if List.length list >= 2 then (incr total; Printf.printf "len >= 2"; print_newline ()) 
  else (Printf.printf "not len >= 2"; print_newline ());
  if List.length list <= 0 then (incr total; Printf.printf "len <= 0"; print_newline ()) 
  else (Printf.printf "not len <= 0"; print_newline ());
  if List.length list <= 1 then (incr total; Printf.printf "len <= 1"; print_newline ()) 
  else (Printf.printf "not len <= 1"; print_newline ());
  if List.length list <= 2 then (incr total; Printf.printf "len <= 2"; print_newline ()) 
  else (Printf.printf "not len <= 2"; print_newline ());
  if List.length list > 0 then (incr total; Printf.printf "len > 0"; print_newline ()) 
  else (Printf.printf "not len > 0"; print_newline ());
  if List.length list > 1 then (incr total; Printf.printf "len > 1"; print_newline ()) 
  else (Printf.printf "not len > 1"; print_newline ());
  if List.length list > 2 then (incr total; Printf.printf "len > 2"; print_newline ()) 
  else (Printf.printf "not len > 2"; print_newline ());
  if List.length list < 0 then (incr total; Printf.printf "len < 0"; print_newline ()) 
  else (Printf.printf "not len < 0"; print_newline ());
  if List.length list < 1 then (incr total; Printf.printf "len < 1"; print_newline ()) 
  else (Printf.printf "not len < 1"; print_newline ());
  if List.length list < 2 then (incr total; Printf.printf "len < 2"; print_newline ()) 
    else (Printf.printf "not len < 2"; print_newline ())
    
let _ =
  Printf.printf "LIST NULL"; print_newline ();
  f list0; print_newline ();
  Printf.printf "LIST ONE"; print_newline ();
  f list1; print_newline ();
  Printf.printf "LIST TWO"; print_newline ();
  f list2; print_newline ();
  Printf.printf "TOTAL %d" !total;
  print_newline ();
  