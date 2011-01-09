let _ =
  let x = 
    if Random.int 100 > -10 then 3
    else 3
  in
  while x > 2 do
    Printf.printf "bonjour"; print_newline ();
  done