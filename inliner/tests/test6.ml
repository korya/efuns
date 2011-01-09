let f l =
  let x = ref [] in
  let y = ref [] in
  List.iter (fun e ->
      x := e :: !x;
      y := !x :: !y) l;
  !y
  