
let f x y z = 
  match x with
    0 -> 1
  | 1 -> 2
  | _ -> 
      print_int x;
      print_int y;
      print_int z;
      0