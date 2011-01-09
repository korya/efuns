type t = {
    a: int option;
    b : int option;
    c: int option;
    d : int;
    e : int;
    f : int;
  }
  
let copy r x =
  
  { r with b = r.a } , x
  
let e = { a = None; b = None; c = None; d = Random.int 100; e = 0; f = 0 }