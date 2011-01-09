(*
type t =
  A | B | C | D | E
  
let f x =
  match x with
    A -> 0
  | B -> 1
  | C -> 2
  | D -> 3
  | E -> 4
      
let g x = (f A)+1
*)  
type tt =
  A | B | C | D | E | F | G | H | I | J
| ZZ of int
  
let h x =
  match x with
    A -> 0
  | B -> 1
  | C -> 2
  | D | E | F | G -> 3
  | H -> 7
  | I -> 8
  | J -> 9
  | ZZ i -> i