let switch_knowm x =
  let y = None in
  match y with
    None -> x + 2 
  | Some z -> x + z

let orororor x1 x2 x3 x4 =
  x1 lor (x2 lsl 8) lor (x3 lsl 16) lor (x4 lsl 24)

let deadcode x y z = 
  let zz = y+z in
  let w = y*2 + zz in
  (x+1,y+2,z+3)

let loop () = 
  while true do () done

let set buf pos c =
  buf.[pos*2] <- c

let simple x y z t =
  x+y*z+t
  
let common x y z =
  while true do
    let e = x+y+z in
    print_int e
  done
  
open String
let setInt buffer pos int =
  unsafe_set buffer  pos     (Char.unsafe_chr (int land 0xff));  
  unsafe_set buffer (pos+1)  (Char.unsafe_chr ((int lsr 8) land 0xff));
  unsafe_set buffer (pos+2)  (Char.unsafe_chr ((int lsr 16) land 0xff));
  unsafe_set buffer (pos+3)  (Char.unsafe_chr ((int lsr 24) land 0xff))
  
type a =
  T of int
| U of int
| V of int
| W of int
| X of int
| Y of int
| Z of int
  
let pattern x =
  match x with
    T x -> x+2
  | U y -> y+2
  | V z -> z+2
  | Y _ | Z _ -> 4
  | _ -> 3