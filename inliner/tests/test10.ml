let rec f a b c d e f g h i =
  let x = a+b+c+d+e+f+g+h+i in
  match x with
    0 -> 1
  | 1 -> 2
  | 2 -> 3
  | 3 -> 4
  | _ -> x