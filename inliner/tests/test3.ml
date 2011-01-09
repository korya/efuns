let rec fon a b c d e f g h i j = 
  if a > 0 then
    if b > 0 then 
      if c > 0 then
        a + b + c + d + e + f + g + h + i + j
      else 0 else 0 else 
    fon 0 b c d e f g h i j
  
let x = ref 3
  
  
let g () = 
  x := fon (!x) 1 2 3 4 5 6 7 8 9 ;
  !x, !x, !x
  