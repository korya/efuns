(* ici, outre le checkBound, on peut retirer les modifications sur Esp. *)

let clear a x =
  for i = 0 to Array.length a do
    a.(i) <- None;
  done