let iter_long n =
  let rec iter () =
    iter ()
  in
  iter ()

let rec list_iter f list =
  match list with [] -> () | r :: tail -> f r; list_iter f tail
      