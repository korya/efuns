let _ =
  let limit = 3000 in
  let matrice = Array.init limit (fun i ->
        Array.create limit 0)
  in
  for i = 0 to limit - 1 do
    matrice.(i).(i) <- 1
  done;
  for i = 0 to limit - 1 do
    for j = 0 to limit - 1 do
      matrice.(i).(j) <- matrice.(i).(j) + 1
    done
  done;
  let c = ref 0 in
  for i = 0 to limit - 1 do
    for j = 0 to limit - 1 do
      if matrice.(i).(j) <> 1 then incr c
    done
  done
  