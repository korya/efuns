

let all_null list =
  try
    List.iter (fun x -> if x = 0 then raise Exit) list;
    true
  with Exit -> false