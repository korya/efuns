(* Test bound checks with ocamlopt *)

let a = [| 0; 1; 2 |]

let trail = ref []

let test n =
  let result =
    try
      trail := n :: !trail; a.(n); "doesn't fail"
    with Invalid_argument s ->
           (* Check well-formedness of s *)
           if String.length s = 35
           && s = "out-of-bound array or string access"
           then "fails"
           else "bad Invalid_argument"
       | _ -> "bad exception"
  in
    print_int n; print_string ": "; print_string result; print_newline()

let _ =
  test 0; test 1; test 2; test 3; test 4; test (-1);
  Gc.full_major();
  print_string "Trail:";
  List.iter (fun n -> print_string " "; print_int n) !trail;
  print_newline()


