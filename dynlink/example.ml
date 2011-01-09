
let t = [| 1.0; 2.0 ; 3.0 |]

let _ = 
  print_float t.(0); print_newline ();
  print_float t.(1); print_newline ();
  print_float t.(2); print_newline ();
  Array.iter (fun x -> print_float x; print_newline ()) t;
  print_float t.(1); print_newline ();
  t.(0) <- t.(1) +. t.(2);
  print_float t.(0); print_newline ()
  
  


class test (str:string) =
  object 
  val mutable s = str
  method get = s
  method set str = s <- str
  method print = print_string s; print_newline ()
end


let t = new test "bonjour"
let _ =
  t#print; print_string "<>"; print_newline ();
  print_string (t#get); print_newline ();
  t#set "a demain";
  t#print

let v = 
  List.fold_left (fun v _ -> v + 1) 0 [1;2;3;4;5;6;7]

type t =
  | A of int
  | B of string 
  | C 
  | D

let affiche_t t =
  begin
    match t with
      A i -> Printf.printf "A %d" i
    | B s -> Printf.printf "B %s" s
    | C -> Printf.printf "C"
    | D -> Printf.printf "D"
  end;
  print_newline ()

let rec affiche msg =
  print_string msg; print_newline ()

and affiche2 msg1 msg2 msg3 =
  affiche (msg1 ^ " " ^ msg2 ^ " " ^ msg3)

let _ =
  print_int v; print_newline ();
  affiche "Config started";
  affiche2 "Config" "started" "and OK";
  affiche_t (A 45555);
  affiche_t (B "bonjour");
  affiche_t C;
  affiche_t D;
  let hash = Hashtbl.create 13 in
  Hashtbl.add hash "toto" "TOTO";
  Hashtbl.add hash "tata" "TATA";
  Hashtbl.add hash "tutu" "TUTU";
  affiche
    (try
      Hashtbl.find hash "toto"
    with
      Not_found -> "tonton")
  
let _ = print_int v; print_newline ()
let rec fib n =
  if n < 2 then 1
  else
    fib (n-1) + fib (n-2)

let _ = print_int v; print_newline ()
let _ = print_int (fib 27); print_newline ()
let _ = print_int v; print_newline ()
