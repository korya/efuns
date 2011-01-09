(* Test for output_value / input_value *)

type t = A | B of int | C of float | D of string | E of char
       | F of t | G of t * t | H of int * t | I of t * float | J

let longstring =
"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
let verylongstring =
"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let test_out filename =
  let oc = open_out_bin filename in
  output_value oc 1;
  output_value oc (-1);
  output_value oc 258;
  output_value oc 20000;
  output_value oc 0x12345678;
  output_value oc 0x123456789ABCDEF0;
  output_value oc "foobargeebuz";
  output_value oc longstring;
  output_value oc verylongstring;
  output_value oc 3.141592654;
  output_value oc ();
  output_value oc A;
  output_value oc (B 1);
  output_value oc (C 2.718);
  output_value oc (D "hello, world!");
  output_value oc (E 'l');
  output_value oc (F(B 1));
  output_value oc (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e')))));
  output_value oc (H(1, A));
  output_value oc (I(B 2, 1e-6));
  let x = D "sharing" in
  let y = G(x, x) in
  let z = G(y, G(x, y)) in
  output_value oc z;
  output_value oc [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|];
  let rec big n = if n <= 0 then A else H(n, big(n-1)) in
  output_value oc (big 1000);
  Marshal.to_channel oc y [Marshal.No_sharing];
  Marshal.to_channel oc fib [Marshal.Closures];
  close_out oc


let test n b =
  prerr_string "Test "; prerr_int n;
  if b then prerr_string " passed.\n" else prerr_string " FAILED.\n";
  flush stderr

let test_in filename =
  let ic = open_in_bin filename in
  test 1 (input_value ic = 1);
  test 2 (input_value ic = (-1));
  test 3 (input_value ic = 258);
  test 4 (input_value ic = 20000);
  test 5 (input_value ic = 0x12345678);
  test 6 (input_value ic = 0x123456789ABCDEF0);
  test 7 (input_value ic = "foobargeebuz");
  test 8 (input_value ic = longstring);
  test 9 (input_value ic = verylongstring);
  test 10 (input_value ic = 3.141592654);
  test 11 (input_value ic = ());
  test 12 (match input_value ic with
    A -> true
  | _ -> false);
  test 13 (match input_value ic with
    (B 1) -> true
  | _ -> false);
  test 14 (match input_value ic with
    (C f) -> f = 2.718
  | _ -> false);
  test 15 (match input_value ic with
    (D "hello, world!") -> true
  | _ -> false);
  test 16 (match input_value ic with
    (E 'l') -> true
  | _ -> false);
  test 17 (match input_value ic with
    (F(B 1)) -> true
  | _ -> false);
  test 18 (match input_value ic with
    (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) -> true
  | _ -> false);
  test 19 (match input_value ic with
    (H(1, A)) -> true
  | _ -> false);
  test 20 (match input_value ic with
    (I(B 2, 1e-6)) -> true
  | _ -> false);
  test 21 (match input_value ic with
    G((G((D "sharing" as t1), t2) as t3), G(t4, t5)) ->
      t1 == t2 & t3 == t5 & t4 == t1
  | _ -> false);
  test 22 (input_value ic = [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|]);
  let rec check_big n t =
    if n <= 0 then
      test 23 (match t with A -> true | _ -> false)
    else
      match t with H(m, s) -> if m = n then check_big (n-1) s
                                       else test 23 false
                 | _ -> test 23 false
  in
    check_big 1000 (input_value ic);
  test 24 (match input_value ic with
    G((D "sharing" as t1), (D "sharing" as t2)) -> t1 != t2
  | _ -> false);
  test 25 (let fib = input_value ic in fib 5 = 8 && fib 10 = 89);
  close_in ic

let test_string () =
  let s = Marshal.to_string 1 [] in
  test 101 (Marshal.from_string s 0 = 1);
  let s = Marshal.to_string (-1) [] in
  test 102 (Marshal.from_string s 0 = (-1));
  let s = Marshal.to_string 258 [] in
  test 103 (Marshal.from_string s 0 = 258);
  let s = Marshal.to_string 20000 [] in
  test 104 (Marshal.from_string s 0 = 20000);
  let s = Marshal.to_string 0x12345678 [] in
  test 105 (Marshal.from_string s 0 = 0x12345678);
  let s = Marshal.to_string 0x123456789ABCDEF0 [] in
  test 106 (Marshal.from_string s 0 = 0x123456789ABCDEF0);
  let s = Marshal.to_string "foobargeebuz" [] in
  test 107 (Marshal.from_string s 0 = "foobargeebuz");
  let s = Marshal.to_string longstring [] in
  test 108 (Marshal.from_string s 0 = longstring);
  let s = Marshal.to_string verylongstring [] in
  test 109 (Marshal.from_string s 0 = verylongstring);
  let s = Marshal.to_string 3.141592654 [] in
  test 110 (Marshal.from_string s 0 = 3.141592654);
  let s = Marshal.to_string () [] in
  test 111 (Marshal.from_string s 0 = ());
  let s = Marshal.to_string A [] in
  test 112 (match Marshal.from_string s 0 with
    A -> true
  | _ -> false);
  let s = Marshal.to_string (B 1) [] in
  test 113 (match Marshal.from_string s 0 with
    (B 1) -> true
  | _ -> false);
  let s = Marshal.to_string (C 2.718) [] in
  test 114 (match Marshal.from_string s 0 with
    (C f) -> f = 2.718
  | _ -> false);
  let s = Marshal.to_string (D "hello, world!") [] in
  test 115 (match Marshal.from_string s 0 with
    (D "hello, world!") -> true
  | _ -> false);
  let s = Marshal.to_string (E 'l') [] in
  test 116 (match Marshal.from_string s 0 with
    (E 'l') -> true
  | _ -> false);
  let s = Marshal.to_string (F(B 1)) [] in
  test 117 (match Marshal.from_string s 0 with
    (F(B 1)) -> true
  | _ -> false);
  let s = Marshal.to_string (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) [] in
  test 118 (match Marshal.from_string s 0 with
    (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) -> true
  | _ -> false);
  let s = Marshal.to_string (H(1, A)) [] in
  test 119 (match Marshal.from_string s 0 with
    (H(1, A)) -> true
  | _ -> false);
  let s = Marshal.to_string (I(B 2, 1e-6)) [] in
  test 120 (match Marshal.from_string s 0 with
    (I(B 2, 1e-6)) -> true
  | _ -> false);
  let x = D "sharing" in
  let y = G(x, x) in
  let z = G(y, G(x, y)) in
  let s = Marshal.to_string z [] in
  test 121 (match Marshal.from_string s 0 with
    G((G((D "sharing" as t1), t2) as t3), G(t4, t5)) ->
      t1 == t2 & t3 == t5 & t4 == t1
  | _ -> false);
  let s = Marshal.to_string [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|] [] in
  test 122 (Marshal.from_string s 0 = [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|]);
  let rec big n = if n <= 0 then A else H(n, big(n-1)) in
  let s = Marshal.to_string (big 1000) [] in
  let rec check_big n t =
    if n <= 0 then
      test 123 (match t with A -> true | _ -> false)
    else
      match t with H(m, s) -> if m = n then check_big (n-1) s
                                       else test 123 false
                 | _ -> test 123 false
  in
    check_big 1000 (Marshal.from_string s 0)

let test_buffer () =
  let s = String.create 512 in
  Marshal.to_buffer s 0 512 1 [];
  test 201 (Marshal.from_string s 0 = 1);
  Marshal.to_buffer s 0 512 (-1) [];
  test 202 (Marshal.from_string s 0 = (-1));
  Marshal.to_buffer s 0 512 258 [];
  test 203 (Marshal.from_string s 0 = 258);
  Marshal.to_buffer s 0 512 20000 [];
  test 204 (Marshal.from_string s 0 = 20000);
  Marshal.to_buffer s 0 512 0x12345678 [];
  test 205 (Marshal.from_string s 0 = 0x12345678);
  Marshal.to_buffer s 0 512 0x123456789ABCDEF0 [];
  test 206 (Marshal.from_string s 0 = 0x123456789ABCDEF0);
  Marshal.to_buffer s 0 512 "foobargeebuz" [];
  test 207 (Marshal.from_string s 0 = "foobargeebuz");
  Marshal.to_buffer s 0 512 longstring [];
  test 208 (Marshal.from_string s 0 = longstring);
  test 209
    (try Marshal.to_buffer s 0 512 verylongstring []; false
     with Failure "Marshal.to_buffer: buffer overflow" -> true);
  Marshal.to_buffer s 0 512 3.141592654 [];
  test 210 (Marshal.from_string s 0 = 3.141592654);
  Marshal.to_buffer s 0 512 () [];
  test 211 (Marshal.from_string s 0 = ());
  Marshal.to_buffer s 0 512 A [];
  test 212 (match Marshal.from_string s 0 with
    A -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (B 1) [];
  test 213 (match Marshal.from_string s 0 with
    (B 1) -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (C 2.718) [];
  test 214 (match Marshal.from_string s 0 with
    (C f) -> f = 2.718
  | _ -> false);
  Marshal.to_buffer s 0 512 (D "hello, world!") [];
  test 215 (match Marshal.from_string s 0 with
    (D "hello, world!") -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (E 'l') [];
  test 216 (match Marshal.from_string s 0 with
    (E 'l') -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (F(B 1)) [];
  test 217 (match Marshal.from_string s 0 with
    (F(B 1)) -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) [];
  test 218 (match Marshal.from_string s 0 with
    (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (H(1, A)) [];
  test 219 (match Marshal.from_string s 0 with
    (H(1, A)) -> true
  | _ -> false);
  Marshal.to_buffer s 0 512 (I(B 2, 1e-6)) [];
  test 220 (match Marshal.from_string s 0 with
    (I(B 2, 1e-6)) -> true
  | _ -> false);
  let x = D "sharing" in
  let y = G(x, x) in
  let z = G(y, G(x, y)) in
  Marshal.to_buffer s 0 512 z [];
  test 221 (match Marshal.from_string s 0 with
    G((G((D "sharing" as t1), t2) as t3), G(t4, t5)) ->
      t1 == t2 & t3 == t5 & t4 == t1
  | _ -> false);
  Marshal.to_buffer s 0 512 [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|] [];
  test 222 (Marshal.from_string s 0 = [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16|]);
  let rec big n = if n <= 0 then A else H(n, big(n-1)) in
  test 223
    (try Marshal.to_buffer s 0 512 (big 1000) []; false
     with Failure "Marshal.to_buffer: buffer overflow" -> true)

let test_size() =
  let s = Marshal.to_string (G(A, G(B 2, G(C 3.14, G(D "glop", E 'e'))))) [] in
  test 300 (Marshal.header_size + Marshal.data_size s 0 = String.length s)

(* Test for really big objects *)

let counter = ref 0

let rec make_big n =
  if n <= 0 then begin
    incr counter; B !counter
  end else begin
    let l = make_big (n-1) in
    let r = make_big (n-1) in
    G(l, r)
  end

let rec check_big n x =
  if n <= 0 then begin
    match x with
      B k -> incr counter; k = !counter
    | _   -> false
  end else begin
    match x with
      G(l, r) -> check_big (n-1) l && check_big (n-1) r
    | _       -> false
  end

let main() =
  if Array.length Sys.argv <= 2 then begin
    test_out "intext.data"; test_in "intext.data";
    test_out "intext.data"; test_in "intext.data";
    Sys.remove "intext.data";
    test_string();
    test_buffer();
    test_size()
  end else
  if Sys.argv.(1) = "make" then begin
    let n = int_of_string Sys.argv.(2) in
    let oc = open_out_bin "intext.data" in
    counter := 0;
    output_value oc (make_big n);
    close_out oc
  end else
  if Sys.argv.(1) = "test" then begin
    let n = int_of_string Sys.argv.(2) in
    let ic = open_in_bin "intext.data" in
    let b = (input_value ic : t) in
    Gc.full_major();
    close_in ic;
    counter := 0;
    if check_big n b then
      Printf.printf "Test big %d passed" n
    else
      Printf.printf "Test big %d FAILED" n;
    print_newline()
  end

let _ = Printexc.catch main (); exit 0
