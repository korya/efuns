open Unix

external my_select : Unix.file_descr list -> Unix.file_descr list = "my_select"
external my_select1 : Unix.file_descr -> bool = "my_select1"
(*
  external ioctl: file_descr -> int -> string -> 'a -> unit = "ml2c_ioctl"
*)
        
let forks = ref []
let readers = ref []
let timers = ref []
let alarm = ref false
let _ = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> alarm := true))
let actions = ref 0
let rec iter_pid list =
  match list with
    [] -> []
  | pid :: tail ->
      let tail = iter_pid tail in
      try
        let _ = Unix.waitpid [WNOHANG] pid in
        tail
      with _ -> pid :: tail

let rec iterator lst_it = 
  forks := iter_pid !forks;
  if !alarm then
    begin
      alarm := false;
      let hooks =
        match !timers with
          [] -> []
        | [time,f] -> timers := []; [f]
        | (time,f) :: tail ->
            let rec iter list =
              match list with
                [] -> timers := []; []
              | (t,f) :: tail when t <= 0.000001 ->
                  f :: (iter tail)
              | (t1,f1) :: tail -> 
                  timers := list;
                  let _ = 
                    setitimer ITIMER_REAL { it_interval=0.0; it_value = t1 }
                  in
                  []
            in
            f :: (iter tail)
      in
      List.iter (Log.catch "Concur.alarms: %s\n") hooks
    end;
  if !actions <> !lst_it then 
    lst_it := !actions 
  else
  try
    match !readers with
      [r,f] -> 
        if my_select1 r then
          (incr actions; Log.catch "Concur.iterator: %s\n" f)
        else iterator lst_it
    | _ -> 
        let list = List.map fst !readers in
        match my_select list with
        | [] -> 
            iterator lst_it
        | fds -> 
            incr actions;
            List.iter (fun r ->
                let f = List.assoc r !readers in
                Log.catch "Concur.iterator: %s\n" f) fds;
  with 
    _ -> iterator lst_it

external my_select_no_wait : Unix.file_descr list -> Unix.file_descr list
  = "my_select_no_wait"
  
let poll () =
  try
    let list = List.map fst !readers in
    match my_select_no_wait list with
    | [] -> false
    | fds -> 
        List.iter (fun r ->
            let f = List.assoc r !readers in
            try f () with _ -> ()) fds;
        true
  with _ -> false
      
      
module Mutex = 
  struct
    type t = unit
    let create () = ()
    let lock (m :  t) = ()
    let unlock (m :  t) = ()
    let try_lock (m : t) = true
  end
module Condition =
  struct
    type t = bool ref
    let create () = ref false
    let wait (c : t) (m : Mutex.t) =
      c  := false;
      let lst_it = ref 0 in
      while not !c do
        iterator lst_it;
      done
    let signal (c : t) =
      c  := true
    let broadcast (c : t) =
      c  := true
  end
module Thread =
  struct
    let add_reader fd f = readers  := (fd,f)  ::  !readers
    let remove_reader fd = 
      let rec iter list res =
        match list with
          [] -> res
        | ((fd',f) as ele)  ::  tail ->
            if fd = fd' then iter tail res
            else 
              iter tail (ele  ::  res)
      in
      readers  := iter !readers []

      
      
    let add_timer time f =  match !timers with
        [] -> 
          timers  := [time,f];
          let _ = setitimer ITIMER_REAL { it_interval = 0.0; it_value = time } in ()
      | (t1,f1)  ::  tail -> 
          let it = getitimer ITIMER_REAL in
          let t1 = it.it_value in
          if t1 >= time then
            let _ = setitimer ITIMER_REAL { it_interval = 0.0; it_value =time } in
            timers  := (time,f)  ::  (t1 -. time ,f1)  ::  tail
          else
          let rec iter tail time f =
            match tail with
              [] -> [time, f]
            | (t1, f1) :: tail ->
                if t1 < time then
                  (t1, f1) :: (iter tail (time -. t1) f)
                else
                  (time, f) :: (t1 -. time, f1) :: tail
          in
          (* Reset the timer (in case there is a change in the clock setting *)
          let _ = setitimer ITIMER_REAL { it_interval = 0.0; it_value = t1 } 
          in          
          timers := (t1,f1) :: (iter tail (time -. t1) f)
(*
            | (t1,f1)  ::  tail when t1 >= time -> 
          let it = getitimer ITIMER_REAL in
          let t1 = it.it_value in
          if t1 >= time then
            let _ = setitimer ITIMER_REAL { it_interval = 0.0; it_value =time } in
            timers  := (time,f)  ::  (t1 -. time ,f1)  ::  tail
          else
            timers  := (t1,f1)  ::  (time -. t1, f)  ::  tail
      | list -> 
          let rec iter list time =
            match list with
              (t1,f1)  ::  tail ->
                if time > t1 then
                  (t1,f1)  ::  (iter tail (time -. t1))
                else
                  (time,f)  ::  (t1 -. time,f1)  ::  tail
            | [] -> [time,f]
          in
          timers  := iter list time
  *)

    let fork () =
      let pid = Unix.fork () in
      if pid > 0 then forks := pid :: !forks;
      pid
      
  end
module ThreadUnix = Unix
  