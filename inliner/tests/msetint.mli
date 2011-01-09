type t
val empty : unit -> t
val is_empty : t -> bool
val mem : int -> t -> bool
val add : int -> t -> unit
val singleton : int -> t
val remove : int -> t -> unit
val union : t -> t -> unit
val inter : t -> t -> unit
val diff : t -> t -> unit
val copy : t -> t
val min : int -> int -> int
val compare : t -> t -> int
val equal : t -> t -> bool
val subset : t -> t -> bool
val iter : (int -> unit) -> t -> unit
val fold : (int -> 'a -> 'a) -> 'a -> t -> 'a
val cardinal : t -> int
val elements : t -> int list
val min_elt : t -> int
val max_elt : t -> int
val choose : t -> int
