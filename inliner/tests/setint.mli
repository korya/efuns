type t
val empty : t
val is_empty : t -> bool
val mem : int -> t -> bool
val add : int -> t -> t
val singleton : int -> t
val remove : int -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
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
