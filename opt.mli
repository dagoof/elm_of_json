type 'a t = 'a option

val some : 'a -> 'a t
val none : 'a t
val bind : ('a -> 'b t) -> 'a t -> 'b t
val map :  ('a -> 'b) -> 'a t -> 'b t
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val default : 'a -> 'a t -> 'a
