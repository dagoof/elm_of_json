type 'a t = 'a option

val some : 'a -> 'a t
val none : 'a t
val bind : ('a -> 'b t) -> 'a t -> 'b t
val map :  ('a -> 'b) -> 'a t -> 'b t
val default : 'a -> 'a t -> 'a
