let (>>>) f g x = g @@ f x

type 'a t = 'a option

let some v = Some v

let none = None

let bind f = function
  | Some v -> f v
  | None -> None

let map f = bind ( f >>> some )

let default v = function
  | Some v -> v
  | None -> v
