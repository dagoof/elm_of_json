open Batteries

let (>>>) f g x = g @@ f x

module Map = struct
  include Map

  let to_list =
    Map.enum >>> List.of_enum

  let of_list =
    List.enum >>> Map.of_enum
end

type basic_decoder =
  [ `Null
  | `Bool
  | `String
  | `Number
  ] [@@deriving show]

type composite_decoder =
  [ basic_decoder
  | `Optional of composite_decoder
  | `Array  of composite_decoder
  | `Object of (string * composite_decoder) list
  | `OneOf  of composite_decoder list
  ] [@@deriving show]


let object_part = function
  | `Object xs -> Some xs
  | otherwise  -> None

let is_object = function
  | `Object xs -> true
  | otherwise  -> false

let objects objs =
  List.filter_map object_part objs

let non_objects objs =
  List.filter (is_object >>> not) objs

let unify_decoders decoders =
  match List.unique decoders with
  | [x] -> x
  | xs  -> `OneOf xs

module Sometimes : sig
  type 'a t =
    | Required of 'a
    | Optional of 'a
  val map : ( 'a -> 'b ) -> 'a t -> 'b t
  val required : 'a -> 'a t
  val optional : 'a -> 'a t
  val of_t : 'a t -> 'a
end = struct
  type 'a t =
    | Required of 'a
    | Optional of 'a

  let required v = Required v

  let optional v = Optional v

  let map fn = function
    | Required v -> required @@ fn v
    | Optional v -> optional @@ fn v

  let of_t = function
    | Required v -> v
    | Optional v -> v
end

let merge_maps key merged next =
  let open Sometimes in

  match (next, merged) with
  | Some v, None ->
    Opt.some @@ optional [v]
  | None, Some ( Required v ) ->
    Opt.some @@ optional v
  | Some v, Some something ->
    Opt.some @@ map (fun vs -> v :: vs) something
  | None, v -> v

let decoder_of_sometimes sometimes =
  let _decoder = function
    | Sometimes.Required v -> v
    | Sometimes.Optional v -> `Optional v
  in

  _decoder @@ Sometimes.map unify_decoders sometimes


let rec decode = function
  | `A vals   ->
    let decoders = List.map decode vals in

    let objs =
      Opt.map
        (fun combined -> `Object combined)
        (rudimentary_combine @@ objects decoders )
    and others = non_objects decoders in
    let decoders2 =
      Opt.(default others @@ map2 List.cons objs (some others))
    in

    `Array (unify_decoders decoders2)

  | `O vals   -> `Object (List.map (fun (k, v) -> k, decode v) vals)
  | `Null     -> `Null
  | `Bool   v -> `Bool
  | `Float  v -> `Number
  | `String v -> `String

and rudimentary_combine objs =
  let whoa sofar next =
    Opt.some @@

    match sofar with
    | None ->
      Map.map (List.singleton >>> Sometimes.required) next
    | Some stuff ->
      Map.merge merge_maps stuff next
  in

  Opt.map Map.to_list @@
  Opt.map (Map.map decoder_of_sometimes) @@
  List.fold_left
    whoa
    None
    (List.map Map.of_list objs)

type 'a tree =
  | Branch of 'a * 'a tree list
  | Leaf

module Elm = struct
  type import =
    { modul : string
    ; alias : string option
    ; exposing : string list option
    }

  let import ?alias ?exposing modul =
    { modul; alias; exposing }

  type basic_type =
    | Bool of bool
    | Int of int
    | Float of float
    | String of string

  type t =
    | Simple of basic_type
    | Record of string * t list
    | List   of t list

(*
    | Enum   of string * t option list
    | Tuple  of t list
*)

  type fn =
    | Broken

  type ast =
    { imports : import list
    ; types : t tree
    ; funcs : fn tree
    }

  let import_to_string import =
    let or_empty f t =
      Opt.( map f t |> default "" )
    in

    let alias =
      or_empty (Printf.sprintf " as %s") import.alias
    and exposing =
      or_empty
        (String.concat ", " >>> Printf.sprintf " exposing (%s)")
        import.exposing
    in

    Printf.sprintf "import %s%s%s" import.modul alias exposing

  let typical_imports =
    [ import ~alias:"Decoder" "Json.Decoder"
    ; import ~alias:"H" "Html"
    ; import ~alias:"E" "Html.Events"
    ; import ~alias:"A" "Html.Attributes"
    ; import ~exposing:["second"; "millisecond"] "Time"
    ]

end

let () =
  List.iter Elm.( import_to_string >>> print_endline ) Elm.typical_imports

let the_json =
  `O [ "data"
     , `A
         [ `O [ "image_url", `String "https://test.com" ]
         ; `O [ "image_url", `String "Up" ]
         ; `O [ "image_url", `Bool false ]
         ]
     ; "data2"
     , `A
         [ `O [ "always", `Bool true;   "image_url", `String "https://test.com" ]
         ; `O [ "always", `Bool false;  "user_profile", `String "tester" ]
         ; `O [ "always", `Bool false; "image_url", `String "Up"; "user_profile", `String "sure" ]
         ; `O [ "always", `Bool true;   "image_url", `Bool false ]
         ]
     ]

let () =
  print_endline @@ show_composite_decoder @@ decode the_json
