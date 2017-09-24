open Batteries

let (>>>) f g x = g @@ f x

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

let mergesets a b =
  let emptyset = Opt.default Set.empty in
  Set.union ( emptyset a ) ( emptyset b )

let combine_objects objs =
  List.of_enum @@
  Enum.map begin fun (key, decoders) ->
    (key, unify_decoders @@ Set.to_list decoders)
  end @@
  Map.enum @@
  List.fold_left begin fun sofar obj ->
    let next =
      List.fold_left begin fun map (field, decoder) ->
        Map.modify_opt field begin fun x ->
          match x with
          | Some decoders -> Opt.some @@ Set.add decoder decoders
          | None ->          Opt.some @@ Set.singleton decoder
        end map
      end Map.empty obj
    in

    Map.merge (fun key a b -> Opt.some @@ mergesets a b ) sofar next
  end Map.empty objs


let rec decode = function
  | `A vals   ->
    let decoders = List.map decode vals in
    let objs = `Object ( combine_objects @@ objects decoders ) in
    `Array (unify_decoders @@ objs :: non_objects decoders)

  | `O vals   -> `Object (List.map (fun (k, v) -> k, decode v) vals)
  | `Null     -> `Null
  | `Bool   v -> `Bool
  | `Float  v -> `Number
  | `String v -> `String

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
         ; `O [ "always", `String "ok"; "image_url", `String "Up"; "user_profile", `String "sure" ]
         ; `O [ "always", `Bool true;   "image_url", `Bool false ]
         ]
     ]

let () =
  print_endline @@ show_composite_decoder @@ decode the_json
