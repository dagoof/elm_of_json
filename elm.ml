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
  | `Array  of composite_decoder
  | `Object of (string * composite_decoder) list
  | `OneOf  of composite_decoder list
  ] [@@deriving show]

let unify_decoders decoders =
  match List.unique decoders with
  | [x] -> `Array x
  | xs  -> `OneOf xs

let rec decode = function
  | `A vals   -> unify_decoders @@ List.map decode vals
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
         ]
     ]

let () =
  print_endline @@ show_composite_decoder @@ decode the_json
