type function_symbol =
  | Concat

  | Parse_json

  | GetField
  | GetString
  | GetInteger

  | JsonObject
  | JsonField
  | JsonString
  | JsonNumber

  | Is_equal_Integer
  | Is_equal_String

  | Http_get

type base_sort =
  | String
  | Integer
  | Json
  | JsonField

module Language = struct
  type nonrec function_symbol = function_symbol
  type nonrec base_sort = base_sort

  let base_sort_of_string = function
    | "string"     -> Ok String
    | "integer"    -> Ok Integer
    | "json"       -> Ok Json
    | "json-field" -> Ok JsonField
    | _            -> Error ()

  let string_of_base_sort = function
    | String    -> "string"
    | Integer   -> "integer"
    | Json      -> "json"
    | JsonField -> "json-field"

  module Base_Sort = struct
    type t = base_sort

    let equal (x : base_sort) (y : base_sort) =
      Stdlib.(=) x y

    let pp fmt s =
      Format.pp_print_string fmt (string_of_base_sort s)
  end

  let string_sort = String
  let integer_sort = Integer

  let function_of_string = function
    | "concat"           -> Ok Concat
    | "get-field"        -> Ok GetField
    | "get-string"       -> Ok GetString
    | "get-integer"      -> Ok GetInteger
    | "object"           -> Ok JsonObject
    | "field"            -> Ok JsonField
    | "json-string"      -> Ok JsonString
    | "json-number"      -> Ok JsonNumber
    | "eq-string?"       -> Ok Is_equal_String
    | "eq-integer?"      -> Ok Is_equal_Integer
    | "parse-json"       -> Ok Parse_json
    | "http-get"         -> Ok Http_get
    | _                  -> Error ()

  let boolean =
    Checker.(EnumSort (ConstrSet.of_list [ "True"; "False" ]))
  let string =
    Checker.(AbstrSort String)
  let integer =
    Checker.(AbstrSort Integer)
  let json =
    Checker.(AbstrSort Json)
  let json_field =
    Checker.(AbstrSort JsonField)

  let function_scheme constructor =
    let open Checker.Sorts in
    match constructor with
    | Concat ->
      string @*-> return string
    | GetField ->
      json @--> string @--> return json
    | GetString ->
      json @--> return string
    | GetInteger ->
      json @--> return integer

    | JsonObject ->
      json_field @*-> return json
    | JsonField ->
      string @--> json @--> return json_field
    | JsonString ->
      string @--> return json
    | JsonNumber ->
      integer @--> return json

    | Is_equal_String ->
      string @--> string @--> return boolean
    | Is_equal_Integer ->
      integer @--> integer @--> return boolean

    | Parse_json ->
      string @--> return json
    | Http_get ->
      string @--> return string
end

module Checker = Checker.Make (Language)
