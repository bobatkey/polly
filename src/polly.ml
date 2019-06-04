type constructor =
  | Permit
  | Deny
  | Not_applicable

  | IsPermit
  | IsDeny

  | Guard
  | FirstApplicable

  | Concat

  | GetField
  | GetString
  | GetInteger

  | JsonObject
  | JsonField
  | JsonString
  | JsonNumber

  | Is_equal_Integer
  | Is_equal_String

  | If
  | Try
  | Error
  | First_successful

  | Parse_json

  | Http_get

type base_sort =
  | String
  | Integer
  | Json
  | JsonField
  | Decision
  | Boolean

module Language = struct
  type nonrec constructor = constructor
  type nonrec base_sort = base_sort

  let base_sort_of_string = function
    | "string"     -> Ok String
    | "integer"    -> Ok Integer
    | "json"       -> Ok Json
    | "json-field" -> Ok JsonField
    | "decision"   -> Ok Decision
    | "boolean"    -> Ok Boolean
    | _            -> Error ()

  let string_of_base_sort = function
    | String    -> "string"
    | Integer   -> "integer"
    | Json      -> "json"
    | JsonField -> "json-field"
    | Decision  -> "decision"
    | Boolean   -> "boolean"

  module Base_Sort = struct
    type t = base_sort

    let equal (x : base_sort) (y : base_sort) =
      Stdlib.(=) x y

    let pp fmt s =
      Format.pp_print_string fmt (string_of_base_sort s)
  end

  let string_sort = String
  let integer_sort = Integer

  let constructor_of_string = function
    | "PERMIT"           -> Ok Permit
    | "DENY"             -> Ok Deny
    | "NOT_APPLICABLE"   -> Ok Not_applicable
    | "permits"          -> Ok IsPermit
    | "denies"           -> Ok IsDeny
    | "guard"            -> Ok Guard
    | "first-applicable" -> Ok FirstApplicable
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
    | "if"               -> Ok If
    | "try"              -> Ok Try
    | "error"            -> Ok Error
    | "first-successful" -> Ok First_successful
    | "parse-json"       -> Ok Parse_json
    | "http-get"         -> Ok Http_get
    | _                  -> Error ()

  let sort_of_constructor constructor =
    let open Checker.Sorts in
    match constructor with
    | Permit ->
      return (B Decision)
    | Deny ->
      return (B Decision)
    | Not_applicable ->
      return (B Decision)

    | IsPermit ->
      B Decision @--> return (B Boolean)
    | IsDeny ->
      B Decision @--> return (B Boolean)

    | Guard ->
      B Boolean @--> B Decision @--> return (B Decision)
    | FirstApplicable ->
      B Decision @*-> return (B Decision)
    | Concat ->
      B String @*-> return (B String)
    | GetField ->
      B Json @--> B String @--> return (B Json)
    | GetString ->
      B Json @--> return (B String)
    | GetInteger ->
      B Json @--> return (B Integer)

    | JsonObject ->
      B JsonField @*-> return (B Json)
    | JsonField ->
      B String @--> B Json @--> return (B JsonField)
    | JsonString ->
      B String @--> return (B Json)
    | JsonNumber ->
      B Integer @--> return (B Json)

    | Is_equal_String ->
      B String @--> B String @--> return (B Boolean)
    | Is_equal_Integer ->
      B Integer @--> B Integer @--> return (B Boolean)

    | If ->
      B Boolean @--> V "a" @--> V "a" @--> return (V "a")
    | Try ->
      V "a" @--> V "a" @--> return (V "a")
    | Error ->
      B String @--> return (V "a")
    | First_successful ->
      V "a" @*-> return (V "a")

    | Parse_json ->
      B String @--> return (B Json)
    | Http_get ->
      B String @--> return (B String)
end

module Checker = Checker.Make (Language)
