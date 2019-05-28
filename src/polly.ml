type constructor =
  | Permit
  | Deny

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

type sort =
  | String
  | Integer
  | Json
  | JsonField
  | Decision
  | Boolean

module Language = struct
  type nonrec constructor = constructor
  type nonrec sort = sort

  let sort_of_string = function
    | "string"     -> Ok String
    | "integer"    -> Ok Integer
    | "json"       -> Ok Json
    | "json-field" -> Ok JsonField
    | "decision"   -> Ok Decision
    | "boolean"    -> Ok Boolean
    | _            -> Error ()

  let string_of_sort = function
    | String    -> "string"
    | Integer   -> "integer"
    | Json      -> "json"
    | JsonField -> "json-field"
    | Decision  -> "decision"
    | Boolean   -> "boolean"

  module Sort = struct
    type t = sort

    let equal (x : sort) (y : sort) =
      Stdlib.(=) x y
  end

  let string_sort = String
  let integer_sort = Integer

  let constructor_of_string = function
    | "PERMIT"           -> Ok Permit
    | "DENY"             -> Ok Deny
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
