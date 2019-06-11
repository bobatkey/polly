module Lwt_result : sig
  include module type of Lwt_result
  val both : ('a, 'err) t -> ('b, 'err) t -> ('a * 'b, 'err) t
  val map_p : ('a -> ('b,'e) t) -> 'a list -> ('b list, 'e) t
end = struct
  include Lwt_result

  let both t1 t2 =
    Lwt.map
      (function
        | Error e, _ | _, Error e -> Error e
        | Ok x, Ok y              -> Ok (x, y))
      (Lwt.both t1 t2)

  let map_p f xs =
    let rec loop = function
      | [] ->
         Lwt_result.return []
      | t::ts ->
         both (f t) (loop ts) |> map (fun (x, xs) -> x :: xs)
    in
    loop xs
end

(* module Decision = struct
 *   module Type = struct
 *     type decision = Permit | Deny | Not_applicable
 *   end
 *
 *   include Type
 *
 *   type t = decision
 *
 *   let pp fmt = function
 *     | Permit ->
 *       Format.pp_print_string fmt "PERMIT"
 *     | Deny ->
 *       Format.pp_print_string fmt "DENY"
 *     | Not_applicable ->
 *       Format.pp_print_string fmt "NOT_APPLICABLE"
 * end *)

module Value = struct
  module Type = struct
    type value =
      | String    of string
      | Integer   of int
      | Symbol    of string
      | Json      of Yojson.Basic.t
      | JsonField of string * Yojson.Basic.t
  end

  include Type

  type t = value

  let pp fmt = function
    | String s   -> Format.fprintf fmt "String %S" s
    | Integer i  -> Format.fprintf fmt "Integer %d" i
    | Symbol c   -> Format.fprintf fmt "%s" c
    | Json j     -> Format.fprintf fmt "Json (%a)" Yojson.Basic.pp j
    | JsonField (n,j) ->
       Format.fprintf fmt "JsonField (%S, %a)"
         n
         Yojson.Basic.pp j
end

open Value.Type
open Polly.Checker

type 'a io =
  IO of ('a,string) result Lwt.t

type _ spec =
  | Ret   : ('a -> value) -> 'a spec
  | RetE  : ('a -> value) -> ('a,string) result spec
  | RetIO : ('a -> value) -> 'a io spec
  | ArgS  : (value -> 'a option) * 'b spec -> ('a -> 'b) spec
  | ArgL  : (value -> 'a option) * 'b spec -> ('a list -> 'b) spec

type 'a ep =
  { embed   : 'a -> value
  ; project : value -> 'a option
  }

let string =
  { embed   = (fun s -> String s)
  ; project = (function
               | String s -> Some s
               | _        -> None)
  }

let integer =
  { embed   = (fun s -> Integer s)
  ; project = (function
               | Integer i -> Some i
               | _         -> None)
  }

let json =
  { embed   = (fun j -> Json j)
  ; project = (function
               | Json j -> Some j
               | _      -> None)
  }

let boolean =
  { embed   = (fun b -> if b then Symbol "True" else Symbol "False")
  ; project =
      (function
        | Symbol "True"  -> Some true
        | Symbol "False" -> Some false
        | _              -> None)
  }

let jsonfield =
  { embed   = (fun (nm, j) -> JsonField (nm, j))
  ; project = (function
               | JsonField (nm, j) -> Some (nm, j)
               | _                 -> None)
  }

let value =
  { embed   = (fun v -> v)
  ; project = (fun x -> Some x)
  }

let ret ep = Ret ep.embed
let ret_e ep = RetE ep.embed
let ret_io ep = RetIO ep.embed
let (@->) ep c = ArgS (ep.project, c)
let (@*->) ep c = ArgL (ep.project, c)

let rec traverse f = function
  | [] -> Some []
  | x::xs ->
    (match f x with
     | None -> None
     | Some y ->
       match traverse f xs with
       | None -> None
       | Some ys -> Some (y::ys))

let eval_args eval args =
  Lwt_result.map_p
    (function
      | A_one arg   -> Lwt_result.map (fun s -> `S s) (eval arg)
      | A_many args -> Lwt_result.map (fun l -> `L l) (Lwt_result.map_p eval args))
    args

let rec lift : type a. a spec -> a -> [`S of value|`L of value list] list -> (value,string) result Lwt.t =
  fun spec f args -> match spec, f, args with
    | Ret em, v, [] ->
      Lwt.return (Ok (em v))
    | RetE em, v, [] ->
      (match v with
       | Ok x         -> Lwt.return (Ok (em x))
       | Error _ as e -> Lwt.return e)
    | RetIO em, IO t, [] ->
      Lwt_result.map em t
    | ArgS (p, c), f, `S v::args ->
      (match p v with
       | None -> Lwt.fail_with "type error"
       | Some x -> lift c (f x) args)
    | ArgL (p, c), f, `L vs::args ->
      (match traverse p vs with
       | None -> Lwt.fail_with "type error"
       | Some xs -> lift c (f xs) args)
    | _ ->
      Lwt.fail_with "wrong number or kind of arguments"

let lift spec f eval arguments =
  Lwt_result.bind
    (eval_args eval arguments)
    (lift spec f)

let concat =
  lift (string @*-> ret string)
    (String.concat "")

let get_field =
  lift (json @-> string @-> ret_e json)
    (fun json fnm -> match json with
       | `Assoc assocs ->
         (match List.assoc fnm assocs with
          | exception Not_found -> Error "field not found"
          | value               -> Ok value)
       | _ ->
         Error "not a JSON object")
let get_string =
  lift (json @-> ret_e string)
    (function `String s -> Ok s
            | _         -> Error "json: not a string")
let get_integer =
  lift (json @-> ret_e integer)
    (function `Int i -> Ok i
            | _      -> Error "json: not an integer")

let json_object =
  lift (jsonfield @*-> ret json)
    (fun fields -> `Assoc fields)
let json_field =
  lift (string @-> json @-> ret jsonfield)
    (fun nm j -> (nm, j))
let json_string =
  lift (string @-> ret json)
    (fun s -> `String s)
let json_number =
  lift (integer @-> ret json)
    (fun i -> `Int i)

let eq_string =
  lift (string @-> string @-> ret boolean)
    (=)
let eq_integer =
  lift (integer @-> integer @-> ret boolean)
    (=)

let error =
  lift (string @-> ret_e value)
    (fun s -> Error s)

let parse_json =
  lift (string @-> ret_e json)
    (fun str -> match Yojson.Basic.from_string str with
       | exception (Yojson.Json_error msg) ->
         Error msg
       | json ->
         Ok json)

let http_get =
  lift (string @-> ret_io string)
    begin fun uri_str ->
      let uri = Uri.of_string uri_str in
      IO (Lwt_result.bind
            (Lwt_result.map_err
               Printexc.to_string
               (Lwt_result.catch (Cohttp_lwt_unix.Client.get uri)))
            (fun (resp, body) ->
               if Cohttp.Response.status resp = `OK then
                 Lwt_result.ok (Cohttp_lwt.Body.to_string body)
               else
                 Lwt_result.fail "HTTP failed"))
    end


(**********************************************************************)

type state =
  | Evaled   of (Value.t, string) Lwt_result.t
  | Unevaled of expr

type store =
  (string, state) Hashtbl.t

let log_with_time fmt =
  let time = Ptime_clock.now () in
  Lwt_io.eprintf ("%s: " ^^ fmt) (Ptime.to_rfc3339 ~frac_s:5 time)

let rec eval_name (table : store) nm =
  match Hashtbl.find table nm with
  | exception Not_found ->
    Lwt.fail_with (Printf.sprintf "definition not found: %s" nm)
  | Evaled t ->
    t
  | Unevaled expr ->
    let open Lwt.Infix in
    let task =
      log_with_time "Starting %s\n%!" nm
      >>= fun () ->
      eval table expr
      >>= function
      | Ok _ as value ->
        log_with_time "Finishing %s (OK)\n%!" nm >|= fun () ->
        value
      | Error e as value ->
        log_with_time "Finishing %s (ERROR: %s)\n%!" nm e >|= fun () ->
        value
    in
    Hashtbl.replace table nm (Evaled task);
    task


and eval table = function
  | E_name nm ->
    eval_name table nm

  | E_string s ->
    Lwt_result.return (String s)

  | E_int i ->
    Lwt_result.return (Integer i)

  | E_cons c ->
    Lwt_result.return (Symbol c)

  | E_func (Concat, arguments) ->
    concat (eval table) arguments

  | E_func (GetField, arguments) ->
    get_field (eval table) arguments

  | E_func (GetString, arguments) ->
    get_string (eval table) arguments

  | E_func (GetInteger, arguments) ->
    get_integer (eval table) arguments

  | E_func (JsonObject, arguments) ->
    json_object (eval table) arguments

  | E_func (JsonField, arguments) ->
    json_field (eval table) arguments

  | E_func (JsonString, arguments) ->
    json_string (eval table) arguments

  | E_func (JsonNumber, arguments) ->
    json_number (eval table) arguments

  | E_func (Is_equal_String, arguments) ->
    eq_string (eval table) arguments

  | E_func (Is_equal_Integer, arguments) ->
    eq_integer (eval table) arguments

  | E_func (Parse_json, arguments) ->
    parse_json (eval table) arguments

  | E_func (Http_get, arguments) ->
    http_get (eval table) arguments

  | E_table { cols; rows } ->
    Lwt_result.bind
      (Lwt_result.map_p (eval table) cols)
      (fun values ->
         eval_rows table values rows)

and eval_rows table values = function
  | [] ->
    Lwt.fail_with "internal error: match failure"
  | { pattern; expr }::rows ->
    (match match_pattern pattern values with
     | Some [] ->
       eval table expr
     | Some _ ->
       Lwt.fail_with "internal error: pattern underrun"
     | None ->
       eval_rows table values rows)

and match_pattern pattern values =
  match pattern, values with
  | P_cons cnm, Symbol cnm'::values ->
    if String.equal cnm cnm'
    then Some values
    else None
  | P_cons _, _::_ ->
    None
  | P_any, _::values ->
    Some values
  | (P_cons _ | P_any), [] ->
    failwith "internal error: pattern, but no values"
  | P_seq (p::ps), values ->
    (match match_pattern p values with
     | None ->
       None
     | Some values ->
       match_pattern (P_seq ps) values)
  | P_seq [], values ->
    Some values
  | P_or ps, values ->
    let rec loop = function
      | [] -> None
      | p::ps ->
        match match_pattern p values with
        | None -> loop ps
        | Some values -> Some values
    in
    loop ps

let eval args program =
  let table = Hashtbl.create 100 in
  program.required |> Hashtbl.iter begin fun name _sort ->
    (* FIXME: checking the parameters ought to be done in the checker *)
    (* assert (Polly.Language.Base_Sort.equal sort String); *)
    let value = String (List.assoc name args) in
    Hashtbl.add table name (Evaled (Lwt_result.return value))
  end;
  program.defined |> Hashtbl.iter begin fun name expr ->
    Hashtbl.add table name (Unevaled expr)
  end;
  eval_name table "main"
