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

module Decision = struct
  module Type = struct
    type decision = Permit | Deny | Not_applicable
  end

  include Type

  type t = decision

  let pp fmt = function
    | Permit ->
      Format.pp_print_string fmt "PERMIT"
    | Deny ->
      Format.pp_print_string fmt "DENY"
    | Not_applicable ->
      Format.pp_print_string fmt "NOT_APPLICABLE"
end

module Value = struct
  module Type = struct
    type value =
      | String    of string
      | Integer   of int
      | Boolean   of bool
      | Decision  of Decision.t
      | Json      of Yojson.Basic.t
      | JsonField of string * Yojson.Basic.t
  end

  include Type

  type t = value

  let pp fmt = function
    | String s   -> Format.fprintf fmt "String %S" s
    | Integer i  -> Format.fprintf fmt "Integer %d" i
    | Boolean b  -> Format.fprintf fmt "Boolean %b" b
    | Decision d -> Format.fprintf fmt "Decision %a" Decision.pp d
    | Json j     -> Format.fprintf fmt "Json (%a)" Yojson.Basic.pp j
    | JsonField (n,j) ->
       Format.fprintf fmt "JsonField (%S, %a)"
         n
         Yojson.Basic.pp j
end

open Value.Type
open Decision.Type
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

let decision =
  { embed   = (fun d -> Decision d)
  ; project = (function
               | Decision d -> Some d
               | _          -> None)
  }

let boolean =
  { embed   = (fun b -> Boolean b)
  ; project = (function
               | Boolean b -> Some b
               | _         -> None)
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
      | A_single arg -> Lwt_result.map (fun s -> `S s) (eval arg)
      | A_list args  -> Lwt_result.map (fun l -> `L l) (Lwt_result.map_p eval args))
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

let permit = lift (ret decision) Permit
let deny   = lift (ret decision) Deny
let not_applicable = lift (ret decision) Not_applicable

let is_permit = lift (decision @-> ret boolean)
    (function Permit -> true
            | _      -> false)
let is_deny = lift (decision @-> ret boolean)
    (function Deny -> true
            | _    -> false)

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

  | E_cons { constructor=Permit; arguments } ->
    permit (eval table) arguments

  | E_cons { constructor=Deny; arguments } ->
    deny (eval table) arguments

  | E_cons { constructor=Not_applicable; arguments } ->
    not_applicable (eval table) arguments

  | E_cons { constructor=IsPermit; arguments } ->
    is_permit (eval table) arguments
  | E_cons { constructor=IsDeny; arguments } ->
    is_deny (eval table) arguments

  | E_cons { constructor=Guard
           ; arguments=[A_single c; A_single d] } ->
    Lwt_result.bind
      (eval table c)
      (function
        | Boolean true  -> eval table d
        | Boolean false -> Lwt_result.return (Decision Not_applicable)
        | _             -> Lwt.fail_with "type error")

  | E_cons { constructor=FirstApplicable
           ; arguments=[A_list exprs] } ->
    let rec get_first = function
      | [] ->
        Lwt_result.return (Decision Not_applicable)
      | e::es ->
        Lwt_result.bind
          (eval table e)
          (function
            | Decision (Permit | Deny) as r ->
              Lwt_result.return r
            | Decision Not_applicable ->
              get_first es
            (* FIXME: handle errors specially? *)
            | _ ->
              Lwt.fail_with "type error")
    in
    get_first exprs

  | E_cons { constructor=Concat; arguments } ->
    concat (eval table) arguments

  | E_cons { constructor=GetField; arguments } ->
    get_field (eval table) arguments

  | E_cons { constructor=GetString; arguments } ->
    get_string (eval table) arguments

  | E_cons { constructor=GetInteger; arguments } ->
    get_integer (eval table) arguments

  | E_cons { constructor=JsonObject; arguments } ->
    json_object (eval table) arguments

  | E_cons { constructor=JsonField; arguments } ->
    json_field (eval table) arguments

  | E_cons { constructor=JsonString; arguments } ->
    json_string (eval table) arguments

  | E_cons { constructor=JsonNumber; arguments } ->
    json_number (eval table) arguments

  | E_cons { constructor=Is_equal_String; arguments } ->
    eq_string (eval table) arguments

  | E_cons { constructor=Is_equal_Integer; arguments } ->
    eq_integer (eval table) arguments

  | E_cons { constructor=If
           ; arguments=[A_single e_c; A_single e_thn; A_single e_els] } ->
    Lwt_result.bind (eval table e_c)
      (function
        | Boolean true ->
          eval table e_thn
        | Boolean false ->
          eval table e_els
        | _ ->
          Lwt.return (Error "type error"))

  | E_cons { constructor=Try; arguments=[A_single body; A_single on_error] } ->
    Lwt.bind (eval table body)
      (function
        | Ok value ->
          Lwt_result.return value
        | Error _ ->
          eval table on_error)

  | E_cons { constructor=Error; arguments } ->
    error (eval table) arguments

  | E_cons { constructor=First_successful; arguments=[A_list exprs] } ->
    let rec try_loop = function
      | [] ->
        Lwt_result.fail "No successful result"
      | expr::exprs ->
        Lwt.bind (eval table expr)
          (function
            | Ok value ->
              Lwt_result.return value
            | Error _ ->
              try_loop exprs)
    in
    try_loop exprs

  | E_cons { constructor=Parse_json; arguments } ->
    parse_json (eval table) arguments

  | E_cons { constructor=Http_get; arguments } ->
    http_get (eval table) arguments

  | E_cons _ ->
    Lwt.fail_with "internal: syntax error"

let eval args program =
  let table = Hashtbl.create 100 in
  program.required |> Hashtbl.iter begin fun name sort ->
    assert (Polly.Language.Base_Sort.equal sort String);
    let value = String (List.assoc name args) in
    Hashtbl.add table name (Evaled (Lwt_result.return value))
  end;
  program.defined |> Hashtbl.iter begin fun name expr ->
    Hashtbl.add table name (Unevaled expr)
  end;
  eval_name table "main"
