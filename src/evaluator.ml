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

  open Type

  type t = decision

  let pp fmt = function
    | Permit         -> Format.pp_print_string fmt "PERMIT"
    | Deny           -> Format.pp_print_string fmt "DENY"
    | Not_applicable -> Format.pp_print_string fmt "NOT_APPLICABLE"
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

  open Type

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


let concat xs =
  let buf = Buffer.create 12 in
  let rec loop = function
    | [] ->
       Ok (String (Buffer.contents buf))
    | String s::vs ->
       Buffer.add_string buf s;
       loop vs
    | _::_ ->
       Error "type error"
  in
  loop xs

open Polly.Checker

type state =
  | Evaled   of (Value.t, string) Lwt_result.t
  | Unevaled of expr

type store =
  (string, state) Hashtbl.t

let log_with_time fmt =
  let time = Ptime_clock.now () in
  Lwt_io.printf ("%s: " ^^ fmt) (Ptime.to_rfc3339 ~frac_s:5 time)

let rec eval_name (table : store) nm =
  match Hashtbl.find table nm with
    | exception Not_found ->
       Lwt.fail_with (Printf.sprintf "definition not found: %s" nm)
    | Evaled t ->
       t
    | Unevaled expr ->
       let open Lwt.Infix in
       log_with_time "Starting %s\n%!" nm >>= fun () ->
       let task = eval table expr in
       Hashtbl.replace table nm (Evaled task);
       task >>= fun value ->
       log_with_time "Finishing %s\n%!" nm >|= fun () ->
       value

and eval table = function
  | E_name nm ->
     eval_name table nm

  | E_string s ->
     Lwt_result.return (String s)

  | E_int i ->
     Lwt_result.return (Integer i)

  | E_cons { constructor=Permit; arguments } ->
     (match arguments with
       | [] -> Lwt_result.return (Decision Permit)
       | _  -> Lwt.fail_with "syntax error: PERMIT")

  | E_cons { constructor=Deny; arguments } ->
     (match arguments with
       | [] -> Lwt_result.return (Decision Deny)
       | _  -> Lwt.fail_with "syntax error: DENY")

  | E_cons { constructor=Guard; arguments } ->
     (match arguments with
       | [A_single c; A_single d] ->
          Lwt_result.bind
            (eval table c)
            (function
              | Boolean true  -> eval table d
              | Boolean false -> Lwt_result.return (Decision Not_applicable)
              | _             -> Lwt.fail_with "type error")
       | _ ->
          Lwt.fail_with "syntax error: guard")

  | E_cons { constructor=FirstApplicable; arguments} ->
     (match arguments with
       | [A_list exprs] ->
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
       | _ ->
          Lwt.fail_with "syntax error: first-applicable")

  | E_cons { constructor=Concat; arguments } ->
     (match arguments with
       | [A_list exprs] ->
          Lwt_result.bind_result
            (Lwt_result.map_p (eval table) exprs)
            concat
       | _ ->
          Lwt.fail_with "syntax error: concat")

  | E_cons { constructor=GetField; arguments } ->
     (match arguments with
       | [A_single e_json; A_single e_name] ->
          Lwt_result.bind
            (Lwt_result.both (eval table e_json) (eval table e_name))
            (function
              | Json (`Assoc assocs), String fname ->
                 (match List.assoc fname assocs with
                   | exception _ -> Lwt.return (Error "not found")
                   | value       -> Lwt_result.return (Json value))
              | Json _, String _ ->
                 Lwt_result.fail "get-field: not a json object"
              | _ ->
                 Lwt.fail_with "type error")
       | _ ->
          Lwt.fail_with "syntax error: get-field")

  | E_cons { constructor=GetString; arguments } ->
     (match arguments with
       | [A_single e_json] ->
          Lwt_result.bind
            (eval table e_json)
            (function
              | Json (`String s) -> Lwt_result.return (String s)
              | Json _           -> Lwt_result.fail "json: not a string"
              | _                -> Lwt.fail_with "type error")
       | _ ->
          Lwt.fail_with "syntax error: string")

  | E_cons { constructor=GetInteger; arguments } ->
     (match arguments with
       | [A_single e_json] ->
          Lwt_result.bind
            (eval table e_json)
            (function
              | Json (`Int i) -> Lwt_result.return (Integer i)
              | Json _        -> Lwt_result.fail "json: not an integer"
              | _             -> Lwt.fail_with "type error")
       | _ ->
          Lwt.fail_with "syntax error: integer")

  | E_cons { constructor=JsonObject; arguments } ->
     (match arguments with
       | [A_list fields] ->
          Lwt_result.bind (Lwt_result.map_p (eval table) fields)
            (fun fields ->
               let rec loop fs = function
                 | [] ->
                    Ok (List.rev fs)
                 | JsonField (nm, json)::vs ->
                    loop ((nm, json)::fs) vs
                 | _::_ ->
                    Error "type error"
               in
               Lwt_result.bind (Lwt.return (loop [] fields))
                 (fun fields ->
                    Lwt_result.return (Json (`Assoc fields))))
       | _ ->
          Lwt.fail_with "syntax error: object")

  | E_cons { constructor=JsonField; arguments } ->
     (match arguments with
       | [A_single nm; A_single json] ->
          Lwt_result.bind (Lwt_result.both (eval table nm) (eval table json))
            (function
              | String nm, Json json ->
                 Lwt_result.return (JsonField (nm, json))
              | _ ->
                 Lwt_result.fail "type error")
       | _ ->
          Lwt.fail_with "syntax error: field")

  | E_cons { constructor=JsonString; arguments } ->
     (match arguments with
       | [A_single e_str] ->
          Lwt_result.bind (eval table e_str)
            (function
              | String str ->
                 Lwt_result.return (Json (`String str))
              | _ ->
                 Lwt_result.fail "type error")
       | _ ->
          Lwt.fail_with "syntax error: json-string")

  | E_cons { constructor=JsonNumber; arguments } ->
     (match arguments with
       | [A_single e_str] ->
          Lwt_result.bind (eval table e_str)
            (function
              | Integer i ->
                 Lwt_result.return (Json (`Int i))
              | _ ->
                 Lwt_result.fail "type error")
       | _ ->
          Lwt.fail_with "syntax error: json-number")

  | E_cons { constructor=Is_equal_String; arguments } ->
     (match arguments with
       | [A_single e1; A_single e2] ->
          Lwt_result.bind
            (Lwt_result.both (eval table e1) (eval table e2))
            (function
              | String s1, String s2 ->
                 Lwt_result.return (Boolean (s1 = s2))
              | _ ->
                 Lwt.fail_with "type error")
       | _ ->
          Lwt.fail_with "syntax error: eq-string?")

  | E_cons { constructor=Is_equal_Integer; arguments } ->
     (match arguments with
       | [A_single e1; A_single e2] ->
          Lwt_result.bind
            (Lwt_result.both (eval table e1) (eval table e2))
            (function
              | Integer i1, Integer i2 ->
                 Lwt_result.return (Boolean (i1 = i2))
              | _ ->
                 Lwt.fail_with "type error")
       | _ ->
          Lwt.fail_with "syntax error: eq-integer?")

  | E_cons { constructor=If; arguments } ->
     (match arguments with
       | [A_single e_c; A_single e_thn; A_single e_els] ->
          Lwt_result.bind (eval table e_c)
            (function
              | Boolean true ->
                 eval table e_thn
              | Boolean false ->
                 eval table e_els
              | _ ->
                 Lwt.return (Error "type error"))
       | _ ->
          Lwt.fail_with "syntax error: if")

  | E_cons { constructor=Try; arguments } ->
     (match arguments with
       | [A_single body; A_single on_error] ->
          Lwt.bind (eval table body)
            (function
              | Ok value ->
                 Lwt_result.return value
              | Error _ ->
                 eval table on_error)
       | _ ->
          Lwt.fail_with "syntax error: try")

  | E_cons { constructor=Error; arguments } ->
     (match arguments with
       | [A_single e_msg] ->
          Lwt_result.bind (eval table e_msg)
            (function
              | String s ->
                 Lwt_result.fail s
              | _ ->
                 Lwt.return (Error "type error"))
       | _ ->
          Lwt.fail_with "syntax error: error")

  | E_cons { constructor=First_successful; arguments } ->
     (match arguments with
       | [A_list exprs] ->
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
       | _ ->
          Lwt.fail_with "syntax error: first-sucessful")

  | E_cons { constructor=Parse_json; arguments } ->
     (match arguments with
       | [A_single e] ->
          Lwt_result.bind
            (eval table e)
            (function
              | String str ->
                 (match Yojson.Basic.from_string str with
                   | exception (Yojson.Json_error msg) ->
                      Lwt_result.fail msg
                   | json ->
                      Lwt_result.return (Json json))
              | _ ->
                 Lwt.fail_with "type error")
       | _ ->
          Lwt.fail_with "syntax error: json-of-string")

  | E_cons { constructor=Http_get; arguments } ->
     (match arguments with
       | [A_single e_url] ->
          Lwt_result.bind
            (eval table e_url)
            (function
              | String uri_str ->
                 let uri = Uri.of_string uri_str in
                 Lwt_result.bind
                   (Lwt_result.map_err
                      Printexc.to_string
                      (Lwt_result.catch (Cohttp_lwt_unix.Client.get uri)))
                   (fun (resp, body) ->
                      if Cohttp.Response.status resp = `OK then
                        Lwt.bind
                          (Cohttp_lwt.Body.to_string body)
                          (fun data -> Lwt_result.return (String data))
                      else
                        Lwt_result.fail "HTTP failed")
              | _ ->
                 Lwt.fail_with "type error")
       | _ ->
          Lwt.fail_with "syntax error: http-get")

let eval args program =
  let table = Hashtbl.create 100 in
  program.required |> Hashtbl.iter begin fun name sort ->
    assert (Polly.Language.Sort.equal sort String);
    let value = String (List.assoc name args) in
    Hashtbl.add table name (Evaled (Lwt_result.return value))
  end;
  program.defined |> Hashtbl.iter begin fun name expr ->
    Hashtbl.add table name (Unevaled expr)
  end;
  eval_name table "main"
