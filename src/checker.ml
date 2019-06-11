type constructor_name =
  string

module ConstrSet = Set.Make (String)

type 'base sort =
  | EnumSort  of ConstrSet.t
  | AbstrSort of 'base

type 'base poly_sort =
  | V of string
  | B of 'base sort

type 'base constructor_scheme =
  { arg_sorts : [ `S of 'base sort | `M of 'base sort ] list
  ; ret_sort  : 'base sort
  }

module Sorts = struct
  type 'base t = 'base constructor_scheme

  let return s =
    { arg_sorts = []
    ; ret_sort  = s
    }

  let ( @--> ) sort c =
    { c with arg_sorts = `S sort :: c.arg_sorts }

  let ( @*-> ) sort c =
    { c with arg_sorts = `M sort :: c.arg_sorts }
end

module type LANGUAGE = sig
  type function_symbol

  type base_sort

  val base_sort_of_string : string -> (base_sort, unit) result

  val string_sort : base_sort

  val integer_sort : base_sort

  val function_of_string : string -> (function_symbol, unit) result

  val function_scheme : function_symbol -> base_sort constructor_scheme

  module Base_Sort : sig
    type t = base_sort

    val equal : t -> t -> bool

    val pp : Format.formatter -> t -> unit
  end
end

module type CHECKER = sig
  module L : LANGUAGE

  type base_sort =
    L.base_sort

  type nonrec sort =
    base_sort sort

  type constructor_symbol =
    string

  type identifier =
    string

  type pattern =
    | P_cons   of constructor_symbol
    | P_any
    | P_seq    of pattern list
    | P_or     of pattern list
    | P_string of string

  type expr =
    | E_string of string
    | E_int    of int
    | E_name   of string
    | E_cons   of string
    | E_func   of L.function_symbol * argument list
    | E_table  of { cols : expr list; rows : clause list }

  and clause =
    { pattern : pattern
    ; expr    : expr
    }

  and argument =
    | A_one  of expr
    | A_many of expr list

  type program = private
    { defined  : (string, expr) Hashtbl.t
    ; required : (string, sort) Hashtbl.t
    }

  val check_program : Ast.program -> sort -> (program, string) result
end

module Make (L : LANGUAGE) : CHECKER with module L = L = struct
  module L = L

  type base_sort = L.base_sort
  type nonrec sort = base_sort sort

  type constructor_symbol =
    string

  type identifier =
    string

  type pattern =
    | P_cons   of constructor_symbol
    | P_any
    | P_seq    of pattern list
    | P_or     of pattern list
    | P_string of string

  type expr =
    | E_string of string
    | E_int    of int
    | E_name   of string
    | E_cons   of string
    | E_func   of L.function_symbol * argument list
    | E_table  of { cols : expr list; rows : clause list }

  and clause =
    { pattern : pattern
    ; expr    : expr
    }

  and argument =
    | A_one  of expr
    | A_many of expr list

  type program =
    { defined  : (string, expr) Hashtbl.t
    ; required : (string, sort) Hashtbl.t
    }

  open R.Infix

  let compare_types loc expected computed =
    match expected, computed with
    | AbstrSort s1, AbstrSort s2 ->
      if L.Base_Sort.equal s1 s2 then Ok ()
      else
        R.errorf "Expecting sort '%a', but expression has sort '%a' at %a"
          L.Base_Sort.pp s1
          L.Base_Sort.pp s2
          Location.pp    loc
    | EnumSort cs1, EnumSort cs2 ->
      if ConstrSet.equal cs1 cs2 then Ok ()
      else
        R.errorf
          "Expected constructors and possible constructors do not match at %a"
          Location.pp loc
    | AbstrSort _, EnumSort _
    | EnumSort _,  AbstrSort _ ->
      R.errorf
        "Sort mismatch at %a"
        Location.pp loc

  let check_sort = function
    | Ast.{ data = S_name nm; loc } ->
      (match L.base_sort_of_string nm with
       | Error () ->
         R.errorf "Named sort '%s' not recognised at %a"
           nm
           Location.pp loc
       | Ok sort ->
         Ok (AbstrSort sort))
    | Ast.{ data = S_enum constrs; loc } ->
      let rec build s = function
        | [] -> Ok (EnumSort s)
        | c::cs ->
          if ConstrSet.mem c s then
            R.errorf "Duplicate constructor name '%s' at %a"
              c
              Location.pp loc
          else
            build (ConstrSet.add c s) cs
      in
      build ConstrSet.empty constrs

  let rec check env expected = function
    | Ast.{ data = E_cons cnm; loc } ->
      (match expected with
       | EnumSort constrs when ConstrSet.mem cnm constrs ->
         Ok (E_cons cnm)
       | _ ->
         R.errorf
           "This constructor does not fit into expected sort at %a"
           Location.pp loc)
    | Ast.{ data = E_table { cols; rows }; loc = _ } ->
      R.traverse (synthesise env) cols
      >>= fun col_infos ->
      let cols, col_sorts = List.split col_infos in
      R.traverse (check_clause env col_sorts expected) rows
      >>= fun rows ->
      Ok (E_table { cols; rows })
    | expr ->
      synthesise env expr
      >>= fun (checked_expr, computed) ->
      compare_types expr.Ast.loc expected computed
      >>= fun () ->
      Ok checked_expr

  and check_clause env col_sorts expected = function
    | Ast.{ data = { pattern; expr }; loc = _ } ->
      (let loc = pattern.Ast.loc in
       check_pattern pattern col_sorts >>= fun (pattern, rcols) ->
       match rcols with
       | [] ->
         check env expected expr >>= fun expr ->
         Ok { pattern; expr }
       | _::_ ->
         R.errorf "Pattern arity mismatch at %a"
           Location.pp loc)

  and synthesise env = function
    | Ast.{ data = E_cons _; loc } ->
      R.errorf "Unable to synthesise type for constructor at %a"
        Location.pp loc
    | Ast.{ data = E_int i; loc = _ } ->
      Ok (E_int i, AbstrSort L.integer_sort)
    | Ast.{ data = E_string s; loc = _ } ->
      Ok (E_string s, AbstrSort L.string_sort)
    | Ast.{ data = E_name nm; loc } ->
      (match Hashtbl.find env nm with
       | exception Not_found ->
         R.errorf "Name '%s' not defined at %a"
           nm
           Location.pp loc
       | sort ->
         Ok (E_name nm, sort))
    | Ast.{ data = E_func (fnm, arguments); loc } ->
      (match L.function_of_string fnm with
       | Error () ->
         R.errorf
           "Function symbol '%s' not understood at %a"
           fnm
           Location.pp loc
       | Ok fsymbol ->
         let {arg_sorts; ret_sort} = L.function_scheme fsymbol in
         check_arguments env [] arguments arg_sorts
         >>= fun arguments ->
         Ok (E_func (fsymbol, arguments), ret_sort))
    | Ast.{ data = E_table { cols = _; rows = _ }; loc } ->
      R.errorf
        "Not synthesising from a table at %a"
        Location.pp loc

  and check_arguments env accum args sorts =
    match args, sorts with
    | [], [] ->
      Ok (List.rev accum)
    | Ast.{ data = A_one expr; loc = _ }::args, `S sort::sorts ->
      check env sort expr >>= fun expr ->
      check_arguments env (A_one expr::accum) args sorts
    | Ast.{ data = A_many exprs; loc = _ }::args, `M sort::sorts ->
      R.traverse (check env sort) exprs >>= fun exprs ->
      check_arguments env (A_many exprs::accum) args sorts
    | _ ->
      R.errorf "Argument mismatch"

  and check_pattern pat cols =
    match pat, cols with
    | _, [] ->
      R.errorf "Nothing to match!"
    | Ast.{ data = P_cons cnm; loc }, EnumSort cnms::cols ->
      if ConstrSet.mem cnm cnms then
        Ok (P_cons cnm, cols)
      else
        R.errorf "Pattern '%s' will never match at %a"
          cnm
          Location.pp loc
    | Ast.{ data = P_cons cnm; loc }, AbstrSort _::_ ->
      R.errorf
        "Attempting to match constructor '%s' against abstract type at %a"
        cnm
        Location.pp loc
    | Ast.{ data = P_any; loc = _ }, _::cols ->
      Ok (P_any, cols)
    | Ast.{ data = P_string str; loc }, sort::cols ->
      (match sort with
       | AbstrSort s when L.Base_Sort.equal s L.string_sort ->
         Ok (P_string str, cols)
       | AbstrSort _ | EnumSort _ ->
         R.errorf
           "Attempting to match a string literal against non string sort at %a"
           Location.pp loc)
    | Ast.{ data = P_seq pats; loc = _ }, cols ->
      let rec seq accum pats cols =
        match pats with
        | [] ->
          Ok (P_seq (List.rev accum), cols)
        | pat::pats ->
          check_pattern pat cols >>= fun (p, cols) ->
          seq (p::accum) pats cols
      in
      seq [] pats cols
    | Ast.{ data = P_or []; loc = _}, _ ->
      failwith "internal error: empty or pattern"
    | Ast.{ data = P_or (pat::pats); loc }, cols ->
      check_pattern pat cols >>= fun (p, rcols) ->
      let rec check_pats accum = function
        | [] ->
          Ok (P_or (p::List.rev accum), rcols)
        | pat::pats ->
          check_pattern pat cols >>= fun (p, rcols') ->
          if rcols = rcols' then
            check_pats (p::accum) pats
          else
            R.errorf "Pattern length mismatch in 'or' pattern at %a"
              Location.pp loc
      in
      check_pats [] pats

  let check_program program main_sort =
    let defined  = Hashtbl.create 20 in
    let required = Hashtbl.create 20 in
    let env      = Hashtbl.create 20 in
    let check_decl Ast.{ data = { name = { data = name; loc }; sort; defn }; _ } =
      if Hashtbl.mem env name then
        R.errorf "Name '%s' defined multiple times at %a"
          name
          Location.pp loc
      else
        check_sort sort
        >>= fun sort ->
        Hashtbl.add env name sort;
        match defn with
        | None ->
          Hashtbl.add required name sort;
          Ok ()
        | Some expr ->
          check env sort expr
          >>= fun expr ->
          Hashtbl.add defined name expr;
          Ok ()
    in
    R.iter check_decl program >>= fun () ->
    match Hashtbl.find env "main" with
    | exception Not_found ->
      Error "no 'main' defined"
    | sort ->
      (* FIXME: get the right location *)
      compare_types Location.generated main_sort sort >>= fun () ->
      Ok { defined; required }
end
