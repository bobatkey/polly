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

  type case_tree =
    | Execute   of int
    | Switch    of int * (string * case_tree) list * case_tree
    | StrSwitch of int * (string * case_tree) list * case_tree
    | Fail

  type expr =
    | E_string of string
    | E_int    of int
    | E_name   of string
    | E_cons   of string
    | E_func   of L.function_symbol * argument list
    | E_table  of { cols : expr list; tree : case_tree; cases : expr list }

  and argument =
    | A_one  of expr
    | A_many of expr list

  type program = private
    { defined  : (string, expr) Hashtbl.t
    ; required : (string, sort) Hashtbl.t
    }

  val check_program : Ast.program -> sort -> (program, string) result
end

module Make (L : LANGUAGE) (* : CHECKER with module L = L*)  = struct
  module L = L

  type base_sort = L.base_sort
  type nonrec sort = base_sort sort

  type constructor_symbol =
    string

  type identifier =
    string

  type path = int list

  type case_tree =
    | Execute   of int
    | Switch    of path * (string * case_tree) list
    | StrSwitch of path * (string * case_tree) list
    | Catch     of case_tree * case_tree
    | Seq       of case_tree * case_tree
    | Unit
    | Fail

  type expr =
    | E_string of string
    | E_int    of int
    | E_name   of string
    | E_cons   of string
    | E_func   of L.function_symbol * argument list
    | E_table  of { cols : expr list; tree : case_tree; cases : expr list }

  and argument =
    | A_one  of expr
    | A_many of expr list

  type program =
    { defined  : (string, expr) Hashtbl.t
    ; required : (string, sort) Hashtbl.t
    }

  open R.Infix

  let append_all xss yss =
    xss
    |> List.map (fun xs -> yss |> List.map (fun ys -> xs @ ys))
    |> List.concat

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

  type pat_type =
    | Tup of pat_type list
    | Sin of sort

  type pat =
    | P_cons  of constructor_symbol
    | P_str   of string
    | P_any
    | P_or    of pat list
    | P_tuple of pat list

  let combine_opt xs ys =
    let rec combine acc xs ys =
      match xs, ys with
      | [],    []     -> Some (List.rev acc)
      | x::xs, y::ys  -> combine ((x,y)::acc) xs ys
      | [], _ | _, [] -> None
    in
    combine [] xs ys

  let rec check_pattern pattern sort =
    match pattern, sort with
    | Ast.{ data = P_any; loc = _ }, _ ->
      Ok P_any
    | Ast.{ data = P_cons cnm; loc = _ }, Sin (EnumSort cnms)
      when ConstrSet.mem cnm cnms ->
      Ok (P_cons cnm)
    | Ast.{ data = P_string str; loc = _ }, Sin (AbstrSort sort)
      when L.Base_Sort.equal sort L.string_sort ->
      Ok (P_str str)
    | Ast.{ data = P_seq pats; loc }, Tup sorts ->
      (match combine_opt pats sorts with
       | None ->
         R.errorf "Length mismatch in pattern at %a" Location.pp loc
       | Some pat_sorts ->
         R.traverse (fun (p,s) -> check_pattern p s) pat_sorts
         >>= fun pats ->
         Ok (P_tuple pats))
    | Ast.{ data = P_or pats; loc = _ }, sort ->
      R.traverse (fun p -> check_pattern p sort) pats
      >>= fun pats ->
      Ok (P_or pats)
    | Ast.{ data = P_cons _ | P_string _ ; loc }, _
    | Ast.{ data = P_seq _; loc }, Sin _ ->
      R.errorf "Pattern ill-typed at %a"
        Location.pp loc

  module ConstrMap = struct
    include Map.Make (String)
    let find k m = match find k m with
      | exception Not_found -> []
      | l -> l
    let add k v m =
      let vs = find k m in
      add k (v::vs) m
  end

  module StringLitMap = struct
    include Map.Make (String)
    let find k m = match find k m with
      | exception Not_found -> []
      | l -> l
    let add k v m =
      let vs = find k m in
      add k (v::vs) m
  end

  let rec chunk = function
    | [] ->
      []
    | ([], _)::_ ->
      invalid_arg "internal: empty pattern"
    | (P_any::_, _)::_ as patterns ->
      let rec gather accum = function
        | ([], _)::_ ->
          invalid_arg "internal: empty pattern1"
        | (P_any::pats, i)::patterns ->
          gather ((pats, i)::accum) patterns
        | ([] | (_::_, _)::_) as patterns ->
          `Shift (List.rev accum)::chunk patterns
      in
      gather [] patterns
    | ((P_cons _)::_, _)::_ as patterns ->
      let rec gather accum = function
        | ([], _)::_ ->
          invalid_arg "internal: empty pattern2"
        | (P_cons cnm::pats, i)::patterns ->
          gather (ConstrMap.add cnm (pats, i) accum) patterns
        | ([] | (_::_, _)::_) as patterns ->
          let clauses =
            ConstrMap.fold
              (fun cnm patterns -> List.cons (cnm, List.rev patterns))
              accum
              []
          in
          `ConsSwitch clauses::chunk patterns
      in
      gather ConstrMap.empty patterns
    | ((P_str _)::_, _)::_ as patterns ->
      let rec gather accum = function
        | ([], _)::_ ->
          invalid_arg "internal: empty pattern3"
        | (P_str str::pats, i)::patterns ->
          gather (StringLitMap.add str (pats, i) accum) patterns
        | ([] | (_::_, _)::_) as patterns ->
          let clauses =
            StringLitMap.fold
              (fun str patterns ->
                 List.cons (str, List.rev patterns))
              accum
              []
          in
          `StrSwitch clauses::chunk patterns
      in
      gather StringLitMap.empty patterns
    | ((P_tuple ps)::_, _)::_ as patterns ->
      let width = List.length ps in
      let rec gather accum = function
        | ([], _)::_ ->
          invalid_arg "internal: empty pattern"
        | (P_tuple ps::pats, i)::patterns ->
          gather ((ps @ pats, i)::accum) patterns
        | ([] | (_::_, _)::_) as patterns ->
          `Tuple (width, List.rev accum)::chunk patterns
      in
      gather [] patterns
    | ((P_or ps::pats, i))::patterns ->
       `Or (ps, pats, i)::chunk patterns

  let rec compile_patterns paths patterns =
    match paths, patterns with
    | _, []               -> Fail
    | [], ([], Some i)::_ -> Execute i
    | [], ([], None)::_   -> Unit
    | [], _               -> invalid_arg "missing pattern"
    | path::paths, patterns ->
      let compile_chunk = function
        | `Shift patterns ->
          compile_patterns paths patterns
        | `ConsSwitch clauses ->
          let clauses =
            List.map
              (fun (cnm,patterns) -> cnm, compile_patterns paths patterns)
              clauses
          in
          Switch (List.rev path, clauses)
        | `StrSwitch clauses ->
          let clauses =
            List.map
              (fun (str,patterns) -> str, compile_patterns paths patterns)
              clauses
          in
          StrSwitch (List.rev path, clauses)
        | `Tuple (width, patterns) ->
          let paths = List.init width (fun i -> i::path) @ paths in
          compile_patterns paths patterns
        | `Or (alt_pats, more_pats, i) ->
          let guard =
            compile_patterns [path] (List.map (fun p -> ([p],None)) alt_pats)
          and rest =
            compile_patterns paths [more_pats, i]
          in
          Seq (guard, rest)
      in
      List.fold_right
        (fun chunk cont -> Catch (compile_chunk chunk, cont))
        (chunk patterns)
        Fail

  let compile_patterns patterns =
    compile_patterns [[]] patterns

  (* Augustsson-style coverage checking
     - Phrased as a problem    x covering {p1, p2, ...}
     - Has Conor muddled this up with Coquand's pattern matching for dependent
       types?
     - Seems similar to Maranget's thing too, but his has a way of not necessarily
       expanding each wildcard test case, and allows starting with an arbitrary
       pattern on the LHS.

        x  covering { (_, true), (true, false) }
    =>  (x,y) covering { (_, true), (true, false) }
        [ SPLIT ON y ]
    =>  (x,true) covering { (_, true) }  [DONE]
        (x,false) covering { (true, false) }
        [ SPLIT ON x ] (violates left-to-right evaluation!?)
    =>  (true,false) covering { (true, false) } [ DONE ]
        (false,false) covering { }  [ FAILED ]

    This enables exhaustivity checking by checking to see whether we ever end up
    with an example of a term that cannot be matched. *)


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
      let pat_sort = Tup (List.map (fun x -> Sin x) col_sorts) in
      (* FIXME: coverage checking *)
      R.traverse (check_clause env pat_sort expected) rows
      >>= fun rows ->
      let patterns, cases =
        rows
        |> List.mapi (fun i (pattern, expr) -> (([pattern], Some i), expr))
        |> List.split
      in
      let tree = compile_patterns patterns in
      Ok (E_table { cols; tree; cases })
    | expr ->
      synthesise env expr
      >>= fun (checked_expr, computed) ->
      compare_types expr.Ast.loc expected computed
      >>= fun () ->
      Ok checked_expr

  and check_clause table col_sorts expected = function
    | Ast.{ data = { pattern; expr }; loc = _ } ->
      check_pattern pattern col_sorts >>= fun pattern ->
      check table expected expr >>= fun expr ->
      Ok (pattern, expr)

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

  let enumsort loc =
    let rec build s = function
      | [] ->
        Ok (EnumSort s)
      | c::cs ->
        if ConstrSet.mem c s then
          R.errorf "Duplicate constructor name '%s' at %a"
            c
            Location.pp loc
        else
          build (ConstrSet.add c s) cs
    in
    build ConstrSet.empty

  let check_sort sorts = function
    | Ast.{ data = S_name nm; loc } ->
      (match Hashtbl.find sorts nm with
       | exception Not_found ->
         (match L.base_sort_of_string nm with
          | Error () ->
            R.errorf "Named sort '%s' not recognised at %a"
              nm
              Location.pp loc
          | Ok sort ->
            Ok (AbstrSort sort))
       | sort ->
         Ok sort)
    | Ast.{ data = S_enum constrs; loc } ->
      enumsort loc constrs



  let check_program program main_sort =
    let defined  = Hashtbl.create 20 in
    let sorts    = Hashtbl.create 20 in
    let required = Hashtbl.create 20 in
    let env      = Hashtbl.create 20 in
    let check_decl = function
      | Ast.{ data = Val { name = { data = name; loc }; sort; defn }; _ } ->
        (if Hashtbl.mem env name then
          R.errorf "Name '%s' defined multiple times at %a"
            name
            Location.pp loc
        else
          check_sort sorts sort
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
            Ok ())
      | Ast.{ data = Sort { name = { data = name; loc }; constrs }; loc = loc2 } ->
        (if Hashtbl.mem sorts name then
           R.errorf "Sort name '%s' defined for a second time at %a"
             name
             Location.pp loc
         else
           enumsort loc2 constrs >>= fun sort ->
           Hashtbl.add sorts name sort;
           Ok ())
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
