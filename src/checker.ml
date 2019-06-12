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

  type pattern_bit =
    | P_cons   of constructor_symbol
    | P_any
    | P_string of string

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
(*
  type pat_type =
    | Columns of pat_type list
    | Column  of sort
*)
(* matrix
       v1, v2, v3
   | ( A,  B
     | B,  C), D -> foop
*)

  (*
  let rec check_and_flatten_pattern pattern pat_sort =
    match pattern, pat_sort with
    | Ast.{ data = P_cons cnm; loc = _ }, Column (EnumSort cnms)
      when ConstrSet.mem cnm cnms ->
      Ok [[P_cons cnm]]
    | Ast.{ data = P_string str; loc = _ }, Column (AbstrSort sort)
      when L.Base_Sort.equal sort L.string_sort ->
      Ok [[P_string str]]
    | Ast.{ data = P_any; loc = _ }, Column _ ->
      Ok [[P_any]]
    | Ast.{ data = P_cons _ | P_string _ | P_any; loc }, _ ->
      R.errorf "Pattern ill-typed at %a"
        Location.pp loc
    | Ast.{ data = P_seq ps; loc }, Columns cols ->
      let rec loop ps cols =
        match ps, cols with
        | [], [] -> Ok [[]]
        | p::ps, c::cs ->
          check_and_flatten_pattern p cs >>= fun p' ->
          loop ps cs >>= fun
*)

  (* FIXME: This should not auto flatten sequences: cols is either a
     single sort, or a tuple of sorts. There is no left over cols. *)
  let rec check_and_flatten_pattern cols i pattern =
    if i >= Array.length cols then
      R.errorf "Pattern too long at %a" Location.pp pattern.Ast.loc
    else
      match pattern with
      | Ast.{ data = P_cons cnm; loc } ->
        (match cols.(i) with
         | EnumSort cnms when ConstrSet.mem cnm cnms ->
           Ok ([[P_cons cnm]], i+1)
         | _sort ->
           R.errorf "Type error in pattern at %a"
             Location.pp loc)
      | Ast.{ data = P_any; loc = _ } ->
        Ok ([[P_any]], i+1)
      | Ast.{ data = P_string str; loc } ->
        (match cols.(i) with
         | AbstrSort sort when L.Base_Sort.equal sort L.string_sort ->
           Ok ([[P_string str]], i+1)
         | _sort ->
           R.errorf "Type error in pattern at %a"
             Location.pp loc)
      | Ast.{ data = P_seq ps; loc = _ } ->
        let rec loop i = function
          | [] ->
            Ok ([[]], i)
          | p::ps ->
            check_and_flatten_pattern cols i p
            >>= fun (p1, j) ->
            loop j ps
            >>= fun (p2, k) ->
            Ok (append_all p1 p2, k)
        in
        loop i ps
      | Ast.{ data = P_or []; loc = _ } ->
        failwith "internal error: empty or pattern"
      | Ast.{ data = P_or (p::ps); loc } ->
        check_and_flatten_pattern cols i p >>= fun (p1, j) ->
        let rec loop accum = function
          | [] ->
            Ok (List.concat (List.rev accum), j)
          | p::ps ->
            check_and_flatten_pattern cols i p >>= fun (p, j') ->
            if j = j' then loop (p::accum) ps
            else R.errorf "Pattern length mismatch at %a" Location.pp loc
        in
        loop [p1] ps
(*
      | Ast.{ data = P_bind (pat, _nm); loc } ->
        check_and_flatten_pattern cols i pat >>= fun (ps, j) ->
        if j = i + 1 then
          (* FIXME: actually do the binding, and remember the type *)
          Ok (ps, j)
        else
          R.errorf "Attempting to bind multiple values at %a"
            Location.pp loc
*)

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
    | ((P_string _)::_, _)::_ as patterns ->
      let rec gather accum = function
        | ([], _)::_ ->
          invalid_arg "internal: empty pattern3"
        | (P_string str::pats, i)::patterns ->
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

  let rec compile_patterns n i patterns =
    Printf.eprintf "compile_patterns %d %d\n%!" n i;
    assert (List.for_all (fun (p,_) -> List.length p = n-i) patterns);
    match n-i, patterns with
    | _, []         -> fun x -> x
    | 0, ([], i)::_ -> fun _ -> Execute i
    | 0, _          -> invalid_arg "missing pattern"
    | _, patterns ->
      chunk patterns |> List.fold_right begin function
        | `Shift patterns ->
          compile_patterns n (i+1) patterns
        | `ConsSwitch clauses ->
          let clauses =
            List.map
              (fun (cnm,patterns) ->
                 cnm, compile_patterns n (i+1) patterns Fail)
              clauses
          in
          fun x -> Switch (i, clauses, x)
        | `StrSwitch clauses ->
          let clauses =
            List.map
              (fun (str,patterns) ->
                 str, compile_patterns n (i+1) patterns Fail)
              clauses
          in
          fun x -> StrSwitch (i, clauses, x)
      end

  let compile_patterns n patterns =
    compile_patterns n 0 patterns Fail

  (* Augustsson-style matching (also does coverage checking)
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
      let col_sorts = Array.of_list col_sorts in
      (* FIXME: coverage checking *)
      R.traverse (check_clause env col_sorts expected) rows
      >>= fun rows ->
      let cases = List.map snd rows in
      let patterns =
        List.concat @@
        List.mapi (fun i (patterns, _) ->
            List.map (fun p -> (p,i)) patterns) @@
        rows
      in
      let tree = compile_patterns (Array.length col_sorts) patterns in
      Ok (E_table { cols; tree; cases })
    | expr ->
      synthesise env expr
      >>= fun (checked_expr, computed) ->
      compare_types expr.Ast.loc expected computed
      >>= fun () ->
      Ok checked_expr

  and check_clause table col_sorts expected = function
    | Ast.{ data = { pattern; expr }; loc = _ } ->
      let loc = pattern.Ast.loc in
      check_and_flatten_pattern col_sorts 0 pattern >>= fun (patterns, i) ->
      if i <> Array.length col_sorts then
        R.errorf
          "Pattern does not match all columns at %a"
          Location.pp loc
      else
        check table expected expr >>= fun expr ->
        Ok (patterns, expr)

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
