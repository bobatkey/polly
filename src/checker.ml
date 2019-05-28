type 'sort poly_sort =
  | V of string
  | B of 'sort

type 'sort arg_sort =
  { multiplicity : [ `Single | `Multiple ]
  ; sort         : 'sort poly_sort
  }

type 'sort constructor_sort =
  { arg_sorts : 'sort arg_sort list
  ; ret_sort  : 'sort poly_sort
  }

module Sorts = struct
  type 'sort t = 'sort constructor_sort

  let return s =
    { arg_sorts = []
    ; ret_sort  = s
    }

  let ( @--> ) sort c =
    { c with arg_sorts = { multiplicity = `Single; sort } :: c.arg_sorts }

  let ( @*-> ) sort c =
    { c with arg_sorts = { multiplicity = `Multiple; sort } :: c.arg_sorts }
end

module type LANGUAGE = sig
  type constructor

  type sort

  val sort_of_string : string -> (sort, unit) result

  val string_of_sort : sort -> string

  val string_sort : sort

  val integer_sort : sort

  val constructor_of_string : string -> (constructor, unit) result

  val sort_of_constructor : constructor -> sort constructor_sort

  module Sort : sig
    type t = sort

    val equal : t -> t -> bool
  end
end

module type CHECKER = sig
  module L : LANGUAGE

  type expr = private
    | E_name of string
    | E_cons of { constructor : L.constructor
                ; arguments   : argument list
                }
    | E_string of string
    | E_int    of int

  and argument = private
    | A_single of expr
    | A_list   of expr list

  type program =
    { defined  : (string, expr) Hashtbl.t
    ; required : (string, L.sort) Hashtbl.t
    }

  val check_program : Ast.program -> L.sort -> (program, string) result
end

(* A plan for strictness / laziness and control flow
   - During elaboration, we describe how to elaborate into primitive
     control flow operations.
   - Ultimately split into:
     - Basic control flow ('if', and maybe 'switch')
       - lazy in their
     - Pure functions (strict in their arguments)
       - arguments are executed in parallel
     - IO functions, which will require asynchronous operation.
*)

module Make (L : LANGUAGE) : CHECKER with module L = L = struct
  module L = L

  type identifier =
    string

  type expr =
    | E_name   of identifier
    | E_cons   of { constructor : L.constructor
                  ; arguments   : argument list
                  }
    | E_string of string
    | E_int    of int

  and argument =
    | A_single of expr
    | A_list   of expr list

  type program =
    { defined  : (identifier, expr) Hashtbl.t
    ; required : (identifier, L.sort) Hashtbl.t
    }

  type inf_sort =
    | MV of L.sort option Unionfind.equiv_class
    | Ba of L.sort

  let instantiate { arg_sorts; ret_sort } =
    let inst = Hashtbl.create 10 in
    let inst_sort = function
      | V nm ->
        (try MV (Hashtbl.find inst nm)
         with Not_found ->
           let c = Unionfind.make_class None in
           Hashtbl.add inst nm c;
           MV c)
      | B s ->
        Ba s
    in
    let inst_arg_sort { multiplicity; sort } =
      (multiplicity, inst_sort sort)
    in
    let arg_sorts = List.map inst_arg_sort arg_sorts in
    let ret_sort  = inst_sort ret_sort in
    arg_sorts, ret_sort

  open R.Infix

  let sort_mismatch sort1 sort2 =
    R.errorf "sort mismatch: %s is not equal to %s"
      (L.string_of_sort sort1)
      (L.string_of_sort sort2)

  let unify sort1 sort2 =
    match sort1, sort2 with
    | MV mv1, MV mv2 -> begin
        if Unionfind.equiv mv1 mv2 then
          Ok ()
        else match Unionfind.find mv1, Unionfind.find mv2 with
          | None, None ->
            Unionfind.union mv1 mv2; Ok ()
          | None, Some s | Some s, None ->
            Unionfind.union mv1 mv2;
            Unionfind.set mv1 (Some s);
            Ok ()
          | Some s1, Some s2 ->
            if L.Sort.equal s1 s2 then
              (Unionfind.union mv1 mv2; Ok ())
            else
              sort_mismatch s1 s2
      end
    | MV mv, Ba s1 | Ba s1, MV mv -> begin
        match Unionfind.find mv with
        | None ->
          Unionfind.set mv (Some s1); Ok ()
        | Some s2 ->
          if L.Sort.equal s1 s2 then
            Ok ()
          else
            sort_mismatch s1 s2
      end
    | Ba s1, Ba s2 ->
      if L.Sort.equal s1 s2 then
        Ok ()
      else
        sort_mismatch s1 s2

  let rec check env = function
    | Ast.{ data = E_name nm; loc } -> begin
        match Hashtbl.find env nm with
        | exception Not_found ->
          R.errorf "Name '%s' not defined at %a"
            nm
            Location.pp loc
        | sort ->
          Ok (E_name nm, Ba sort)
      end

    | Ast.{ data = E_cons { constructor; arguments }; loc } -> begin
        match L.constructor_of_string constructor with
        | Error () ->
          R.errorf "Symbol '%s' not understood at %a"
            constructor
            Location.pp loc
        | Ok constructor ->
          let arg_sorts, ret_sort =
            instantiate (L.sort_of_constructor constructor)
          in
          check_arguments env [] arguments arg_sorts >>= fun arguments ->
          Ok (E_cons { constructor; arguments }, ret_sort)
      end

    | Ast.{ data = E_string s; loc = _ } ->
      Ok (E_string s, Ba L.string_sort)

    | Ast.{ data = E_int i; loc = _ } ->
      Ok (E_int i, Ba L.integer_sort)

  and check_arguments env checked arguments sorts =
    match arguments, sorts with
    | [], [] ->
      Ok (List.rev checked)

    | arg::arguments, sort::sorts ->
      check_argument env arg sort >>= fun arg ->
      check_arguments env (arg::checked) arguments sorts

    | [], _::_ ->
      Error "not enough parameters"

    | _::_, [] ->
      Error "too many parameters"

  and check_against env expr sort =
    check env expr >>= fun (expr, sort') ->
    unify sort' sort >>= fun () ->
    Ok expr

  and check_argument env argument sort =
    match argument, sort with
    | Ast.A_single e, (`Single, sort) ->
      check_against env e sort >>= fun e ->
      Ok (A_single e)

    | Ast.A_list es, (`Multiple, sort) ->
      check_list env [] es sort >>= fun es ->
      Ok (A_list es)

    | Ast.A_single _, (`Multiple, _) ->
      (* FIXME: could silently promote? *)
      Error "multi-parameter expected"

    | Ast.A_list _, (`Single, _) ->
      Error "single parameter expected"

  and check_list env checked exprs sort =
    match exprs with
    | [] ->
      Ok (List.rev checked)
    | expr::exprs ->
      check_against env expr sort >>= fun expr ->
      check_list env (expr::checked) exprs sort

  let check_sort Ast.{ ident; loc } =
    match L.sort_of_string ident with
    | Ok sort ->
       Ok sort
    | Error () ->
       R.errorf "Sort name '%s' not recognised at %a"
         ident
         Location.pp loc

  let check_program program main_sort =
    let defined  = Hashtbl.create 20 in
    let required = Hashtbl.create 20 in
    let env      = Hashtbl.create 20 in
    let check_decl Ast.{ name; sort; defn } =
      if Hashtbl.mem env name then
        R.errorf "Name '%s' defined multiple times" name
      else
        check_sort sort
        >>= fun sort ->
        Hashtbl.add env name sort;
        match defn with
          | None ->
             Hashtbl.add required name sort;
             Ok ()
          | Some expr ->
             check_against env expr (Ba sort)
             >>= fun expr ->
             Hashtbl.add defined name expr;
             Ok ()
    in
    R.iter check_decl program >>= fun () ->
    match Hashtbl.find env "main" with
      | exception Not_found ->
         Error "no 'main' defined"
      | sort when not (L.Sort.equal sort main_sort) ->
         Error "main has wrong sort"
      | _ ->
         Ok { defined; required }
end
