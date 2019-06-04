type sort =
  { ident : string
  ; loc   : Location.t
  }

(* FIXME: predicate patterns *)
type pattern =
  | P_cons of string
  | P_any
  | P_or of pattern list

type expr_data =
  | E_name   of string
  | E_cons   of { constructor : string
                ; arguments   : argument list
                }
  | E_string of string
  | E_int    of int
  | E_table  of { columns : expr list
                ; rows    : clause list
                }

and expr =
  { data : expr_data
  ; loc  : Location.t
  }

and argument =
  | A_single of expr
  | A_list   of expr list

and clause =
  { patterns : pattern list
  ; expr     : expr
  ; location : Location.t
  }

type entity =
  { name : string
  ; sort : sort
  ; defn : expr option
  }

type program =
  entity list
