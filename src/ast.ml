type sort =
  | T_name of string

type expr_data =
  | E_name   of string
  | E_cons   of { constructor : string
                ; arguments   : argument list
                }
  | E_string of string
  | E_int    of int

and expr =
  { data : expr_data
  ; loc  : Location.t
  }

and argument =
  | A_single of expr
  | A_list   of expr list

type entity =
  { name : string
  ; sort : sort
  ; defn : expr option
  }

type program =
  entity list
