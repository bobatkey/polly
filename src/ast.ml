type 'a with_location =
  { data : 'a
  ; loc  : Location.t
  }


type sort_data =
  | S_name of string
  | S_enum of string list

and sort =
  sort_data with_location



type constructor_symbol =
  string

type function_symbol =
  string


type pattern_data =
  | P_cons     of constructor_symbol
  | P_any
  | P_seq      of pattern list
  | P_or       of pattern list
  | P_string   of string
  | P_anywhere of pattern

and pattern =
  pattern_data with_location


type expr =
  expr_data with_location

and clause =
  clause_data with_location

and argument =
  argument_data with_location

and expr_data =
  | E_int    of int
  | E_string of string
  | E_cons   of constructor_symbol
  | E_name   of string
  | E_func   of function_symbol * argument list
  | E_table  of { cols : expr list; rows : clause list }

and clause_data =
  { pattern : pattern
  ; expr    : expr
  }

and argument_data =
  | A_one  of expr
  | A_many of expr list



type definition =
  | Val of
      { name : string with_location
      ; sort : sort
      ; defn : expr option
      }
  | Sort of
      { name    : string with_location
      ; constrs : string list
      }

type program =
  definition with_location list
