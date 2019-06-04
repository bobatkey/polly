%{
open Ast
%}

%token <string> LC_IDENT
%token <string> UC_IDENT
%token <int> INTLIT
%token <string> STRINGLIT
%token LPAREN LBRACE
%token RPAREN RBRACE
%token COLON COMMA
%token UNDERSCORE ARROW PIPE
%token EOF

%token KW_EXTERN KW_DEFINE KW_AS
%token KW_TABLE KW_END


%start <Ast.program> program

%%

program:
  | decls=list(declaration); EOF { decls }

declaration:
  | KW_EXTERN; name=LC_IDENT; COLON; sort=type_expr /* FIXME: attributes? */
    { { name; sort; defn = None } }
  | KW_DEFINE; name=LC_IDENT; COLON; sort=type_expr; KW_AS; expr=expr
    { { name; sort; defn = Some expr } }

type_expr:
  | ident=LC_IDENT
    { { ident; loc = Location.mk $startpos $endpos } }

expr:
  | constructor=LC_IDENT; arguments=nonempty_list(argument)
    { { data = E_cons { constructor; arguments }
      ; loc  = Location.mk $startpos $endpos } }

  | KW_TABLE; columns=separated_nonempty_list(COMMA,expr); rows=clause+; KW_END
    { { data = E_table { columns; rows }
      ; loc  = Location.mk $startpos $endpos } }

  | e=base_expr
    { e }

base_expr:
  | name=LC_IDENT
    { { data = E_name name
      ; loc  = Location.mk $startpos $endpos } }

  | constructor=UC_IDENT
    { { data = E_cons { constructor; arguments = [] }
      ; loc  = Location.mk $startpos $endpos } }

  | s=STRINGLIT
    { { data = E_string s
      ; loc  = Location.mk $startpos $endpos } }

  | i=INTLIT
    { { data = E_int i
      ; loc  = Location.mk $startpos $endpos } }

  | LPAREN; e=expr; RPAREN
    { e }

argument:
  | e=base_expr
    { A_single e }

  | LBRACE; es=separated_list(COMMA,expr); RBRACE
    { A_list es }

clause:
  | PIPE; patterns=separated_nonempty_list(COMMA,pattern); ARROW; expr=expr
    { { patterns; expr; location = Location.mk $startpos $endpos } }

pattern:
  | ident=UC_IDENT
    { P_cons ident }
  | UNDERSCORE
    { P_any }
  | LPAREN; pats=separated_nonempty_list(PIPE,pattern); RPAREN
    { P_or pats }
