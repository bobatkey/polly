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
%token EOF

%token KW_EXTERN KW_DEFINE KW_AS

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
  | name=LC_IDENT
    { T_name name }

expr:
  | constructor=LC_IDENT; arguments=nonempty_list(argument)
    { E_cons { constructor; arguments } }

  | e=base_expr
    { e }

base_expr:
  | name=LC_IDENT
    { E_name name }

  | constructor=UC_IDENT
    { E_cons { constructor; arguments = [] } }

  | s=STRINGLIT
    { E_string s }

  | i=INTLIT
    { E_int i }

  | LPAREN; e=expr; RPAREN
    { e }

argument:
  | e=base_expr
    { A_single e }

  | LBRACE; es=separated_list(COMMA,expr); RBRACE
    { A_list es }
