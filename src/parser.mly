%{
open Ast
%}

%token <string> LC_IDENT
%token <string> UC_IDENT
%token <int> INTLIT
%token <string> STRINGLIT
%token LBRACE RBRACE
%token LPAREN RPAREN
%token COLON COMMA
%token UNDERSCORE ARROW PIPE
%token EOF

%token KW_EXTERN KW_DEFINE KW_AS
%token KW_TABLE KW_END


%start <Ast.program> program

%%

program:
  | decls=list(declaration); EOF { decls }

name:
  | nm=LC_IDENT
    { { data = nm; loc = Location.mk $startpos $endpos } }

declaration:
  | KW_EXTERN; name=name; COLON; sort=sort /* FIXME: attributes? */
    { { data = { name; sort; defn = None }
      ; loc = Location.mk $startpos $endpos } }
  | KW_DEFINE; name=name; COLON; sort=sort; KW_AS; expr=expr
    { { data = { name; sort; defn = Some expr }
      ; loc  = Location.mk $startpos $endpos } }



sort:
  | ident=LC_IDENT
    { { data = S_name ident; loc = Location.mk $startpos $endpos } }
  | c=UC_IDENT; cs=list(PIPE; e=UC_IDENT {e})
    { { data = S_enum (c::cs); loc = Location.mk $startpos $endpos } }

expr:
  | fname=LC_IDENT; args=argument+
    { { data = E_func (fname, args)
      ; loc  = Location.mk $startpos $endpos } }
  | KW_TABLE; cols=separated_nonempty_list(COMMA,expr); rows=clause+; KW_END
    { { data = E_table { cols; rows }
      ; loc  = Location.mk $startpos $endpos } }
  | e=base_expr
    { e }

base_expr:
  | i=INTLIT
    { { data = E_int i
      ; loc  = Location.mk $startpos $endpos } }
  | s=STRINGLIT
    { { data = E_string s
      ; loc  = Location.mk $startpos $endpos } }
  | cnm=UC_IDENT
    { { data = E_cons cnm; loc = Location.mk $startpos $endpos } }
  | ident=LC_IDENT
    { { data = E_name ident
      ; loc  = Location.mk $startpos $endpos } }
  | LPAREN; e=expr; RPAREN
    { e }

argument:
  | e=base_expr
    { { data = A_one e
      ; loc  = Location.mk $startpos $endpos } }

  | LBRACE; es=separated_list(COMMA,expr); RBRACE
    { { data = A_many es
      ; loc  = Location.mk $startpos $endpos } }

clause:
  | PIPE; pattern=pattern; ARROW; expr=expr
    { { data = { pattern; expr }
      ; loc  = Location.mk $startpos $endpos } }

pattern:
  | pats=separated_nonempty_list(COMMA,pattern1)
    { { data = P_seq pats; loc = Location.mk $startpos $endpos } }

pattern1:
  | pats=separated_nonempty_list(PIPE,pattern0)
    { { data = P_or pats; loc = Location.mk $startpos $endpos } }

pattern0:
  | ident=UC_IDENT
    { { data = P_cons ident; loc = Location.mk $startpos $endpos } }
  | str=STRINGLIT
    { { data = P_string str; loc = Location.mk $startpos $endpos } }
  | UNDERSCORE
    { { data = P_any; loc = Location.mk $startpos $endpos } }
  | LPAREN; pat=pattern; RPAREN
    { pat }
