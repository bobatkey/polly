{
open Parser

type token = Parser.token
}

let white    = [' ' '\t']+
let lc_ident = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '?']*
let uc_ident = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '?']*
let digit    = ['0'-'9']

rule token = parse
| white               { token lexbuf }
| '\n'                { Lexing.new_line lexbuf; token lexbuf }
| "//"                { comment lexbuf }
| "("                 { LPAREN }
| ")"                 { RPAREN }
| "{"                 { LBRACE }
| "}"                 { RBRACE }
| ":"                 { COLON }
| ","                 { COMMA }
| "external"          { KW_EXTERN }
| "define"            { KW_DEFINE }
| "as"                { KW_AS }
| "table"             { KW_TABLE }
| "end"               { KW_END }
| '|'                 { PIPE }
| '_'                 { UNDERSCORE }
| "->"                { ARROW }
| lc_ident            { LC_IDENT (Lexing.lexeme lexbuf) }
| uc_ident            { UC_IDENT (Lexing.lexeme lexbuf) }
| "\""                { let b = Buffer.create 10 in stringlit b lexbuf }
| ('-'|'+'|"")digit+  { INTLIT (int_of_string (Lexing.lexeme lexbuf)) }
| eof                 { EOF }

and stringlit b = parse
| '\"'                { (STRINGLIT (Buffer.contents b)) }
| '\\''\"'            { Buffer.add_char b '\"'; stringlit b lexbuf }
| '\\''\\'            { Buffer.add_char b '\\'; stringlit b lexbuf }
| '\n'                { Lexing.new_line lexbuf;
                        Buffer.add_char b '\n';
                        stringlit b lexbuf }
| [^'\\''\"''\n']*    { Buffer.add_string b (Lexing.lexeme lexbuf);
                        stringlit b lexbuf }

and comment = parse
| '\n'                { Lexing.new_line lexbuf; token lexbuf }
| _                   { comment lexbuf }
| eof                 { EOF }
