%{
  open Ast
  open BinOp
%}

%token EOF PLUS MINUS TIMES DIV MOD LPAR RPAR LET EQUAL IN
%token <int> INT
%token <string> IDENT
(* For function support *)
%token FUN RA

%start < Ast.expression > expression

(* For function support *)
%left FUN
%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS

%%

expression:
 | e=expr EOF            { e }

expr:
  | MINUS e=expr %prec UMINUS  { Uminus e }
  | e1=expr o=bop e2=expr      { Binop(o,e1,e2) }
  | e=simple_expr              { e }
  (* For function support *)
  | FUN id=IDENT RA e=expr %prec FUN   { Fun(id,e) }
  | e1=simple_expr e2=simple_expr      { App(e1,e2) }
  (*Modification For Exercice 11 to be able to 
  support let definition*)
  | LET id=IDENT EQUAL e1=expr IN e2=expr { App(Fun(id, e2), e1) }


simple_expr:
  | LPAR e=expr RPAR           { e }
  | id=IDENT                   { Var id }
  | i=INT                      { Const i }

%inline bop:
  | MINUS     { Bsub }
  | PLUS      { Badd }
  | TIMES     { Bmul }
  | DIV       { Bdiv }
  | MOD       { Bmod }

%%
