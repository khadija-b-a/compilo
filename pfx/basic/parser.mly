%{
  (* Ocaml code here*)

%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)
%token EOF
%token <int> INT
%token PUSH POP SWAP ADD SUB MUL DIV REM


(******************************
 * Entry points of the parser *
 ******************************)

(* enter your %start clause here *)
%start <Ast.program> program

%%

(*************
 * The rules *
 *************)

(* list all rules composing your grammar; obviously your entry point has to be present *)
program: i=INT EOF { i,[] }

cmd_list:
  | /* empty */ { [] }
  | cmd_list cmd { $2 :: $1 }

cmd:
  | PUSH INT { Push($2) }
  | POP      { Pop }
  | SWAP     { Swap }
  | ADD      { Add }
  | SUB      { Sub }
  | MUL      { Mul }
  | DIV      { Div }
  | REM      { Rem }
  ;

%%
