
%{
  (* Ocaml code here*)

%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)
%token EOF
%token ADD SUB MUL DIV REM POP SWAP EXEC GET LPAREN RPAREN
%token <int> INT
%token <int> PUSH 
(*The different Token were added : LPAREN, RPAREN, GET and EXEC *)

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
program: i=INT  q=instruction_seq  EOF { i, q }

instruction_seq : 
  | { [] }
  | instr=instruction instrs=instruction_seq { instr :: instrs } 

instruction : 
  | ADD        { Add }
  | SUB        { Sub }
  | MUL        { Mul }
  | DIV        { Div }
  | REM        { Rem }
  | POP        { Pop }
  | SWAP       { Swap }
  | PUSH n=INT { Push n }
  (*Changes for Exercice 9 implementation*)
  | EXEC       { Exec }
  | GET        { Get }
  | LPAREN instruc=instruction_seq RPAREN {DoExec(instruc)}
%%
