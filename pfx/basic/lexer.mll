{
  open Parser 

  let print_token = function 
  | EOF   -> print_string "EOF " 
  | ADD   -> print_string "ADD "
  | SUB   -> print_string "SUB "
  | MUL   -> print_string "MUL "
  | DIV   -> print_string "DIV "
  | REM   -> print_string "REM "
  | POP   -> print_string "POP "
  | SWAP  -> print_string "SWAP "
  | PUSH  -> print_string "PUSH "
  | INT n -> print_int n ; print_string " " 
  | LPAREN -> print_string "LPAREN "
  | RPAREN -> print_string "RPAREN "

  let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb)
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']
let operator = ['+' '-' '*' '/']
let parentheses = ['(' ')']

rule token = parse
  (* newlines *)
  | newline { token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* integers *)
  | digit+ as nb           { mk_int nb }
  (* commands  *)
  | "PUSH"      { PUSH }
  | "POP"       { POP }
  | "SWAP"      { SWAP }
  | "ADD"       { ADD }
  | "SUB"       { SUB }
  | "MUL"       { MUL }
  | "DIV"       { DIV }
  | "REM"       { REM }
(* parentheses *)
  | '('        { LPAREN }
  | ')'        { RPAREN }
  (* illegal characters *)
  | _ as c     { failwith (Printf.sprintf "Illegal character '%c': " c) }
