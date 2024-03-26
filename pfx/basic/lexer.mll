{
  open Parser

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
  | _ as c                  { failwith (Printf.sprintf "Illegal character '%c': " c) }