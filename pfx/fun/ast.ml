type command =
  | Push of int
  | Pop
  | Swap
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Exec
  | Get
  | DoExec of command list

type program = int * command list

(* Add here all useful functions and types related to the AST, for instance, string_of_ functions *)

let rec string_of_command = function
  | Push n -> "Push " ^ string_of_int n
  | Pop -> "Pop"
  | Swap -> "Swap"
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Rem -> "Rem"
  | Exec -> "Exec"
  | Get -> "Get"
  | DoExec(instruc)-> "(" ^ String.concat " " (List.map string_of_command instruc) ^ ")"



let string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds)

