open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  | [], _ -> Error("Nothing to step",state)
  (* Valid configurations *)
  | Push n :: q, stack -> Ok (q, n :: stack)
  | Pop :: q, _ :: stack -> Ok (q, stack)
  | Swap :: q, x :: y :: stack -> Ok (q, y :: x :: stack)
  | Add :: q, x :: y :: stack -> Ok (q, (x + y) :: stack)
  | Sub :: q, x :: y :: stack -> Ok (q, (y - x) :: stack)
  | Mul :: q, x :: y :: stack -> Ok (q, (x * y) :: stack)
  | Div :: q, x :: y :: stack ->
      if x = 0 then Error ("Division by zero", state)
      else Ok (q, (y / x) :: stack)
  | Rem :: q, x :: y :: stack ->
      if x = 0 then Error ("Remainder with zero divisor", state)
      else Ok (q, (y mod x) :: stack)
  (* Invalid configurations *)
  | _, _ -> Error ("Invalid configuration", state)

let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
