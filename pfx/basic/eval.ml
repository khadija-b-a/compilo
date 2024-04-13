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
  
  | Pop :: q, stack -> 
  ( match stack with 
    | _ :: s -> Ok (q, s)
    | _ -> Error("Empty stack", state)
  ) 

  | Swap :: q, stack -> 
  ( match stack with 
    | x :: y :: s -> Ok (q, y :: x :: s)
    | _ -> Error("Not enough arguments for SWAP", state)
  )

  | Add :: q, stack ->
    (match stack with
    | x :: y :: s -> Ok (q, (x + y) :: s)
    | _ -> Error("Not enough arguments for ADD", state)
    )

  | Sub :: q, stack ->
  ( match stack with 
    | x :: y :: s -> Ok (q, (x-y) :: s)
    | _ -> Error("Not enough arguments for SUB", state)
  ) 

  | Mul :: q, stack -> 
  ( match stack with 
    | x :: y :: s -> Ok (q, (x*y) :: s)
    | _ -> Error("Not enough arguments for MUL", state)
  )

  | Div :: q, stack ->
    (match stack with
    | x :: y :: s ->
        if x = 0 then Error ("Division by zero", ([], stack))
        else Ok (q, (y / x) :: s)
    | _ -> Error("Not enough arguments for DIV", state)
    )

  | Rem :: q, stack ->
  (match stack with
    | x :: y :: s ->
      if x = 0 then Error ("Remainder with zero divisor", ([], stack))
      else Ok (q, (y mod x) :: s)
    | _ -> Error("Not enough arguments for REM", state))



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
