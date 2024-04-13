open Ast
open Printf

type exec_or_int =
  | Int of int
  | Executable of command list 

let string_of_state (cmds, stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (String.concat ";" (List.map (fun x -> match x with | Int i -> string_of_int i | Executable _ -> "Executable") stack)))


(* Question 4.2 *)
let step state =
  match state with
  | [], _ -> Error("Nothing to step",state)
  (* Valid configurations *)
  | Push(n) :: q, stack -> Ok (q, Int n :: stack)
  | Pop :: q, _ :: s -> Ok (q, s)
  | Pop :: q, [] -> Error("Runtime error: Stack underflow with Pop", (q, []))
  | Swap :: q, n1 :: n2 :: s -> Ok (q, n2 :: n1 :: s)
  | Swap :: q, _ -> Error("Runtime error: Not enough elements for Swap", (q, []))
  | Add :: q, Int n1 :: Int n2 :: s -> Ok (q, Int (n2 + n1) :: s)
  | Add :: q, _ -> Error("Runtime error: Not enough elements for Add", (q, []))
  | Sub :: q, Int n1 :: Int n2 :: s -> Ok (q, Int (n1 - n2) :: s)
  | Sub :: q, _ -> Error("Runtime error: Not enough elements for Sub", (q, []))
  | Mul :: q, Int n1 :: Int n2 :: s -> Ok (q, Int (n2 * n1) :: s)
  | Mul :: q, _ -> Error("Runtime error: Not enough elements for Mul", (q, []))
  | Div :: q,Int n1 :: Int n2 :: s when n2 != 0 -> Ok (q, Int (n1 / n2) :: s)
  | Div :: q, _ :: Int 0 :: _ -> Error("Runtime error: Division by zero", (q, []))
  | Div :: q, _ -> Error("Runtime error: Not enough elements for Div", (q, []))
  | Rem :: q, Int n1 :: Int n2 :: s when n2 != 0 -> Ok (q, Int (n1 mod n2) :: s)
  | Rem :: q, _ :: Int 0 :: _ -> Error("Runtime error: Division by zero in Rem", (q, []))
  | Rem :: q, _ -> Error("Runtime error: Not enough elements for Rem", (q, []))
  (*Added code *)
  | DoExec(instruc) :: q, stack -> Ok(q, Executable instruc :: stack)
  | Exec ::q, Executable instruc :: stackTail -> Ok(instruc@q,stackTail)
  | Get :: q, Int n :: stackTail ->
    let rec getNth i = function
      |Int t::q1 -> if i = 0 then t else getNth (i-1) q1 
      |Executable _::q1 -> 
        if i = 0 then -1
        else getNth (i-1) q1
      |[] -> -2
    in
    let nth =  getNth n (Int n::stackTail) in
    if nth = -1 then Error("TypeException error: Wrong type for Get", (q, []))
    else if nth = -2 then Error("Runtime error: Not enough elements for Get", (q, []))
    else
      Ok(q, Int nth :: stackTail)

  (* Invalid configurations *)
  | _, _ -> Error ("Invalid configuration", state)

let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], Int v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds, List.map(fun x -> Int x) args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
