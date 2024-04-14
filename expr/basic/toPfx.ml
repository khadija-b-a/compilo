open Ast

(*let generate = function
  | Const _ -> failwith "To implement"
  | Binop(_,_,_) -> failwith "To implement"
  | Uminus _ -> failwith "To implement"
  | Var _ -> failwith "Not yet supported"*)



let rec generate (e : Ast.expression) : BasicPfx.Ast.command list = 
  match e with 
  | Const n -> [Push n]
  | Binop(op, e1, e2) -> 
      generate e2 @
      generate e1 @
      (match op with 
        | BinOp.Badd -> [Add]
        | BinOp.Bsub -> [Sub]
        | BinOp.Bmul -> [Mul]
        | BinOp.Bdiv -> [Div]
        | BinOp.Bmod -> [Rem]
      )
  | Uminus e -> generate e @ [Push 0; Sub]  (* Utilisation de la soustraction à partir de zéro pour simuler la négation *)
  | Var _ -> failwith "Not yet supported"

