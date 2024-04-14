open Ast
open FunPfx.Ast


(*let generate = function
  | Const _ -> failwith "To implement"
  | Binop(_,_,_) -> failwith "To implement"
  | Uminus _ -> failwith "To implement"
  | Var _ -> failwith "Not yet supported"*)



(*let rec generate (e : Ast.expression) : BasicPfx.Ast.command list = 
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
  | Var _ -> failwith "Not yet supported"*)


let generate env dpth =
  let binop_to_Pfx = function
    | BinOp.Badd -> Add
    | BinOp.Bsub -> Sub
    | BinOp.Bmul -> Mul
    | BinOp.Bdiv -> Div
    | BinOp.Bmod -> Rem in

  let rec gen_expr env dpth = function
    | Const n -> [Push n], dpth + 1
    | Binop (op, e1, e2) ->
        let seq2, dpth2 = gen_expr env dpth e2 in
        let seq1, dpth1 = gen_expr env dpth2 e1 in
        seq2 @ seq1 @ [binop_to_Pfx op], dpth1 - 1
    | Uminus e ->
        let seq, dpth2 = gen_expr env dpth e in
        seq @ [Push 0; Sub], dpth2
    | App (e1, e2) ->
        let seq2, dpth2 = gen_expr env dpth e2 in
        let seq1, dpth1 = gen_expr env dpth2 e1 in
        seq2 @ seq1 @ [Exec; Swap; Pop], dpth1 - 1
    | Fun (x, e) ->
        let seq, _ = gen_expr ((x, dpth) :: env) (dpth + 1) e in
        [DoExec seq], dpth
    | Var v ->
        let pos = List.assoc v env in
        [Push (dpth - pos); Get], dpth + 1 in

  gen_expr env dpth
