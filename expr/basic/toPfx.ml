open Ast

let rec generate = function
  | Const c -> [CommandConst c]  (* Représenter la constante en tant que commande spéciale *)
  | Var v -> failwith "Not yet supported"
  | Binop(op, e1, e2) ->
      let generated_e1 = generate e1 in
      let generated_e2 = generate e2 in
      generated_e1 @ generated_e2 @ [CommandBinop op]  (* Représenter l'opération binaire en tant que commande spéciale *)
  | Uminus e -> generate e @ [CommandUminus]