(* Function that generate a Pfx program from an Expr program *)
(*val generate : Ast.expression -> BasicPfx.Ast.command list*)

(*adapted Function signature for exercice 10*)
val generate : (string * int) list -> int -> Ast.expression -> FunPfx.Ast.command list * int

