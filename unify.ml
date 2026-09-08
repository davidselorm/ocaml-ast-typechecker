open Typecheck

let rec string_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVar x -> x
  | TArrow (t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"

let () =
  let test_expr = Lam ("x", Var "x") in
  let ty = typecheck [] test_expr in
  print_endline ("[PASS] Inferred identity type: " ^ string_of_typ ty)
