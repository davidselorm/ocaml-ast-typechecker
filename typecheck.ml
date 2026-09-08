(* Hindley-Milner Type Inference Engine in OCaml *)

type typ =
  | TInt
  | TBool
  | TArrow of typ * typ
  | TVar of string

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Lam of string * expr
  | App of expr * expr

type env = (string * typ) list

let rec typecheck (gamma : env) (e : expr) : typ =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x ->
      (try List.assoc x gamma
       with Not_found -> failwith ("Unbound identifier: " ^ x))
  | Lam (x, body) ->
      let tvar = TVar ("'a") in
      let body_ty = typecheck ((x, tvar) :: gamma) body in
      TArrow (tvar, body_ty)
  | App (e1, e2) ->
      let ty1 = typecheck gamma e1 in
      let ty2 = typecheck gamma e2 in
      match ty1 with
      | TArrow (arg, ret) when arg = ty2 -> ret
      | _ -> failwith "Type mismatch in application"
