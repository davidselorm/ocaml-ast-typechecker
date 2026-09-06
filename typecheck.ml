type typ = TVar of string | TInt | TBool | TArrow of typ * typ

let rec occurs (v: string) (t: typ) : bool =
  match t with
  | TVar x -> x = v
  | TInt | TBool -> false
  | TArrow (t1, t2) -> occurs v t1 || occurs v t2
