open Typecheck

let rec unify (t1: typ) (t2: typ) : (string * typ) list =
  match (t1, t2) with
  | (TInt, TInt) | (TBool, TBool) -> []
  | (TVar a, t) | (t, TVar a) ->
      if t = TVar a then []
      else if occurs a t then failwith "Occurs check failed"
      else [(a, t)]
  | _ -> failwith "Type mismatch"
