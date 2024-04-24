open Ast

exception Type_error of ast

let rec infer = function
  | Bool _ -> TyBool
  | Int _ -> TyInt
  | Add (p1, p2) ->
      check p1 TyInt;
      check p2 TyInt;
      TyInt
  | Lt (p1, p2) ->
      check p1 TyInt;
      check p2 TyInt;
      TyBool
  | If (p, p1, p2) ->
      check p TyBool;
      let t = infer p1 in
      check p2 t;
      t

and check p t = if infer p <> t then raise (Type_error p)
