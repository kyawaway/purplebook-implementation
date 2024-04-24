(** Ast **)

type ast =
  | Bool of bool
  | Int of int
  | Add of ast * ast
  | Lt of ast * ast
  | If of ast * ast * ast

type ty = TyBool | TyInt

(* util *)
let rec string_of_ast = function
  | Bool x -> string_of_bool x
  | Int x -> string_of_int x
  | Add (x, y) -> string_of_ast x ^ " + " ^ string_of_ast y
  | Lt (x, y) -> string_of_ast x ^ " < " ^ string_of_ast y
  | If (x, y, z) ->
      "if " ^ string_of_ast x ^ " then " ^ string_of_ast y ^ " else "
      ^ string_of_ast z
