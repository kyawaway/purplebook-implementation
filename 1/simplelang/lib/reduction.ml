open Ast

exception Nomilization_error

let rec reduction : ast -> ast option = function
  | Bool _ | Int _ -> None
  | Add (Int n1, Int n2) -> Some (Int (n1 + n2))
  | Add (p1, p2) -> (
      match reduction p1 with
      | Some p1' -> Some (Add (p1', p2))
      | None -> (
          match reduction p2 with
          | Some p2' -> Some (Add (p1, p2'))
          | None -> None))
  | Lt (Int n1, Int n2) -> Some (Bool (n1 < n2))
  | Lt (p1, p2) -> (
      match reduction p1 with
      | Some p1' -> Some (Lt (p1', p2))
      | None -> (
          match reduction p2 with
          | Some p2' -> Some (Lt (p1, p2'))
          | None -> None))
  | If (Bool true, p1, _) -> Some p1
  | If (Bool false, _, p2) -> Some p2
  | If (p, p1, p2) -> (
      match reduction p with Some p' -> Some (If (p', p1, p2)) | None -> None)

let normalize (e : ast) : ast =
  let rec normalize' (e : ast) : ast =
    match reduction e with Some e' -> normalize' e' | None -> e
  in
  normalize' e
