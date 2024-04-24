open Ast
open Parser
open Infer
open Reduction

let repl () =
  print_string "\n********** Welcome to my Interpreter! **********\n";
  while true do
    print_string "> ";
    flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    try
      let exp = parse Lexer.main lexbuf in
      let _ = infer exp in
      let result = normalize exp in
      print_string ("# " ^ string_of_ast result);
      print_newline ()
    with
    | Parsing.Parse_error ->
        print_string "Error: Parse Error";
        print_newline ()
    | Type_error p ->
        print_string ("Error: Type Error in " ^ "'" ^ string_of_ast p ^ "'");
        print_newline ()
    | e ->
        (* どう考えてもおかしいので，気が向いたら直す *)
        let pos = Lexing.lexeme_start_p lexbuf in
        let col = pos.pos_cnum - pos.pos_bol in
        print_string
          ("Error: " ^ Printexc.to_string e ^ " at line "
         ^ string_of_int pos.pos_lnum ^ " and column " ^ string_of_int col);
        print_newline ()
  done
