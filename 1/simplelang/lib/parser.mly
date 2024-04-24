%{
  open Ast 
%}

%token <int> INT

%token PLUS LT LPAREN RPAREN TRUE FALSE IF THEN ELSE 

%token EOL

%nonassoc ELSE
%left LT
%left PLUS 

%start parse 
%type <ast> parse 

%%

parse:
  expr EOL { $1 }
  | error
  { failwith
      (Printf.sprintf "parse error near characters %d-%d"
         (Parsing.symbol_start ())
         (Parsing.symbol_end ()))
  }
;

expr:
  | INT { Int($1) }
  | TRUE { Bool(true) }
  | FALSE { Bool(false) }
  | exl=expr PLUS exr=expr { Add(exl, exr) }
  | LPAREN exl=expr PLUS exr=expr RPAREN { Add(exl, exr) }
  | exl=expr LT exr=expr { Lt(exl, exr) }
  | IF ex=expr THEN exthen=expr ELSE exelse=expr { If(ex, exthen, exelse) }
  ;
