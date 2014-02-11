{
  open Lang
  open LangParser
}

let letter   = ['A'-'Z''a'-'z']
let digit    = ['0'-'9']
let white    = [' ' '\t' '\r']
let newline  = ['\n']

rule token = parse

  | eof         { EOF }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "let"       { LET }
  | "rec"       { REC }
  | "=="        { EQEQ }
  | "="         { EQ }
  | "in"        { IN }
  | "fun"       { FUN }
  | "->"        { ARROW }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { MUL }
  | "/"         { DIV }
  | "<"         { LT }
  | "<="        { LE }
  | "!="        { NE }
  | "&&"        { AND }
  | "||"        { OR }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "["         { LBRACK }
  | "]"         { RBRACK }
  | ";"         { SEMI }
  | ","         { COMMA }
  | "::"        { DCOLON }

  | digit+ as i                   { Num (int_of_string i) }
  | letter (letter|digit)* as s   { Id s }
  | "_"                           { Id "_" }

  | white       { token lexbuf }
  | newline     { Lexing.new_line lexbuf; token lexbuf }

  | "(*"        { comments 0 lexbuf }

  | _ { raise (Lang.LangParseError
                 ("Illegal Character '" ^ Lexing.lexeme lexbuf ^ "'")) }

and comments level = parse
  | eof		  { Printf.printf "comments are not closed\n"; raise End_of_file }
  | "*)"    { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | "(*"    { comments (level+1) lexbuf }
  | newline { Lexing.new_line lexbuf; comments level lexbuf }
  | _       { comments level lexbuf }

