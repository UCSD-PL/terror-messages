{
  open Lang
  open LangParser
}

let letter   = ['A'-'Z''a'-'z']
let digit    = ['0'-'9']
let white    = [' ' '\t' '\r']
let newline  = ['\n']

let str =
  (letter
    | digit
    | [' ' '+' '-' '*' '/' '=' '(' ')' '&' '|' '.' ',' '{' '}' ':' ';' '#'])*

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
  | "()"        { UNIT }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "["         { LBRACK }
  | "]"         { RBRACK }
  | ";"         { SEMI }
  | ","         { COMMA }
  | "::"        { DCOLON }

  | digit+ as i                   { INT (int_of_string i) }
  | letter (letter|digit)* as s   { ID s }
  | "_"                           { ID "_" }
  | '"' (str as s) '"'            { STR s}

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

