
open Lexing

let pr = Printf.printf
let spr = Printf.sprintf

let strPrefix s pre =
  let n = String.length s in
  let m = String.length pre in
    (n >= m) && (String.sub s 0 m = pre)


(***** Parsing ****************************************************************)

let token_list_of_string s =
  let lb = Lexing.from_string s in
  let rec helper l = 
  try 
    let t = LangLexer.token lb in
    if t = LangParser.EOF then List.rev l else helper (t::l)
  with _ -> List.rev l
  in
    helper []

let filename_to_exp f = 
  LangParser.program LangLexer.token (Lexing.from_channel (open_in f))

let string_to_exp s =
  LangParser.program LangLexer.token (Lexing.from_string s)

let string_of_position (p, e) = 
  Format.sprintf "%s:%d:%d-%d" p.pos_fname p.pos_lnum
    (p.pos_cnum - p.pos_bol) (e.pos_cnum - e.pos_bol)

(* rkc: handling position info similar to Lambdajs.parse_lambdajs and
   Lambdajs_env.parse_env *)
let doParse start_production name =
  let lexbuf = Lexing.from_channel (open_in name) in
  let strPos () = string_of_position (lexbuf.lex_curr_p, lexbuf.lex_curr_p) in
    try begin
      (* Set the correct filename in lexbuf (for source-tracking). *)
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                               pos_fname = name; pos_lnum = 1 };
      start_production LangLexer.token lexbuf
    end with
      | Failure "lexing: empty token" ->
          Log.printParseErr (spr "lexical error\n\nat %s" (strPos ()))
      | Failure s when strPrefix s "Lex: bad char" ->
          Log.printParseErr (spr "%s\n\nat %s" s (strPos ()))
      | Failure s when strPrefix s "parse error" ->
          Log.printParseErr s
      | Failure s when strPrefix s "lexical error" ->
          Log.printParseErr s
      | Lang.LangParseError(s) ->
          Log.printParseErr (spr "at %s\n\n%s" (strPos ()) s)
      | Parsing.Parse_error _  (* thrown when using ocamlyacc *)
      | LangParser.Error ->    (* thrown when using menhir *)
          Log.printParseErr
            (spr "unexpected token [%s]\n\nat %s" (lexeme lexbuf) (strPos ()))

let _ =
  match List.tl (Array.to_list Sys.argv) with
    | []      -> ()
    | _::_::_ -> failwith "too many command-line args"
    | [f]     -> begin
        let e = doParse LangParser.program f in
        let t = Typing.tc e in
        pr "- : %s\n" (LangUtils.strTyp t);
        ()
      end

