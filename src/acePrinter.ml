
open Lang

let strTyp = LangUtils.strTyp

let fpr = Printf.fprintf

type hover_annotation = string

type tooltip = int * int * hover_annotation

type row = int

type col = int

type highlight_range = (row * col) * (row * col)

type printing_tree =
  | Leaf of string * hover_annotation
  | Inner of printing_tree list * hover_annotation
  | Tab of int
  | Newline
  | HighlightError of printing_tree

let leaf  ?(ann="") s = Leaf  (s, ann)
let inner ?(ann="") l = Inner (l, ann)

let semi ann = inner [leaf " "; leaf ~ann ";"] (* space before ";" *)

let sepTrees : printing_tree -> printing_tree list -> printing_tree =
fun sep trees ->
  let rec foo = function
    | []    -> []
    | t::[] -> [t]
    | t::ts -> t :: sep :: foo ts
  in
  inner (foo trees)

let treeCommas = sepTrees (leaf " , ") (* space before "," *)

let treeCommasAndNewlines k = sepTrees (inner [leaf " ,"; Newline; Tab k])

(* ImpScript type variable syntax conflicts with JavaScript string literals.
   TODO for compatibility with the HTML files, could change ImpScript to parse
   backquote and turn it into single quote. *)
let replacePrimes s =
  Str.global_replace (Str.regexp "'") "`" s

let rec containsNewline = function
  | Newline -> true
  | Leaf _ | Tab _ -> false
  | HighlightError tr -> containsNewline tr
  | Inner (l, _) -> List.exists containsNewline l

let stripParens s =
  s |> Utils.explode
    |> List.tl |> List.rev |> List.tl |> List.rev
    |> Utils.implode

let strBaseVal = function
  | Int i  -> spr "%d" i
  | Bool b -> spr "%b" b
  | Str s  -> spr "\"%s\"" s
  | Unit   -> "()"
  | Nil    -> "[]"

let strSolvedUnsolved t1 t2 =
  spr "%s\n\nsynthesized during constraint generation:\n%s"
    (strTyp t1) (strTyp t2)

let rec treeOfPattern : pattern -> printing_tree =
function
  | PVar pvar ->
      let ann = strSolvedUnsolved pvar.pvarSolved pvar.pvarUnsolved in
      leaf ~ann pvar.pvar
  | PTuple ps ->
      inner [leaf "( "; treeCommas (List.map treeOfPattern ps); leaf " )"]

let rec walkExp : int -> exp -> printing_tree =
fun k exp -> match exp.exp with
  | EConst bv ->
      let ann = strSolvedUnsolved exp.tSolved exp.tUnsolved in
      leaf ~ann (strBaseVal bv)
  | EVar x ->
      let ann = strSolvedUnsolved exp.tSolved exp.tUnsolved in
      leaf ~ann x
  | EFun (p, e) ->
      inner [
        leaf "fun "; treeOfPattern p; leaf " ->"; Newline;
        Tab (succ k); walkExp (succ k) e
      ]
  | EApp ({exp = EApp ({exp = EVar op}, e1)}, e2)
    when List.mem op LangUtils.infixOps ->
      inner [
        leaf "("; walkExp k e1;
        leaf " "; leaf (stripParens op); leaf " ";
        walkExp k e2; leaf ")"
      ]
  | EApp (e1, e2) ->
      inner [leaf "("; walkExp k e1; leaf " "; walkExp k e2; leaf ")"]
  | EIf (e1, e2, e3) -> leaf "IF"
  | ETuple es ->
      let tree = treeCommas (List.map (walkExp k) es) in
      inner [
        leaf "("; tree; leaf ")"
      ]
  | ELet (p, e1, e2) -> walkLet false k p e1 e2
  | ELetRec (p, e1, e2) -> walkLet true k p e1 e2

and walkLet isRec k p e1 e2 =
  let let_ = if isRec then "let rec " else "let " in
  inner [
    leaf let_; treeOfPattern p; leaf " = "; walkExp k e1; leaf " in ";
    Newline; Tab k; walkExp k e2
  ]

let tab k = String.make (2 * k) ' '

let rec walkTree
  : int -> int -> printing_tree
 -> (string * int * int * tooltip list * highlight_range list) =
fun row col -> function

  | Newline ->
      ("\n", row + 1, 1, [], [])

  | Tab k ->
      let s = tab k in
      (s, row, col + String.length s, [], [])

  | Leaf (s, ann) ->
      let tips = if ann = "" then [] else [(row, col, ann)] in
      (s, row, col + String.length s, tips, [])

  | Inner (l, ann) ->
      let tips = if ann = "" then [] else [(row, col, ann)] in
      List.fold_left (fun (acc,row,col,tips,ranges) tree ->
        let (s,row,col,tips',ranges') = walkTree row col tree in
        (acc ^ s, row, col, tips @ tips', ranges @ ranges')
      ) ("", row, col, tips, []) l

  | HighlightError tr ->
      let (s,row',col',tips,ranges) = walkTree row col tr in
      let highlightRange = ((row-1, col-1), (row'-1, col'-1)) in
      (s, row', col', tips, highlightRange :: ranges)

let printFileAndTooltips oc tree =
  (* 
     - break up s into single-line string literal chunks
     - using single quotes when writing JavaScript to HTML page,
       and double quotes for ImpScript string literals
  *)
  let (s,_,_,tips,ranges) = walkTree 1 1 tree in
  let s = replacePrimes s in
  let s = Str.global_replace (Str.regexp "\n") "',\n  '" s in

  fpr oc "var arraySrc = [\n";
  fpr oc "  '%s'\n" s;
  fpr oc "];\n";
  fpr oc "\n";
  fpr oc "editor.setValue(arraySrc.join('\\n'));\n";
  fpr oc "editor.gotoLine(0);\n";
  fpr oc "\n";
  fpr oc "clearAnnotations();\n";
  fpr oc "\n";

  List.iter (fun (i,j,s) ->
    let s = replacePrimes s in
    let l = Str.split (Str.regexp "\n") s in
    if List.length l <= 1 then
      fpr oc "addAnnot(%d, %d, '%s');\n\n" i j s
    else begin
      fpr oc "addAnnot(%d, %d, [\n" i j;
      List.iter (fun s -> fpr oc "  '%s',\n" s) l;
      fpr oc "].join('\\n'));\n\n";
    end
  ) tips;

  (* instead, could track using data constructors for hover_annotation
     List.iter
       (fun (i,j,s) -> fpr oc "addAnnot(%4d, %3d, '%s');\n" i j s) tips; *)

  List.iter (fun ((r1,c1), (r2,c2)) ->
    let sRange = spr "new Range(%d, %d, %d, %d)" r1 c1 r2 c2 in
    fpr oc "editor.session.addMarker(%s, 'ace_step', 'error');\n\n" sRange
  ) ranges;

  ()

let replace sMatch sReplace s =
  Str.global_replace (Str.regexp sMatch) sReplace s

let print exp fHtml =

  let ic = open_in (Settings.ace_dir ^ "example.html") in
  let oc = open_out fHtml in
  let acePath = "../../ace-output/" in
        (* this works only for files tests/blah1/blah2/some-test.js
           TODO compute from fHtml *)

  let placeholder = ".*<script src=\"display-file.js\"></script>.*" in
  let rec foo () =
    try
      let s = input_line ic in
      if Str.string_match (Str.regexp placeholder) s 0 then begin
        let tree = walkExp 0 exp in
        fpr oc "<script>\n\n";
        printFileAndTooltips oc tree;
        fpr oc "</script>\n";
        foo ();
      end else begin
        let s = replace "src=\"js" (spr "src=\"%sjs" acePath) s in
        let s = replace "href=\"css" (spr "href=\"%scss" acePath) s in
        fpr oc "%s\n" s;
        foo ();
      end
    with End_of_file -> ()
  in
  foo ();

  Log.log1 "Wrote to %s.\n" fHtml;
  fpr (open_out Settings.out_html_filename) "%s\n" fHtml;
  ()

