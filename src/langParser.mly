%{
open Lang
open LangUtils
%}

%token <int> Num
%token <string> Id
%token TRUE FALSE EOF LET REC EQ IN FUN ARROW IF THEN ELSE
%token PLUS MINUS MUL DIV LT LE NE AND OR EQEQ
%token LPAREN RPAREN LBRACK RBRACK
%token SEMI COMMA DCOLON

%start program
%type <Lang.exp> program

%%

program : e=exp EOF { e }

exp : e=exp8        { e }

exp8 :

| FUN p=pattern ps=list(pattern) ARROW e=exp
    { match ps with
        | [] -> eFun ~pos:($startpos,$endpos) p e
        | _  -> eFunCurried ~pos:($startpos,$endpos) (p::ps) e }

| LET p=pattern ps=list(pattern) EQ e1=exp IN e2=exp
    { match ps with
        | [] -> eLet ~pos:($startpos,$endpos) p e1 e2
        | _  -> eLet ~pos:($startpos,$endpos) p (eFunCurried ps e1) e2 }

| LET REC p=pattern ps=list(pattern) EQ e1=exp IN e2=exp
    { match ps with
        | [] -> eLetRec ~pos:($startpos,$endpos) p e1 e2
        | _  -> eLetRec ~pos:($startpos,$endpos) p (eFunCurried ps e1) e2 }

| IF e1=exp THEN e2=exp ELSE e3=exp  { eIf ~pos:($startpos,$endpos) e1 e2 e3}

| e=exp7                     { e }

exp7 :
| e1=exp7 OR e2=exp6         { eOr ~pos:($startpos,$endpos) e1 e2 }
| e=exp6                     { e }

exp6 :
| e1=exp6 AND e2=exp5        { eAnd ~pos:($startpos,$endpos) e1 e2 }
| e=exp5                     { e }

exp5 :
| e1=exp5 EQEQ e2=exp54      { eEqEq ~pos:($startpos,$endpos) e1 e2 } 
| e1=exp5 EQ e2=exp54        { eEq ~pos:($startpos,$endpos) e1 e2 } 
| e1=exp5 NE e2=exp54        { eNe ~pos:($startpos,$endpos) e1 e2 } 
| e1=exp5 LT e2=exp54        { eLt ~pos:($startpos,$endpos) e1 e2 } 
| e1=exp5 LE e2=exp54        { eLe ~pos:($startpos,$endpos) e1 e2 } 
| e=exp54                    { e }

exp54 :
| e1=exp4 DCOLON e2=exp54    { eCons ~pos:($startpos,$endpos) e1 e2 }
| e=exp4                     { e }

exp4 :
| e1=exp4 PLUS e2=exp3       { ePlus ~pos:($startpos,$endpos) e1 e2 }
| e1=exp4 MINUS e2=exp3      { eMinus ~pos:($startpos,$endpos) e1 e2 }
| e=exp3                     { e }

exp3 :
| e1=exp3 MUL e2=exp2        { eMul ~pos:($startpos,$endpos) e1 e2 }
| e1=exp3 DIV e2=exp2        { eDiv ~pos:($startpos,$endpos) e1 e2 }
| e=exp2                     { e }

exp2 :
| e1=exp2 e2=exp1            { eApp ~pos:($startpos,$endpos) e1 e2 }
| e=exp1                     { e }

exp1 :
| i=Num                      { eInt ~pos:($startpos,$endpos) i }
| TRUE                       { eBool ~pos:($startpos,$endpos) true }
| FALSE                      { eBool ~pos:($startpos,$endpos) false }
| x=Id                       { eVar ~pos:($startpos,$endpos) x }
| LPAREN e=exp RPAREN        { e }

| LBRACK es=separated_list(SEMI,exp) RBRACK  { eList ~pos:($startpos,$endpos) es }

pattern :
| x=Id                                            { PVar x }
| LPAREN ps=separated_list(COMMA,pattern) RPAREN  { PTuple ps }

