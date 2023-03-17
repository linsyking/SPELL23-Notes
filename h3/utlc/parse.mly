%parameter<Parsing : Syntax.Parsing.PARSING>

%start <Parsing.t Syntax.cmd list> main

%{


module VarMap = Core.String.Map

let macros = ref VarMap.empty

let add_macro (mvar, e) = 
  (macros := VarMap.set (!macros) ~key:mvar ~data:e)

let expand mvar =
  VarMap.find_exn (!macros) mvar

%}

%%

main : 
 | cmds EOF                      { Core.List.filter_opt $1 }

cmds : 
 | cmd SEMI                      { [$1]    }
 | cmd SEMI cmds                 { $1::$3  }

cmd : 
 | DEF MVAR EQU elet             { add_macro ($2, $4); None      }
 | EVAL elet                     { Some (Syntax.Eval $2)         }
 | PRINT elet                    { Some (Syntax.Print $2)        }
 | TEST elet EQU elet            { Some (Syntax.Test ($2, $4))   }

(* let x = e in e' is directly elaborated to lambda application in parsing *)
elet:
 | LET VAR EQU elam IN elet      { Parsing.app (Parsing.lam ($2, $6), $4) }
 | elam                          { $1 }

(* Lambda terms *)
elam:
 | LAM VAR DOT elam              { Parsing.lam ($2, $4) }
 | eapp                          { $1 }

eapp:
 | eapp atomic                   { Parsing.app ($1, $2) }
 | atomic                        { $1 }

atomic:
 | LPAR elam RPAR                { $2 }
 | MVAR                          { expand $1      }
 | VAR                           { Parsing.var $1 }