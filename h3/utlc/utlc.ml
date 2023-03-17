
module MakeLang (S        : Syntax.SYNTAX) 
                (Parsing  : Syntax.Parsing.PARSING with type t = S.t)
                (Dynamics : Language.DYNAMICS with type t = S.t) =
struct
  module S        = S
  module Parser   = Parse.Make(Parsing)

  let parse lexbuf = Parser.main Lex.initial lexbuf

  let exec_step f e = 
    let rec loop i e = 
      if i < 10000 then
        match f e with 
        | None    -> e
        | Some e' -> loop (i + 1) e'
      else (
        print_endline "Evaluation ended after 10000 steps:";
        e
      )
    in loop 0 e

  let top eval_strat cmds = 
    let eval e = 
      match eval_strat with 
      | "norm" -> 
        Dynamics.norm e
      | "step" -> 
        exec_step Dynamics.step e
      | _ -> failwith "unreachable"
    in 
    Core.List.iter cmds ~f:(function
      | Syntax.Eval e -> 
        print_endline @@ "Evaluating " ^ S.to_string e;
        print_endline @@ S.to_string (eval e)
      | Syntax.Print e -> 
        print_endline @@ S.to_string e
      | Syntax.Test (e1, e2) -> 
        print_endline @@ "Testing e1 = " ^ S.to_string e1;
        print_endline @@ "        e2 = " ^  S.to_string e2;
        print_endline @@ "        e1 = e2 : " ^ Bool.to_string (S.aequiv(eval e1) (eval e2))
    )
end

module LangAst = MakeLang (Syntax.Ast) (Syntax.Parsing.Ast) (Language.Dynamics.Ast)
module LangAbt = MakeLang (Syntax.Abt) (Syntax.Parsing.Abt) (Language.Dynamics.Abt)

(* Argument handling *)
let eval_strat = ref "norm"
let representation = ref "ast"

let _ =
  let speclist = 
    [
      ("-eval", Arg.Symbol (["norm"; "step"], fun s -> eval_strat := s), "Evaluation strategy.");
      ("-rep" , Arg.Symbol (["ast"; "abt"], fun s -> representation := s), "Representation of syntax tree.");
    ]
  in
  let usage_msg = "utlc" in
  let args_fun _ = () in
  Arg.parse speclist args_fun usage_msg

let lexbuf = Lexing.from_channel ~with_positions:true stdin 

let _ = 
  Printf.printf "Representation = '%s'.\n" !representation;
  match !representation with 
  | "ast" -> LangAst.top (!eval_strat) (LangAst.parse lexbuf)
  | "abt" -> LangAbt.top (!eval_strat) (LangAbt.parse lexbuf)
  | _ -> failwith "Unreachable"