let var  = ['a'-'z' 'A'-'Z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
let mvar = '$' ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*


let ws = [' ' '\t' '\r' '\011' '\012'] 


rule initial =
  parse
    ws+           { initial lexbuf }

  | eof           { Tokens.EOF     }
  | ';'           { Tokens.SEMI    }

  | '\n'          { (Lexing.new_line lexbuf; initial lexbuf) }


  | '('           { Tokens.LPAR  }
  | ')'           { Tokens.RPAR  }
  

  | "def"         { Tokens.DEF   }
  | "print"       { Tokens.PRINT }
  | "eval"        { Tokens.EVAL  }
  | "test"        { Tokens.TEST  }

  | "let"         { Tokens.LET   }
  | "in"          { Tokens.IN    }
  | '='           { Tokens.EQU   }

  | '\\'          { Tokens.LAM   }
  | "."           { Tokens.DOT   }

  | var  as v      { Tokens.VAR  v }
  | mvar as v      { Tokens.MVAR v }
