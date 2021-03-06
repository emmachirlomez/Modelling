// Signature file for parser generated by fsyacc
module CalculatorParser
type token = 
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | POW
  | LSBRACK
  | RSBRACK
  | LPAR
  | RPAR
  | TRUE
  | EOF
  | INTDIV
  | FALSE
  | EAGERAND
  | EAGEROR
  | SHORTAND
  | SHORTOR
  | NEG
  | EQUAL
  | GEQUAL
  | GREATER
  | INEQUAL
  | LEQUAL
  | LESSER
  | ASSIGN
  | SKIP
  | CLINE
  | IF
  | FI
  | DO
  | OD
  | NSTAT
  | FUNC
  | VARNAME of (string)
  | NUM of (int)
type tokenId = 
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_POW
    | TOKEN_LSBRACK
    | TOKEN_RSBRACK
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_TRUE
    | TOKEN_EOF
    | TOKEN_INTDIV
    | TOKEN_FALSE
    | TOKEN_EAGERAND
    | TOKEN_EAGEROR
    | TOKEN_SHORTAND
    | TOKEN_SHORTOR
    | TOKEN_NEG
    | TOKEN_EQUAL
    | TOKEN_GEQUAL
    | TOKEN_GREATER
    | TOKEN_INEQUAL
    | TOKEN_LEQUAL
    | TOKEN_LESSER
    | TOKEN_ASSIGN
    | TOKEN_SKIP
    | TOKEN_CLINE
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_NSTAT
    | TOKEN_FUNC
    | TOKEN_VARNAME
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_expressionA2
    | NONTERM_expressionA
    | NONTERM_expressionB
    | NONTERM_expressionC
    | NONTERM_expressionGC
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (statementC) 
