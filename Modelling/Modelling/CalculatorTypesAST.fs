// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module CalculatorTypesAST

type expr =
  | Num of float
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | PowExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
  | IntDiv of (expr * expr) 


type statementA =
    | Number of int
    | Variable of string
    | Array of (string * statementA) 
    | Sum of (statementA * statementA)
    | Diff of (statementA * statementA)
    | Mul of (statementA * statementA)
    | Div of (statementA * statementA)
    | Neg of statementA
    | Pow of (statementA * statementA)
    | BrackA of statementA;;

type statementB = 
    | True
    | False
    | EagerAnd of (statementB * statementB)
    | EagerOr of (statementB * statementB)
    | ShortAnd of (statementB * statementB)
    | ShortOr of (statementB * statementB)
    | Negation of statementB
    | Equality of (statementA * statementA)
    | Inequality of (statementA * statementA)
    | Greater of (statementA * statementA)
    | GreaterOrEqual of (statementA * statementA)
    | Less of (statementA * statementA)
    | LessOrEqual of (statementA * statementA)
    | BrackB of statementB;;

type statementC =
    | Assign of (string * statementA)
    | AssignArray of (string * statementA * statementA)
    | Skip
    | Commandline of (statementC * statementC)
    | IfStat of statementGC
    | DoStat of statementGC
and statementGC = 
    | FunctionStat of (statementB * statementC)
    | NextStat of (statementGC * statementGC);;