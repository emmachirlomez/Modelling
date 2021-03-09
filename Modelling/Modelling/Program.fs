// This script implements our interactive calculator
module mainstuff

// We need to import a couple of modules, including the generated lexer and parser
open FSharp.Text.Lexing
open System
open CalculatorTypesAST
open CalculatorParser
open CalculatorLexer
open Eval


// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = CalculatorParser.start CalculatorLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an arithmetic expression: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "Result: TBD"
        compute n
        with err -> compute (n-1)

// Start interacting with the user
// compute 3

let sample_program = "a := 23; if 3 < 2 -> a := 5 fi";;

[<EntryPoint>]
let main argv =
    printf "%A" <| parseC (parse sample_program) (Map.empty)
    0