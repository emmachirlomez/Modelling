// This script implements our interactive calculator
module mainstuff

// We need to import a couple of modules, including the generated lexer and parser
open FSharp.Text.Lexing
open System
open CalculatorTypesAST
open CalculatorParser
open CalculatorLexer
open PrettyPrinter
open Eval


let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = CalculatorParser.start CalculatorLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let test =
    printf "Enter your GCL program:\n"
    try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "\nParsed program:"
        printfn "%s" <| Print e
        try 
            match Eval e with
                | Ok ans -> printfn "After execution, memory is: %A" ans
                | Error ans -> printfn "Failed to evaluate: %A" ans
        with 
            err -> printfn "Unable to evaluate code :("
    with
        err -> printfn "Unable to parse program, check your syntax!!!";;

// Start interacting with the user
// compute 3

let sample_program =
        "a := 23;
         if 3 < 2 ->
            a := 5 * a;
            if 5 < 3 ->
                a := 6;
                b := 7
            [] 6 > 7 ->
                c := 3 - 4 + 5
            fi
        fi";;

[<EntryPoint>]
let main argv =
    // printf "%A" <| Print (parse sample_program)
    test 
    0