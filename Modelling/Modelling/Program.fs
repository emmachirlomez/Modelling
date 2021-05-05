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
open ProgramGraph
open Task3
open Task4
open SignEval


let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = CalculatorParser.start CalculatorLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let evaluateProgram e =    
    try 
        match Eval e with
            | Ok ans -> printfn "After execution, memory is: %A" ans
            | Error ans -> printfn "Failed to evaluate: %A" ans
    with 
        err -> printfn "Unable to evaluate code :("


// We implement here the function that interacts with the user
let readGCLProgram() =
    printf "Enter your GCL program:\n"
    try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "\nRaw parsed program:"
        printfn "%A" <| e
        printfn "\nPrettyfied parsed program:"
        printfn "%A" <| Print e
        printf "Please enter the filename you want to save the .gv file into: "
        let filename = Console.ReadLine()
        printf "Specify Security Lattice, separated by a comma"
        let securityLattice = Console.ReadLine().Split(",")
        let list_of_edges = GetProgramGraph true e
        InterpretPG list_of_edges 
        printf ""
        printf "%s" <| "Done!\nGenerated:\n" + (GVGenerator (true) e filename)
        evaluateProgram e
    with
        err -> printfn "Unable to parse program, check your syntax!!!";;


// We implement here the function that interacts with the user
let evalGCLProgram() =
    printf "Enter your GCL program:\n"
    try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "\nRaw parsed program:"
        printfn "%A" <| e
        printfn "\nPrettyfied parsed program:"
        printfn "%A" <| Print e
        printfn "%A" <| InterpretPG
        printf "Do you want a deterministic automaton (Y/N)? "
        let response = Console.ReadLine()
        let (list_of_edges, nr_nodes) = GetProgramGraph (response.[0] = 'Y') e
        InterpretPG list_of_edges

    with
        err -> printfn "Unable to parse program, check your syntax!!!";;


let InputVariables() =
    printf "How many variab"
    

let PrintSignAnalysis() =
    printf "Enter your GCL program:\n"
    try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "\nRaw parsed program:"
        printfn "%A" <| e
        printfn "\nPrettyfied parsed program:"
        printfn "%A" <| Print e
        printf "Do you want a deterministic automaton (Y/N)? "
        let response = Console.ReadLine()

        let (list_of_edges, nr_nodes) = GetProgramGraph (true) e
        AnalyseSign list_of_edges nr_nodes

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
    // printf "%A" <| EvaluateSignOfFunction (Set.empty.Add Plus) ((Set.empty.Add Minus).Add Zero) (/) 
    PrintSignAnalysis()
    0;;