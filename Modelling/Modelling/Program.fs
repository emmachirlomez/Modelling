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
open Task5
open Task6
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
        printf "Do you want a deterministic automaton (Y/N)? "
        let response = Console.ReadLine()
        printf "%s" <| "Done!\nGenerated:\n" + (GVGenerator (response.[0] = 'Y' || response.[0] = 'y') e filename)
        evaluateProgram e
    with
        err -> printfn "Unable to parse program, check your syntax!!!";;


//  We implement here the function that interacts with the user
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
        printf "Do you want a deterministic automaton (Y/N)? "
        let response = Console.ReadLine()
        let (list_of_edges, nr_nodes) = GetProgramGraph (response.[0] = 'Y') e
        InterpretPG list_of_edges

    with
        err -> printfn "Unable to parse program, check your syntax!!!";;


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
        let (list_of_edges, nr_nodes) = GetProgramGraph (response.[0] = 'Y') e
        AnalyseSign list_of_edges nr_nodes

    with
        err -> printfn "Unable to parse program, check your syntax!!!";;

let PrintSecurityAnalysis() =
    printf "Enter your GCL program:\n"
    try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "\nRaw parsed program:"
        printfn "%A" <| e
        printfn "\nPrettyfied parsed program:"
        printfn "%A" <| Print e
        printSecurity (get_flows_and_violations e)

    with
        err -> printfn "Unable to parse program, check your syntax!!!";;

// Start interacting with the user
// compute 3

let PrintStates() =
    printf "Enter your GCL program:\n"
    try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "\nRaw parsed program:"
        printfn "%A" <| e
        printfn "\nPrettyfied parsed program:"
        printfn "%A" <| Print e
        let (list_of_edges, nr_nodes) = GetProgramGraph false e
        GetStucksStates list_of_edges

    with
        err -> printfn "Unable to parse program, check your syntax!!!";;


[<EntryPoint>]
let main argv =
    // For task 1 + 2: readGCLProgram()
    // For task 3: evalGCLProgram()
    // For task 4: PrintSignAnalysis()
    // For task 5: PrintSecurityAnalysis()
    // For task 6: 
    PrintStates()
    0;;