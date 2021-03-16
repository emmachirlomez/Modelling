module PrettyPrinter

open CalculatorTypesAST

let spacing = "  ";

let rec prettyPrinterA:(statementA -> string) = fun x ->
    match x with 
        | Number(n) -> n.ToString()
        | Variable(v) -> v
        | Array(s, a) -> s + " [" + (prettyPrinterA a) + "]"
        | Sum(a1, a2) -> (prettyPrinterA a1) + " + " + (prettyPrinterA a2)
        | Diff(a1, a2) -> (prettyPrinterA a1) + " - " + (prettyPrinterA a2)
        | Div(a1, a2) -> (prettyPrinterA a1) + " / " + (prettyPrinterA a2)
        | Pow(a1, a2) -> (prettyPrinterA a1) + "^" + (prettyPrinterA a2)
        | Neg(a) -> "-" + (prettyPrinterA a)
        | Mul (a1, a2) -> (prettyPrinterA a1) + " * " + (prettyPrinterA a2);;

let rec prettyPrinterB:(statementB -> string) = fun x ->
    match x with
        | True -> "true"
        | False -> "false"
        | EagerAnd(b1, b2) -> (prettyPrinterB b1) + " & " + (prettyPrinterB b2)
        | EagerOr(b1, b2) -> (prettyPrinterB b1) + " | " + (prettyPrinterB b2)
        | ShortAnd(b1, b2) -> (prettyPrinterB b1) + " && " + (prettyPrinterB b2)
        | ShortOr(b1, b2) -> (prettyPrinterB b1) + " || " + (prettyPrinterB b2)
        | Negation(b) -> "!" + (prettyPrinterB b)
        | Equality(a1, a2) -> (prettyPrinterA a1) + " = " + (prettyPrinterA a2)
        | Inequality(a1, a2) -> (prettyPrinterA a1) + " != " + (prettyPrinterA a2)
        | Greater(a1, a2) -> (prettyPrinterA a1) + " > " + (prettyPrinterA a2)
        | GreaterOrEqual(a1, a2) -> (prettyPrinterA a1) + " >= " + (prettyPrinterA a2)
        | Less(a1, a2) -> (prettyPrinterA a1) + " < " + (prettyPrinterA a2)
        | LessOrEqual(a1, a2) -> (prettyPrinterA a1) + " <= " + (prettyPrinterA a2);; 


let rec prettyPrinterC:(statementC -> string List) = fun x ->
    match x with
        | Assign(s, a) -> [s + " := " + (prettyPrinterA a)]
        | AssignArray(s, a1, a2) -> [s + "[" + (prettyPrinterA a1) + "]" + " := " + (prettyPrinterA a2)]
        | Skip -> ["skip"]
        | Commandline(c1, c2) -> 
            let c1_list = prettyPrinterC c1
            let c1_reversed = List.rev c1_list
            let real_c1_rev = (c1_reversed.Head + ";") :: c1_reversed.Tail
            let ans = List.rev real_c1_rev
            ans @ (prettyPrinterC c2)
        | IfStat(gc) -> ["if"] @ List.map (fun s -> spacing + s) (prettyPrinterGC gc) @ ["fi"]
        | DoStat(gc) -> ["do"] @ List.map (fun s -> spacing + s) (prettyPrinterGC gc) @ ["do"]

and prettyPrinterGC:(statementGC -> string List) = fun x ->
    match x with 
        | FunctionStat(b, c) -> [(prettyPrinterB b) + " -> "] @ (List.map (fun s -> spacing + s) (prettyPrinterC c))
        | NextStat(gc1, gc2) ->
            let ans = prettyPrinterGC gc2
            (prettyPrinterGC gc1) @ (("[] " + ans.Head)::ans.Tail);;

let rec Print x = 
    let newLine = List.map (fun s -> s + "\n") (prettyPrinterC x) 
    List.fold (+) "" newLine;;

