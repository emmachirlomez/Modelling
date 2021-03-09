module Eval

open CalculatorTypesAST

// This module is able to evaluate a AST objects.

let rec parseA:(statementA -> Map<string, int> -> Result<int, string>) = fun stm mp ->
    match stm with
    | Number x -> Ok x
    | Variable var ->
        if mp.ContainsKey var then
            Ok (mp.Item var)
        else
            Error ("Variable " + var + " does not exist!")
    | Sum (a, b) ->
        match (parseA a mp, parseA b mp) with
        | (Ok s1, Ok s2) -> Ok (s1 + s2)
        | (Error f1, _) -> Error f1
        | (_, Error f2) -> Error f2
    | Diff (a, b) ->
        match (parseA a mp, parseA b mp) with
        | (Ok s1, Ok s2) -> Ok (s1 - s2)
        | (Error f1, _) -> Error f1
        | (_, Error f2) -> Error f2
    | Mul (a, b) ->
        match (parseA a mp, parseA b mp) with
        | (Ok s1, Ok s2) -> Ok (s1 * s2)
        | (Error f1, _) -> Error f1
        | (_, Error f2) -> Error f2
    | Div (a, b) ->
        match (parseA a mp, parseA b mp) with
        | (Ok s1, Ok 0) -> Error "Division by 0!"
        | (Ok s1, Ok s2) -> Ok (s1 / s2)
        | (Error f1, _) -> Error f1
        | (_, Error f2) -> Error f2
    | Neg a ->
        match parseA a mp with
        | Ok s -> Ok (-s)
        | oth -> oth
    | BrackA s -> parseA s mp


and parseB:(statementB -> Map<string, int> -> Result<bool, string>) = fun stm mp ->
    match stm with
    | True -> Ok true
    | False -> Ok false
    | EagerAnd (s1, s2) ->
        match (parseB s1 mp, parseB s2 mp) with
        | (Ok true, Ok true) -> Ok true
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
        | _ -> Ok false
    | EagerOr (s1, s2) ->
        match (parseB s1 mp, parseB s2 mp) with
        | (Ok true, Ok _) -> Ok true
        | (Ok _, Ok true) -> Ok true
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
        | _ -> Ok false
    | ShortAnd (s1, s2) ->
        match parseB s1 mp with
        | Ok true -> parseB s2 mp
        | Error s -> Error s
        | Ok false -> Ok false
    | ShortOr (s1, s2) ->
        match parseB s1 mp with
        | Ok true -> Ok true
        | Error s -> Error s
        | Ok false -> parseB s2 mp
    | Negation s ->
        match parseB s mp with
        | Ok true -> Ok false
        | Ok false -> Ok true
        | x -> x
    | Equality (s1, s2) ->
        match (parseA s1 mp, parseA s2 mp) with
        | (Ok a, Ok b) -> Ok (a = b)
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | Inequality (s1, s2) ->
        match (parseA s1 mp, parseA s2 mp) with
        | (Ok a, Ok b) -> Ok (not (a = b))
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | Greater (s1, s2) ->
        match (parseA s1 mp, parseA s2 mp) with
        | (Ok a, Ok b) -> Ok (a > b)
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | GreaterOrEqual (s1, s2) ->
        match (parseA s1 mp, parseA s2 mp) with
        | (Ok a, Ok b) -> Ok (a >= b)
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | Less (s1, s2) ->
        match (parseA s1 mp, parseA s2 mp) with
        | (Ok a, Ok b) -> Ok (a < b)
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | LessOrEqual (s1, s2) ->
        match (parseA s1 mp, parseA s2 mp) with
        | (Ok a, Ok b) -> Ok (a <= b)
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | BrackB s -> parseB s mp


and parseC:(statementC -> Map<string, int> -> Result<Map<string, int>, string>) = fun stm mp ->
    match stm with
    | Skip -> Ok mp
    | Assign (var, v) ->
        match parseA v mp with
        | Error s -> Error s
        | Ok result -> Ok (mp.Add(var, result))
    | Commandline (s1, s2) ->
        match parseC s1 mp with
        | Ok new_mp -> parseC s2 new_mp
        | fail -> fail
    | IfStat gc -> parseGC gc mp
    | DoStat gc ->
        match conditionTreeIsTrue gc mp with
        | Ok true ->
            match parseGC gc mp with
            | Ok new_mp -> parseC (DoStat gc) new_mp
            | Error err -> Error err
        | Ok false -> Ok mp
        | Error err -> Error err

and conditionTreeIsTrue:(statementGC -> Map<string, int> -> Result<bool, string>) = fun stm mp ->
    match stm with
    | FunctionStat (sb, sc) -> parseB sb mp
    | NextStat (s1, s2) ->
        match conditionTreeIsTrue s1 mp with
        | Ok true -> Ok true
        | Ok false -> conditionTreeIsTrue s2 mp
        | Error err -> Error err

and parseGC:(statementGC -> Map<string, int> -> Result<Map<string, int>, string>) = fun stm mp ->
    match stm with
    | FunctionStat (sb, sc) ->
        match parseB sb mp with
        | Ok true -> parseC sc mp
        | Ok false -> Ok mp
        | Error err -> Error err
    | NextStat (s1, s2) ->
        match conditionTreeIsTrue s1 mp with
        | Ok true -> parseGC s1 mp
        | Ok false -> parseGC s2 mp
        | Error err -> Error err;;


// Sample

// a = 1
// while (a < 10)
//    a = a + 1;

let stm =
    Commandline (Assign ("a", Number 1),
                 DoStat (FunctionStat (Less (Variable "a", (Number 10)),
                                    Assign ("a", Sum (Variable "a", Number 1)))));;



