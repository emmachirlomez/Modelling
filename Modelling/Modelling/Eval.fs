module Eval

open CalculatorTypesAST

// This module is able to evaluate a AST objects.



type Memory = Map<string, string>
type Flows = Set<string * string>

let variable_prettifier : (string -> string) = fun var ->
    match List.ofArray(var.Split('$')) with 
    | [x] -> x
    | [x; y] -> x + "[" + y + "]";;

let rec parseA:(statementA -> Map<string, int> -> Result<int, string>) = fun stm mp ->
    match stm with
    | Number x -> Ok x
    | Variable var ->
        if mp.ContainsKey var then
            Ok (mp.Item var)
        else
            Error ("Variable " + (variable_prettifier var) + " does not exist!")
    | Array (s, a) -> 
        match parseA a mp with
        | Ok index -> 
            let new_name = s + "$" + index.ToString()
            if mp.ContainsKey new_name then
                Ok (mp.Item new_name)
            else
                Error ("Element " + s + "[" + index.ToString() + "] does not exist!")
        | Error r -> Error r
    | Pow (a, b) ->
        match (parseA a mp, parseA b mp) with
        | (Ok s1, Ok s2) -> Ok (pown s1 s2)
        | (Error f1, _) -> Error f1
        | (_, Error f2) -> Error f2 
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


and parseC:(statementC -> Map<string, int> -> Flows -> (Result<Map<string, int>, string> * Flows )) = fun stm mp flows ->
    match stm with
    | Skip -> (Ok mp, flows) 
    | Assign (var, v) -> // TODO: check if var exists
        match parseA v mp with
        | Error s -> (Error s, flows) 
        | Ok result -> 
            if mp.ContainsKey(var) then
                flows.Add(var, mp.)
                (Ok (mp.Add(var, result)), flows) 
            else
                (Error ("Variable " + (variable_prettifier var) + " does not exist!"), flows) 
            
    | AssignArray (s, a1, a2) ->
        match (parseA a1 mp, parseA a2 mp) with
        | (Error r, _) -> (Error r, flows) 
        | (_, Error r) -> (Error r, flows) 
        | (Ok index, Ok new_val) -> 
            let new_var = s + "$" + index.ToString()
            if mp.ContainsKey(new_var) then
                (Ok (mp.Add(new_var, new_val)), flows) 
            else
                (Error ("Variable " + (variable_prettifier new_var) + " does not exist!"), flows) 
            
    | Commandline (s1, s2) ->
        match parseC s1 mp flows with
        | (Ok new_mp, flows)  -> parseC s2 new_mp flows
        | fail -> fail
    | IfStat gc -> parseGC gc mp flows
    | DoStat gc ->
        match conditionTreeIsTrue gc mp with
        | Ok true ->
            match parseGC gc mp flows with
            | (Ok new_mp, flows)  -> parseC (DoStat gc) new_mp flows
            | (Error err, flows)  -> (Error err, flows) 
        | Ok false -> (Ok mp, flows) 
        | Error err -> (Error err, flows) 

and conditionTreeIsTrue:(statementGC -> Map<string, int> -> Result<bool, string>) = fun stm mp ->
    match stm with
    | GC [] -> Ok false
    | GC ((b, c):: t) ->
        match parseB b mp with
        | Ok true -> Ok true
        | Ok false -> conditionTreeIsTrue (GC t) mp
        | Error s -> Error s

and parseGC:(statementGC -> Map<string, int> -> Flows -> (Result<Map<string, int>, string>) * Flows) = fun stm mp flows ->
    match stm with
    | GC [] -> (Ok mp, flows) 
    | GC ((b, c)::t) ->
        match parseB b mp with
        | Ok true -> parseC c mp flows
        | Ok false -> parseGC (GC t) mp flows
        | Error s -> (Error s, flows) ;;

let Eval (x:statementC) = parseC x Map.empty;;

