module Eval

open CGLTypesAST
open VariableInit

// This module is able to evaluate a AST objects.


let rec parseA:(statementA -> Map<string, int> -> Map<string,int list> -> Result<int, string>) = fun stm vmp amp ->
    match stm with
    | Number x -> Ok x
    | Variable var ->
        if vmp.ContainsKey var then
            Ok (vmp.Item var)
        else
            Error ("Variable " + var + " does not exist!")
    | Array (s, a) -> 
        match parseA a vmp amp with
        | Ok index -> 
            let new_name = s
            if amp.ContainsKey new_name then //FIXME
                Ok (amp.Add(new_name, )
            else
                Error ("Element " + s + "[" + index.ToString() + "] does not exist!")
        | Error r -> Error r
    | Pow (a, b) ->
        match (parseA a vmp amp, parseA b vmp amp) with
        | (Ok s1, Ok s2) -> Ok (pown s1 s2)
        | (Error f1, _) -> Error f1
        | (_, Error f2) -> Error f2 
    | Sum (a, b) ->
        match (parseA a vmp amp, parseA b vmp amp) with
        | (Ok s1, Ok s2) -> Ok (s1 + s2)
        | (Error f1, _) -> Error f1
        | (_, Error f2) -> Error f2
    | Diff (a, b) ->
        match (parseA a vmp amp, parseA b vmp amp) with
        | (Ok s1, Ok s2) -> Ok (s1 - s2)
        | (Error f1, _) -> Error f1
        | (_, Error f2) -> Error f2
    | Mul (a, b) ->
        match (parseA a vmp amp, parseA b vmp amp) with
        | (Ok s1, Ok s2) -> Ok (s1 * s2)
        | (Error f1, _) -> Error f1
        | (_, Error f2) -> Error f2
    | Div (a, b) ->
        match (parseA a vmp amp, parseA b vmp amp) with
        | (Ok s1, Ok 0) -> Error "Division by 0!"
        | (Ok s1, Ok s2) -> Ok (s1 / s2)
        | (Error f1, _) -> Error f1
        | (_, Error f2) -> Error f2
    | Neg a ->
        match parseA a vmp amp with
        | Ok s -> Ok (-s)
        | oth -> oth


and parseB:(statementB -> Map<string, int> -> Map<string, int list> -> Result<bool, string>) = fun stm vmp amp ->
    match stm with
    | True -> Ok true
    | False -> Ok false
    | EagerAnd (s1, s2) ->
        match (parseB s1 vmp amp, parseB s2 vmp amp) with
        | (Ok true, Ok true) -> Ok true
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
        | _ -> Ok false
    | EagerOr (s1, s2) ->
        match (parseB s1 vmp amp, parseB s2 vmp amp) with
        | (Ok true, Ok _) -> Ok true
        | (Ok _, Ok true) -> Ok true
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
        | _ -> Ok false
    | ShortAnd (s1, s2) ->
        match parseB s1 vmp amp with
        | Ok true -> parseB s2 vmp amp
        | Error s -> Error s
        | Ok false -> Ok false
    | ShortOr (s1, s2) ->
        match parseB s1 vmp amp with
        | Ok true -> Ok true
        | Error s -> Error s
        | Ok false -> parseB s2 vmp amp
    | Negation s ->
        match parseB s vmp amp with
        | Ok true -> Ok false
        | Ok false -> Ok true
        | x -> x
    | Equality (s1, s2) ->
        match (parseA s1 vmp amp, parseA s2 vmp amp) with
        | (Ok a, Ok b) -> Ok (a = b)
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | Inequality (s1, s2) ->
        match (parseA s1 vmp amp, parseA s2 vmp amp) with
        | (Ok a, Ok b) -> Ok (not (a = b))
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | Greater (s1, s2) ->
        match (parseA s1 vmp amp, parseA s2 vmp amp) with
        | (Ok a, Ok b) -> Ok (a > b)
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | GreaterOrEqual (s1, s2) ->
        match (parseA s1 vmp amp, parseA s2 vmp amp) with
        | (Ok a, Ok b) -> Ok (a >= b)
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | Less (s1, s2) ->
        match (parseA s1 vmp amp, parseA s2 vmp amp) with
        | (Ok a, Ok b) -> Ok (a < b)
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s
    | LessOrEqual (s1, s2) ->
        match (parseA s1 vmp amp, parseA s2 vmp amp) with
        | (Ok a, Ok b) -> Ok (a <= b)
        | (Error s, _) -> Error s
        | (_, Error s) -> Error s


and parseC:(statementC -> Map<string, int> -> Map<string, int list> -> Result<Map<string, int>, string>) = fun stm vmp amp ->
    match stm with
    | Skip -> Ok vmp
    | Assign (var, Number(v)) ->
        match parseA var vmp amp with
        | Error s -> Error s
        | Ok result -> Ok (vmp.Add("v", v))
    | AssignArray (a1, a2) ->
        match (parseA a1 vmp amp, parseA a2 vmp amp) with
        | (Error r, _) -> Error r
        | (_, Error r) -> Error r
        | (Ok index, Ok new_val) -> 
            let new_var = "$" + index.ToString()
            Ok (vmp.Add(new_var, new_val))
    | Commandline (s1, s2) ->
        match parseC s1 vmp amp with
        | Ok new_mp -> parseC s2 new_mp amp
        | fail -> fail
    | IfStat gc -> parseGC gc vmp amp
    | DoStat gc ->
        match conditionTreeIsTrue gc vmp amp with
        | Ok true ->
            match parseGC gc vmp amp with
            | Ok new_mp -> parseC (DoStat gc) new_mp amp
            | Error err -> Error err
        | Ok false -> Ok vmp
        | Error err -> Error err

and conditionTreeIsTrue:(statementGC -> Map<string, int> -> Map<string, int list> -> Result<bool, string>) = fun stm vmp amp ->
    match stm with
    | GC [] -> Ok false
    | GC ((b, c):: t) ->
        match parseB b vmp amp with
        | Ok true -> Ok true
        | Ok false -> conditionTreeIsTrue (GC t) vmp amp
        | Error s -> Error s

and parseGC:(statementGC -> Map<string, int> -> Map<string, int list> -> Result<Map<string, int>, string>) = fun stm vmp amp ->
    match stm with
    | GC [] -> Ok vmp
    | GC ((b, c)::t) ->
        match parseB b vmp amp with
        | Ok true -> parseC c vmp amp
        | Ok false -> parseGC (GC t) vmp amp
        | Error s -> Error s;;

let Eval (x:statementC) = parseC x Variables Arrays;;

