module Task3

open System
open ProgramGraph
open Eval

let rec readInitialVariablesMemory : (int ->  Map<string, int> -> Map<string, int>) = fun count mp ->
    match count with
    | 0 -> mp
    | _ ->
        let rest_of_vars = readInitialVariablesMemory (count - 1) mp
        printf "%s" ("What is the name of the " + count.ToString() + "th variable?\n $ ")
        let varname = Console.ReadLine()
        printf "%s" ("What is the value of '" + varname + "'?\n $ ")
        let value = int(Console.ReadLine())
        rest_of_vars.Add(varname, value);;

let rec readInitialArraysMemory : (int ->  Map<string, int> -> Map<string, int>) = fun count mp ->
    match count with
    | 0 -> mp
    | _ ->
        let rest_of_vars = readInitialArraysMemory (count - 1) mp
        printf "%s" ("What is the name of the #" + count.ToString() + " array?\n $ ")
        let varname = Console.ReadLine()
        printf "%s" ("What is the content of '" + varname + "' (please enter the values without spaces between them)?\n $ ")
        let values = List.map int (List.ofArray(Console.ReadLine().Split(' ')))

        let addOneValue: ((Map <string, int> * int) -> int -> (Map <string, int> * int)) = fun (mp, id) value ->
            (mp.Add(varname + "$" + id.ToString(), value), id + 1)

        let (final_map, _) = List.fold addOneValue (rest_of_vars, 0) values

        final_map;;

let rec readInitialMemory : (unit -> Map<string, int>) = fun () ->
    printf "%s" ("How many variable do you want to initialise?\n $ ")
    let number_var = int(Console.ReadLine())
    let m1 = readInitialVariablesMemory number_var Map.empty
    
    printf "%s" ("How many arrays do you want to initialise?\n $ ")
    let number_array = int(Console.ReadLine())
    let m2 = readInitialArraysMemory number_array m1
    m2;;

let rec evaluatePG : (Edge list -> int -> int -> Map<string, int> -> (Map<string, int> * int * string)) = fun edges node count memory ->
    match (node, count) with 
    | (-2, _) -> (memory, -2, "Finished correctly")
    | (_, 0) -> (memory, node, "Stopped at node " + node.ToString() + " due to exceded number of steps")
    | _ ->
        let keep_edge : (Edge -> bool) = fun edge ->
            match edge with
            | AssignEdge (x, _, _) -> x = node
            | CheckerEdge (x, _, _) -> x = node
        let my_edges = List.filter keep_edge edges

        let rec choose_edge edg =
            match edg with
            | [] -> (memory, node, "Got stuck")
            | AssignEdge (_, stmc, ending)::tail ->
                match parseC stmc memory with
                | Ok(new_memory) -> evaluatePG edges ending (count - 1) new_memory
                | Error(s) -> (memory, node, "Error while executing: " + s)
            | CheckerEdge (_, stmb, ending)::tail ->
                match parseB stmb memory with
                | Ok(true) -> evaluatePG edges ending (count - 1) memory
                | Ok(false) -> choose_edge tail
                | Error(s) -> (memory, node, "Error while executing: " + s)
        choose_edge my_edges

let rec InterpretPG : (Edge list -> unit) = fun edges ->
    printf "%s" ("How many operations before considering it an infifinte loop?\n $ ")
    let max_steps = int(Console.ReadLine())
    let memory = readInitialMemory()

    let (final_memory, final_node, message) = evaluatePG edges -1 max_steps memory

    printf "%s" ("Ending node: " + final_node.ToString() + "\nFinish status: " + message + "\nFinal memory: " + final_memory.ToString())
    