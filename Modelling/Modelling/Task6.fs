module Task6

open System
open ProgramGraph
open Eval
open Task3


type Memory = Map<string, int>

type State = int * Memory


let rec readInitialVariablesMemory : (int ->  Memory -> Memory) = fun count mp ->
    match count with
    | 0 -> mp
    | _ ->
        let rest_of_vars = readInitialVariablesMemory (count - 1) mp
        printf "%s" ("What is the name of the " + count.ToString() + "th variable?\n $ ")
        let varname = Console.ReadLine()
        printf "%s" ("What is the value of '" + varname + "'?\n $ ")
        let value = int(Console.ReadLine())
        rest_of_vars.Add(varname, value);;

let rec readInitialArraysMemory : (int ->  Memory -> Memory) = fun count mp ->
    match count with
    | 0 -> mp
    | _ ->
        let rest_of_vars = readInitialArraysMemory (count - 1) mp
        printf "%s" ("What is the name of the #" + count.ToString() + " array?\n $ ")
        let varname = Console.ReadLine()
        printf "%s" ("What is the content of '" + varname + "' (please enter the values with spaces between them)\n $ ")
        let values = List.map int (List.ofArray(Console.ReadLine().Split(' ')))

        let addOneValue: ((Memory * int) -> int -> (Memory * int)) = fun (mp, id) value ->
            (mp.Add(varname + "$" + id.ToString(), value), id + 1)

        let (final_map, _) = List.fold addOneValue (rest_of_vars, 0) values

        final_map;;

let rec readInitialMemory : (unit -> Memory) = fun () ->
    printf "%s" ("How many variables do you want to initialise?\n $ ")
    let number_var = int(Console.ReadLine())
    let m1 = readInitialVariablesMemory number_var Map.empty
    
    printf "%s" ("How many arrays do you want to initialise?\n $ ")
    let number_array = int(Console.ReadLine())
    let m2 = readInitialArraysMemory number_array m1
    m2;;

// returns all the states you can get from (node, memory)
let getStatesForAState : (Edge list -> int -> Memory -> Set<State>) = fun edges node memory ->
    match node with 
    | -2 -> Set.ofList[(-2, memory)]
    | _ ->
        let keep_edge : (Edge -> bool) = fun edge ->
            match edge with
            | AssignEdge (x, _, _) -> x = node
            | CheckerEdge (x, _, _) -> x = node
        let my_edges = List.filter keep_edge edges
        let mutable reach1 = Set.empty 
        for edge in my_edges do 
            match edge with 
            | AssignEdge (_, stmc, ending) ->
                match parseC stmc memory with
                | Ok(new_memory) -> reach1 <- reach1.Add((ending, new_memory))
                | Error(s) -> ()
            | CheckerEdge (_, stmb, ending) ->
                match parseB stmb memory with
                | Ok(true) ->  reach1 <- reach1.Add((ending, memory))
                | Ok(false) -> ()
                | Error(s) -> ()
        reach1

let rec GetStucksStates : (Edge list -> unit) = fun edges ->
    let memory = readInitialMemory()
    let mutable visited = Set.empty
    let mutable tovisit = Set.empty.Add((-1, memory))
    let mutable stuckstates = Set.empty

    while tovisit <> Set.empty do
        let s = tovisit.MaximumElement
        tovisit <- tovisit.Remove(s)
        if visited.Contains(s) then 
            ()
        else
            visited <- visited.Add(s)
            let reachableStatesFromS = getStatesForAState edges (fst s) (snd s)
            if reachableStatesFromS = Set.empty then
                let smessage = ((fst s), (snd s),"Stuck")
                stuckstates <- stuckstates.Add(smessage)
            else if (fst s) = -2 then
                let smessage = ((fst s), (snd s),"Terminated")
                stuckstates <- stuckstates.Add(smessage)
            else
                for state in reachableStatesFromS do
                    tovisit <- tovisit.Add(state)

    
    
    let rec printMem :( List<(string * int)> -> string) = fun mem ->
        match (mem) with
        | [] -> ""
        | (k, v)::t -> (variable_prettifier k) + ": " + v.ToString() + "\n" +  printMem t

    let qName : (int -> string) = fun n ->
           match n with 
           | -1 -> "qInitial"
           | -2 -> "qFinal"
           | x  -> "q" + x.ToString()

    printf "%s" "\nConfiguration:  \n"
    for item in stuckstates do 
        match item with  
        | (final_node, final_memory, message) -> printf "%s" ("\nStatus: " + message + "\nNode: " + qName(final_node) +  "\n\nFinal memory \n" + printMem(Map.toList(final_memory)))
