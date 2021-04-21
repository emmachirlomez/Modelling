module Task4

open System
open ProgramGraph
open Eval
open SignEval

// For every node we store all the possible sign for all variables
type GlobalSignState = Map<int, Set<Memory>>

let strToSign : (string -> Sign) = fun c ->
    match c with
    | "+" -> Plus
    | "-" -> Minus
    | "0" -> Zero
    | _ -> failwith "Invalid token"

let strToMemSign : (string -> MemSign) = fun c ->
    let addSign : (MemSign -> char -> MemSign) = fun signs chr ->
        match chr with
        | '+' -> signs.Add Plus
        | '0' -> signs.Add Zero
        | '-' -> signs.Add Minus
        | _ -> signs
    List.fold addSign Set.empty (Seq.toList c);;


let rec readInitialVarSigns : (int ->  VarMemory -> VarMemory) = fun count mp ->
    match count with
    | 0 -> mp
    | _ ->
        let rest_of_vars = readInitialVarSigns (count - 1) mp
        printf "%s" ("What is the name of the " + count.ToString() + "th variable?\n $ ")
        let varname = Console.ReadLine()
        printf "%s" ("What is the value of '" + varname + "' (+, 0 or -)?\n $ ")
        let value = Console.ReadLine()
        rest_of_vars.Add(varname, (strToSign value));;


let rec readInitialArrsSigns : (int ->  ArrMemory -> ArrMemory) = fun count mp ->
    match count with
    | 0 -> mp
    | _ ->
        let rest_of_vars = readInitialArrsSigns (count - 1) mp
        printf "%s" ("What is the name of the #" + count.ToString() + " array?\n $ ")
        let varname = Console.ReadLine()
        printf "%s" ("What are the signs of '" + varname + "' (please enter all of the possible values from '+', '-' and '0')\n $ ")
        let values = Console.ReadLine()

        rest_of_vars.Add(varname, strToMemSign values);;


let rec readInitialSigns : (int -> Set<Memory>) = fun count ->
    match count with 
    | 0 -> Set.empty
    | _ ->
        let rest_of_mems = readInitialSigns (count - 1)
        printf "%s" ("Memory #" + count.ToString() + ":")
        printf "%s" ("How many variables do you want to initialise?\n $ ")
        let number_var = int(Console.ReadLine())
        let m1 = readInitialVarSigns number_var Map.empty
    
        printf "%s" ("How many arrays do you want to initialise?\n $ ")
        let number_array = int(Console.ReadLine())
        let m2 = readInitialArrsSigns number_array Map.empty
        // printf "%A" m2
        rest_of_mems.Add(m1, m2);;

let rec readAllMems : (unit -> Set<Memory>) = fun () ->
    printf "%s" ("How many memories do you want to initialise?\n $ ")
    let number_var = int(Console.ReadLine())
    readInitialSigns number_var;;

// Going through all the edge ONCE and expand the memory
let ExpandEdgesSign : (Edge List -> GlobalSignState -> GlobalSignState) = fun edges memory ->

    // Artificial joining 2 Sets
    let MergeSets : (Set<Memory> -> Set<Memory> -> Set<Memory>) = fun s1 s2 ->
        Set.fold (fun (s:Set<Memory>) element -> s.Add(element)) s1 s2

    // This function is used for the fold 
    // Considering that edge, it updates the set of memory signs
    // for the destination node of the edge from the start node
    let updateMemory : (GlobalSignState -> Edge -> GlobalSignState) = fun mem edge ->
        match edge with
        | AssignEdge (from, c, dest) -> 
            let fromMem = mem.Item(from)
            let destMem = mem.Item(dest)
            let modifiedfromMem = passSignMemThroughStatementC fromMem c
            let finalDestMem = MergeSets destMem modifiedfromMem
            mem.Add(dest, finalDestMem)
        | CheckerEdge (from, b, dest) -> 
            let fromMem = mem.Item(from)
            let destMem = mem.Item(dest)
            let modifiedfromMem = passSignMemThroughStatementB fromMem b
            let finalDestMem = MergeSets destMem modifiedfromMem
            mem.Add(dest, finalDestMem)
    List.fold updateMemory memory edges;;

// This function takes the program graph and nr nodes
// and prints the sign for all varibales at any given node
let rec AnalyseSign : (Edge List -> int -> unit) = fun edges nr_nodes ->
    let startMem = List.fold (fun (mem:GlobalSignState) node -> mem.Add(node, Set.empty)) Map.empty ([1..(nr_nodes - 2)]@[-2])
    let globalMem = startMem.Add(-1, readAllMems())

    // globalMem stores the initial memory(for each node all possible signs)
    // and we will try to expand possible memmory by trying all edges

    // Calls ExpandEdgesSign. 
    // If it returned memory is different then call recursivly itself
    // Else, returns same memory
    let rec FindFinalGlobalMemState : (GlobalSignState -> GlobalSignState) = fun memory ->
        let returnedMem = ExpandEdgesSign edges memory
        if returnedMem = memory then
            memory
        else
            FindFinalGlobalMemState returnedMem

    // Getting final memory
    let finalMemory = FindFinalGlobalMemState globalMem
    printf "%A" "The final memory is: "
    printfn "%A" finalMemory;;
        

    




